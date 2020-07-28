-- BASE CASE
-- Token should be saved to sessionStorage
-- Refresh-token should be saved in cookie which only server can setup.

create or replace function login(email text, password text, cookie boolean DEFAULT false, csrf text DEFAULT null ) returns json as $$
declare
    usr record;
    ses record;
    token text;
    head text;
begin
    select * from data."user" as u
    where u.email = $1 and u.password = public.crypt($2, u.password)
    INTO usr;

    if usr is NULL then
        raise invalid_password using detail = 'Invalid email or password', hint = 'Make sure your email and password are correct!';
    else
        head := request.header('host') || '-' ||  request.header('user-agent');
        token := pgjwt.sign(
                    json_build_object(
                         'role', usr.role,
                         'user_id', usr.id,
                         'exp', extract(epoch from now())::integer + settings.get('jwt_lifetime')::int
                    ),
                    settings.get('jwt_secret')
                );

        if csrf is not null then
           delete from data."session" where csrf=csrf; -- remove old refresh token for this user with device
        end if;

        -- TODO add more info, like IP address, Location to logs, and to session.
        insert into data."session" as s
        (user_id, device_name, csrf, exp) values (usr.id, head, pgjwt.url_encode(convert_to(replace(uuid_generate_v4()::text, '-', ''), 'utf8')), extract(epoch from now())::integer + settings.get('refresh_token_lifetime')::int)
        returning *
        into ses;

        if cookie is true then
           perform response.set_cookie('REFRESHTOKEN', ses.id::text, settings.get('refresh_token_lifetime')::int,'/'::text);
        end if;

        return json_build_object(
               'id', usr.id,
               'name', usr.name,
               'email', usr.email,
               'role', usr.role::text,
               'token', token::text,
               'refresh_token', ses.id::text,
               'csrf', ses.csrf::text
               );
    end if;
end
$$ volatile security definer language plpgsql;
-- by default all functions are accessible to the public, we need to remove that and define our specific access rules
revoke all privileges on function login(text, text, boolean, text) from public;
