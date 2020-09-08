create or replace function register(name text, email text, password text, cookie boolean DEFAULT false) returns json as $$
declare
    usr record;
begin
    insert into data."user" as u
    (name, email, password) values ($1, $2, $3)
    returning *
    into usr;

    return login(usr.email, password, cookie);
end
$$ security definer language plpgsql;

revoke all privileges on function register(text, text, text, boolean) from public;
