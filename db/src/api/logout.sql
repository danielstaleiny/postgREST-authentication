
create or replace function logout(refresh_token text default null) returns text as $$
begin
    if refresh_token is not null then
       delete from data."session" where id=refresh_token::uuid;
    end if;
    perform response.delete_cookie('REFRESHTOKEN');
    return json_build_object('ok', true);

end
$$ security definer language plpgsql;

revoke all privileges on function logout(text) from public;
