import NextAuth from "next-auth"
import authOptions from "../../../service/auth_options";

export default NextAuth(authOptions);
