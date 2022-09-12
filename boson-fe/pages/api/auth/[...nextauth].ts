import { sequelize } from "../../../service/db"
import SequelizeAdapter from "../../../service/next-auth-sequelize"
import NextAuth from "next-auth"
import EmailProvider from "next-auth/providers/email";

const adapter = SequelizeAdapter(sequelize)

export default NextAuth({
  adapter,
  providers: [
    EmailProvider({
      server: {
        host: process.env.EMAIL_SERVER_HOST,
        port: process.env.EMAIL_SERVER_PORT,
        auth: {
          user: process.env.EMAIL_SERVER_USER,
          pass: process.env.EMAIL_SERVER_PASSWORD
        }
      },
      from: process.env.EMAIL_FROM,
    }),
  ],
})
