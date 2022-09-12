import { NextAuthOptions } from "next-auth";
import { sequelize } from "./db";
import SequelizeAdapter from "./next-auth-sequelize";
import EmailProvider from "next-auth/providers/email";

const adapter = SequelizeAdapter(sequelize);

const authOptions: NextAuthOptions = {
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
  callbacks: {
    jwt: async ({ user, token }) => {
      if (user) {
        token.uid = user.id;
      }
      return token;
    },
  },
  session: {
    strategy: 'jwt',
  },
};

export default authOptions;
