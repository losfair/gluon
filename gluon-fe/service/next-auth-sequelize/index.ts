import type { Account as AdapterAccount } from "next-auth"
import type {
  Adapter,
  AdapterUser,
  AdapterSession,
  VerificationToken,
} from "next-auth/adapters"
import { Sequelize, Model, ModelCtor } from "sequelize"
import { directTxn, retryableTxn } from "../db"
import * as defaultModels from "./models"

export { defaultModels as models }

// @see https://sequelize.org/master/manual/typescript.html
interface AccountInstance
  extends Model<AdapterAccount, Partial<AdapterAccount>>,
  AdapterAccount { }
interface UserInstance
  extends Model<AdapterUser, Partial<AdapterUser>>,
  AdapterUser { }
interface SessionInstance
  extends Model<AdapterSession, Partial<AdapterSession>>,
  AdapterSession { }
interface VerificationTokenInstance
  extends Model<VerificationToken, Partial<VerificationToken>>,
  VerificationToken { }

interface SequelizeAdapterOptions {
  models?: Partial<{
    User: ModelCtor<UserInstance>
    Account: ModelCtor<AccountInstance>
    Session: ModelCtor<SessionInstance>
    VerificationToken: ModelCtor<VerificationTokenInstance>
  }>
}

export default function SequelizeAdapter(
  client: Sequelize,
  options?: SequelizeAdapterOptions
): Adapter {
  const { models } = options ?? {}
  const defaultModelOptions = { underscored: true, timestamps: false }
  const { User, Account, Session, VerificationToken } = {
    User:
      models?.User ??
      client.define<UserInstance>(
        "user",
        defaultModels.User,
        defaultModelOptions
      ),
    Account:
      models?.Account ??
      client.define<AccountInstance>(
        "account",
        defaultModels.Account,
        defaultModelOptions
      ),
    Session:
      models?.Session ??
      client.define<SessionInstance>(
        "session",
        defaultModels.Session,
        defaultModelOptions
      ),
    VerificationToken:
      models?.VerificationToken ??
      client.define<VerificationTokenInstance>(
        "verificationToken",
        defaultModels.VerificationToken,
        defaultModelOptions
      ),
  }

  Account.belongsTo(User, { onDelete: "cascade" })
  Session.belongsTo(User, { onDelete: "cascade" })

  return {
    async createUser(user) {


      return await retryableTxn(async transaction => await User.create(user, { transaction }));
    },
    async getUser(id) {


      const userInstance = await User.findByPk(id)

      return userInstance?.get({ plain: true }) ?? null
    },
    async getUserByEmail(email) {


      const userInstance = await User.findOne({
        where: { email },
      })

      return userInstance?.get({ plain: true }) ?? null
    },
    async getUserByAccount({ provider, providerAccountId }) {


      return await directTxn(async transaction => {
        const accountInstance = await Account.findOne({
          where: { provider, providerAccountId },
          transaction,
        })

        if (!accountInstance) {
          return null
        }

        const userInstance = await User.findByPk(accountInstance.userId, { transaction })

        return userInstance?.get({ plain: true }) ?? null
      });
    },
    async updateUser(user) {


      return await retryableTxn(async transaction => {
        await User.update(user, { where: { id: user.id }, transaction })
        const userInstance = await User.findByPk(user.id, { transaction })

        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return userInstance!
      });
    },
    async deleteUser(userId) {


      return await retryableTxn(async transaction => {
        const userInstance = await User.findByPk(userId, { transaction })

        await User.destroy({ where: { id: userId }, transaction })

        return userInstance
      });
    },
    async linkAccount(account) {


      await retryableTxn(async transaction => await Account.create(account, { transaction }));
    },
    async unlinkAccount({ provider, providerAccountId }) {


      await retryableTxn(async transaction => await Account.destroy({
        where: { provider, providerAccountId },
        transaction,
      }));
    },
    async createSession(session) {

      return await retryableTxn(async transaction => await Session.create(session, { transaction }));
    },
    async getSessionAndUser(sessionToken) {


      return await directTxn(async transaction => {
        const sessionInstance = await Session.findOne({
          where: { sessionToken },
          transaction,
        })

        if (!sessionInstance) {
          return null
        }

        const userInstance = await User.findByPk(sessionInstance.userId, { transaction })

        if (!userInstance) {
          return null
        }

        return {
          session: sessionInstance?.get({ plain: true }),
          user: userInstance?.get({ plain: true }),
        }
      });
    },
    async updateSession({ sessionToken, expires }) {


      return await retryableTxn(async transaction => {
        await Session.update(
          { expires, sessionToken },
          { where: { sessionToken }, transaction }
        )

        return await Session.findOne({ where: { sessionToken }, transaction })
      });
    },
    async deleteSession(sessionToken) {

      await retryableTxn(async transaction => {
        await Session.destroy({ where: { sessionToken }, transaction })
      });
    },
    async createVerificationToken(token) {


      return await retryableTxn(async transaction => {
        return await VerificationToken.create(token, { transaction })
      });
    },
    async useVerificationToken({ identifier, token }) {


      return await retryableTxn(async transaction => {
        const tokenInstance = await VerificationToken.findOne({
          where: { identifier, token },
          transaction,
        })

        await VerificationToken.destroy({ where: { identifier }, transaction })

        return tokenInstance?.get({ plain: true }) ?? null
      });
    },
  }
}
