package perspective.examples

import java.time.{Instant, OffsetDateTime}

import io.circe._

object TestBig {
  type RawSnowflake         = String
  type ChannelId            = String
  type GuildId              = String
  type UserId               = String
  type RoleId               = String
  type MessageId            = String
  type SnowflakeType[A]     = String
  type UserOrRoleId         = String
  type EmojiId              = String
  type TextChannelId        = String
  type TextGuildChannelId   = String
  type VoiceGuildChannelId  = String
  type GuildChannelId       = String
  type IntegrationId        = String
  type ConnectionVisibility = String

  type IntegrationExpireBehavior = Int
  type ChannelType               = Int
  type PermissionOverwriteType   = String
  type GuildFeature              = String
  type UserFlags                 = Long
  type Permission                = Long
  type PremiumType               = Int
  type PresenceStatus            = String
  type EmbedType                 = String
  type MessageActivityType       = Int
  type TeamMembershipState       = String
  type WebhookType               = String

  implicit lazy val offsetDateTimeEncoder: Encoder[OffsetDateTime] = ???
  implicit lazy val offsetDateTimeDecoder: Decoder[OffsetDateTime] = ???
  implicit lazy val instantEncoder: Encoder[Instant]               = ???
  implicit lazy val instantDecoder: Decoder[Instant]               = ???

  case class RawChannel(
      id: ChannelId,
      `type`: ChannelType,
      guildId: Option[GuildId],
      position: Option[Int],
      permissionOverwrites: Option[Seq[PermissionOverwrite]],
      name: Option[String],
      topic: Option[String],
      nsfw: Option[Boolean],
      lastMessageId: Option[MessageId],
      bitrate: Option[Int],
      userLimit: Option[Int],
      rateLimitPerUser: Option[Int],
      recipients: Option[Seq[User]],
      icon: Option[String],
      ownerId: Option[UserId],
      applicationId: Option[RawSnowflake],
      parentId: Option[RawSnowflake],
      lastPinTimestamp: Option[OffsetDateTime]
  )
  implicit lazy val rawChannelCodec: Codec[RawChannel] = CodecDeriver.deriver[RawChannel].derive

  case class RawEmoji(
      id: EmojiId,
      name: String,
      roles: Seq[RoleId],
      user: Option[User],
      requireColons: Option[Boolean],
      managed: Option[Boolean],
      animated: Option[Boolean],
      available: Option[Boolean]
  )

  implicit lazy val rawEmojiCodec: Codec[RawEmoji] = CodecDeriver.deriver[RawEmoji].derive

  case class GuildPreview(
      id: GuildId,
      name: String,
      icon: Option[String],
      splash: Option[String],
      discoverySplash: Option[String],
      emojis: Seq[RawEmoji],
      features: Seq[GuildFeature],
      approximateMemberCount: Int,
      approximatePresenceCount: Int,
      description: Option[String]
  )

  implicit lazy val guildPreviewCodec: Codec[GuildPreview] = CodecDeriver.deriver[GuildPreview].derive

  case class PartialUser(
      id: UserId,
      username: Option[String],
      discriminator: Option[String],
      avatar: Option[String], // avatar can be null
      bot: Option[Boolean],
      system: Option[Boolean],
      mfaEnabled: Option[Boolean],
      locale: Option[String],
      verified: Option[Boolean],
      email: Option[String],
      flags: Option[UserFlags],
      premiumType: Option[PremiumType]
  )

  implicit lazy val partialUserCodec: Codec[PartialUser] = CodecDeriver.deriver[PartialUser].derive

  case class RawActivity(
      name: String,
      `type`: Int,
      url: Option[String],
      createdAt: Instant,
      timestamps: Option[ActivityTimestamps],
      applicationId: Option[RawSnowflake],
      details: Option[String],
      state: Option[String],
      emoji: Option[ActivityEmoji],
      party: Option[RawActivityParty],
      assets: Option[ActivityAsset]
  )

  implicit lazy val rawActivityCodec: Codec[RawActivity] = CodecDeriver.deriver[RawActivity].derive

  case class ActivityTimestamps(start: Option[Instant], end: Option[Instant])

  implicit lazy val activityTimestampsCodec: Codec[ActivityTimestamps] = CodecDeriver.deriver[ActivityTimestamps].derive

  case class ActivityAsset(
      largeImage: Option[String],
      largeText: Option[String],
      smallImage: Option[String],
      smallText: Option[String]
  )

  implicit lazy val activityAssetCodec: Codec[ActivityAsset] = CodecDeriver.deriver[ActivityAsset].derive

  case class RawActivityParty(id: Option[String], size: Option[Seq[Int]])

  implicit lazy val rawActivityPartyCodec: Codec[RawActivityParty] = CodecDeriver.deriver[RawActivityParty].derive

  case class ActivityEmoji(
      name: String,
      id: Option[EmojiId],
      animated: Option[Boolean]
  )

  implicit lazy val activityEmojiCodec: Codec[ActivityEmoji] = CodecDeriver.deriver[ActivityEmoji].derive

  case class RawPresence(user: PartialUser, game: Option[RawActivity], status: Option[PresenceStatus])

  implicit lazy val rawPresenceCodec: Codec[RawPresence] = CodecDeriver.deriver[RawPresence].derive

  case class UnavailableGuild(id: GuildId, unavailable: Boolean)

  implicit lazy val unavailableGuildCodec: Codec[UnavailableGuild] = CodecDeriver.deriver[UnavailableGuild].derive

  case class PermissionOverwrite(id: UserOrRoleId, `type`: PermissionOverwriteType, allow: Permission, deny: Permission)

  implicit lazy val permissionOverwriteCodec: Codec[PermissionOverwrite] =
    CodecDeriver.deriver[PermissionOverwrite].derive

  case class User(
      id: UserId,
      username: String,
      discriminator: String,
      avatar: Option[String],
      bot: Option[Boolean],
      system: Option[Boolean],
      mfaEnabled: Option[Boolean],
      locale: Option[String],
      verified: Option[Boolean],
      email: Option[String],
      flags: Option[UserFlags],
      premiumType: Option[PremiumType]
  )

  implicit lazy val userCodec: Codec[User] = CodecDeriver.deriver[User].derive

  case class WebhookAuthor(id: SnowflakeType[Webhook], username: String, avatar: String)

  implicit lazy val webhookAuthorCodec: Codec[WebhookAuthor] = CodecDeriver.deriver[WebhookAuthor].derive

  case class Role(
      id: RoleId,
      guildId: GuildId,
      name: String,
      color: Int,
      hoist: Boolean,
      position: Int,
      permissions: Permission,
      managed: Boolean,
      mentionable: Boolean
  )

  implicit lazy val roleCodec: Codec[Role] = CodecDeriver.deriver[Role].derive

  case class RawRole(
      id: RoleId,
      name: String,
      color: Int,
      hoist: Boolean,
      position: Int,
      permissions: Permission,
      managed: Boolean,
      mentionable: Boolean
  )

  implicit lazy val rawRoleCodec: Codec[RawRole] = CodecDeriver.deriver[RawRole].derive

  case class RawGuildMember(
      user: User,
      nick: Option[String],
      roles: Seq[RoleId],
      joinedAt: OffsetDateTime,
      premiumSince: Option[OffsetDateTime],
      deaf: Boolean,
      mute: Boolean
  )

  implicit lazy val rawGuildMemberCodec: Codec[RawGuildMember] = CodecDeriver.deriver[RawGuildMember].derive

  case class Attachment(
      id: SnowflakeType[Attachment],
      filename: String,
      size: Int,
      url: String,
      proxyUrl: String,
      height: Option[Int],
      width: Option[Int]
  )

  implicit lazy val attachementCodec: Codec[Attachment] = CodecDeriver.deriver[Attachment].derive

  case class EmbedField(name: String, value: String, `inline`: Option[Boolean] = None)

  implicit lazy val embedFieldCodec: Codec[EmbedField] = CodecDeriver.deriver[EmbedField].derive

  case class ReceivedEmbedFooter(text: String, iconUrl: Option[String], proxyIconUrl: Option[String])

  implicit lazy val receivedEmbedFooterCodec: Codec[ReceivedEmbedFooter] =
    CodecDeriver.deriver[ReceivedEmbedFooter].derive

  case class ReceivedEmbedImage(url: Option[String], proxyUrl: Option[String], height: Option[Int], width: Option[Int])

  implicit lazy val receivedEmbedImageCodec: Codec[ReceivedEmbedImage] = CodecDeriver.deriver[ReceivedEmbedImage].derive

  case class ReceivedEmbedThumbnail(
      url: Option[String],
      proxyUrl: Option[String],
      height: Option[Int],
      width: Option[Int]
  )

  implicit lazy val receivedEmbedThumbnailCodec: Codec[ReceivedEmbedThumbnail] =
    CodecDeriver.deriver[ReceivedEmbedThumbnail].derive

  case class ReceivedEmbedVideo(url: Option[String], height: Option[Int], width: Option[Int])

  implicit lazy val receivedEmbedVideoCodec: Codec[ReceivedEmbedVideo] = CodecDeriver.deriver[ReceivedEmbedVideo].derive

  case class ReceivedEmbedProvider(name: Option[String], url: Option[String])

  implicit lazy val receivedEmbedProviderCodec: Codec[ReceivedEmbedProvider] =
    CodecDeriver.deriver[ReceivedEmbedProvider].derive

  case class ReceivedEmbedAuthor(
      name: Option[String],
      url: Option[String],
      iconUrl: Option[String],
      proxyIconUrl: Option[String]
  )

  implicit lazy val receivedEmbedAuthorCodec: Codec[ReceivedEmbedAuthor] =
    CodecDeriver.deriver[ReceivedEmbedAuthor].derive

  case class ReceivedEmbed(
      title: Option[String],
      `type`: Option[EmbedType],
      description: Option[String],
      url: Option[String],
      timestamp: Option[OffsetDateTime],
      color: Option[Int],
      footer: Option[ReceivedEmbedFooter],
      image: Option[ReceivedEmbedImage],
      thumbnail: Option[ReceivedEmbedThumbnail],
      video: Option[ReceivedEmbedVideo],
      provider: Option[ReceivedEmbedProvider],
      author: Option[ReceivedEmbedAuthor],
      fields: Option[Seq[EmbedField]]
  )

  implicit lazy val receivedEmbedCodec: Codec[ReceivedEmbed] = CodecDeriver.deriver[ReceivedEmbed].derive

  case class OutgoingEmbedFooter(text: String, iconUrl: Option[String] = None)

  implicit lazy val outgoingEmbedFooterCodec: Codec[OutgoingEmbedFooter] =
    CodecDeriver.deriver[OutgoingEmbedFooter].derive

  case class OutgoingEmbedImage(url: String)

  implicit lazy val outgoingEmbedImageCodec: Codec[OutgoingEmbedImage] = CodecDeriver.deriver[OutgoingEmbedImage].derive

  case class OutgoingEmbedVideo(url: String)

  implicit lazy val outgoingEmbedVideoCodec: Codec[OutgoingEmbedVideo] = CodecDeriver.deriver[OutgoingEmbedVideo].derive

  case class OutgoingEmbedThumbnail(url: String)

  implicit lazy val outgoingEmbedThumbnailCodec: Codec[OutgoingEmbedThumbnail] =
    CodecDeriver.deriver[OutgoingEmbedThumbnail].derive

  case class OutgoingEmbedAuthor(name: String, url: Option[String] = None, iconUrl: Option[String] = None)

  implicit lazy val outgoingEmbedAuthorCodec: Codec[OutgoingEmbedAuthor] =
    CodecDeriver.deriver[OutgoingEmbedAuthor].derive

  case class OutgoingEmbed(
      title: Option[String] = None,
      description: Option[String] = None,
      url: Option[String] = None,
      timestamp: Option[OffsetDateTime] = None,
      color: Option[Int] = None,
      footer: Option[OutgoingEmbedFooter] = None,
      image: Option[OutgoingEmbedImage] = None,
      video: Option[OutgoingEmbedVideo] = None,
      thumbnail: Option[OutgoingEmbedThumbnail] = None,
      author: Option[OutgoingEmbedAuthor] = None,
      fields: Seq[EmbedField] = Seq.empty
  )

  implicit lazy val outgoingEmbedCodec: Codec[OutgoingEmbed] = CodecDeriver.deriver[OutgoingEmbed].derive

  case class PartialEmoji(id: Option[EmojiId], name: Option[String])

  implicit lazy val partialEmojiCodec: Codec[PartialEmoji] = CodecDeriver.deriver[PartialEmoji].derive

  case class Reaction(count: Int, me: Boolean, emoji: PartialEmoji)

  implicit lazy val reactionCodec: Codec[Reaction] = CodecDeriver.deriver[Reaction].derive

  case class RawMessageActivity(`type`: MessageActivityType, partyId: Option[String])

  implicit lazy val rawMessageActivityCodec: Codec[RawMessageActivity] = CodecDeriver.deriver[RawMessageActivity].derive

  case class MessageApplication(
      id: RawSnowflake,
      coverImage: Option[String],
      description: String,
      icon: String,
      name: String
  )

  implicit lazy val messageApplicationCodec: Codec[MessageApplication] = CodecDeriver.deriver[MessageApplication].derive

  case class PartialRawGuildMember(
      nick: Option[String],
      roles: Seq[RoleId],
      joinedAt: OffsetDateTime,
      premiumSince: Option[OffsetDateTime],
      deaf: Boolean,
      mute: Boolean
  )

  implicit lazy val partialRawGuildMemberCodec: Codec[PartialRawGuildMember] =
    CodecDeriver.deriver[PartialRawGuildMember].derive

  case class ChannelMention(
      id: TextChannelId,
      guildId: GuildId,
      `type`: ChannelType,
      name: String
  )

  implicit lazy val channelMentionCodec: Codec[ChannelMention] = CodecDeriver.deriver[ChannelMention].derive

  case class MessageReference(
      messageId: Option[MessageId],
      channelId: TextChannelId,
      guildId: Option[GuildId]
  )

  implicit lazy val messageReferenceCodec: Codec[MessageReference] = CodecDeriver.deriver[MessageReference].derive

  case class VoiceState(
      guildId: Option[GuildId],
      channelId: Option[VoiceGuildChannelId],
      userId: UserId,
      member: Option[RawGuildMember],
      sessionId: String,
      deaf: Boolean,
      mute: Boolean,
      selfDeaf: Boolean,
      selfMute: Boolean,
      selfStream: Option[Boolean],
      suppress: Boolean
  )

  implicit lazy val voiceStateCodec: Codec[VoiceState] = CodecDeriver.deriver[VoiceState].derive

  case class InviteGuild(id: GuildId, name: String, splash: Option[String], icon: Option[String])

  implicit lazy val inviteGuildCodec: Codec[InviteGuild] = CodecDeriver.deriver[InviteGuild].derive

  case class InviteChannel(id: GuildChannelId, name: String, `type`: ChannelType)

  implicit lazy val inviteChannelCodec: Codec[InviteChannel] = CodecDeriver.deriver[InviteChannel].derive

  case class InviteTargetUser(
      id: UserId,
      name: String,
      avatar: Option[String],
      discriminator: String
  )

  implicit lazy val inviteTargetUserCodec: Codec[InviteTargetUser] = CodecDeriver.deriver[InviteTargetUser].derive

  case class Invite(
      code: String,
      guild: Option[InviteGuild],
      channel: InviteChannel,
      inviter: Option[User],
      targetUser: Option[InviteTargetUser],
      targetUserType: Option[Int],
      approximatePresenceCount: Option[Int],
      approximateMemberCount: Option[Int]
  )

  implicit lazy val inviteCodec: Codec[Invite] = CodecDeriver.deriver[Invite].derive

  case class InviteWithMetadata(
      code: String,
      guild: Option[InviteGuild],
      channel: InviteChannel,
      inviter: Option[User],
      targetUser: Option[InviteTargetUser],
      targetUserType: Option[Int],
      approximatePresenceCount: Option[Int],
      approximateMemberCount: Option[Int],
      uses: Int,
      maxUses: Int,
      maxAge: Int,
      temporary: Boolean,
      createdAt: OffsetDateTime
  )

  implicit lazy val inviteWithMetadataCodec: Codec[InviteWithMetadata] = CodecDeriver.deriver[InviteWithMetadata].derive

  case class GuildEmbed(enabled: Boolean, channelId: Option[GuildChannelId])

  implicit lazy val guildEmbedCodec: Codec[GuildEmbed] = CodecDeriver.deriver[GuildEmbed].derive

  case class IntegrationAccount(id: String, name: String)

  implicit lazy val integrationAccountCodec: Codec[IntegrationAccount] = CodecDeriver.deriver[IntegrationAccount].derive

  case class PartialIntegration(
      id: IntegrationId,
      name: String,
      `type`: String,
      account: IntegrationAccount
  )

  implicit lazy val partialIntegrationCodec: Codec[PartialIntegration] = CodecDeriver.deriver[PartialIntegration].derive

  case class Integration(
      id: IntegrationId,
      name: String,
      `type`: String, // TODO: Use enum here
      enabled: Boolean,
      syncing: Boolean,
      roleId: RoleId,
      enableEmoticons: Option[Boolean],
      expireBehavior: IntegrationExpireBehavior,
      expireGracePeriod: Int,
      user: User,
      account: IntegrationAccount,
      syncedAt: OffsetDateTime
  )

  implicit lazy val integrationCodec: Codec[Integration] = CodecDeriver.deriver[Integration].derive

  case class VoiceRegion(
      id: String,
      name: String,
      sampleHostname: String,
      samplePort: Int,
      vip: Boolean,
      optimal: Boolean,
      deprecated: Boolean,
      custom: Boolean
  )

  implicit lazy val voiceRegionCodec: Codec[VoiceRegion] = CodecDeriver.deriver[VoiceRegion].derive

  case class Connection(
      id: String,
      name: String,
      `type`: String,
      revoked: Boolean,
      integrations: Seq[Integration], // TODO: Partial
      verified: Boolean,
      friendSync: Boolean,
      showActivity: Boolean,
      visibility: ConnectionVisibility
  )

  implicit lazy val connectionCodec: Codec[Connection] = CodecDeriver.deriver[Connection].derive

  case class Webhook(
      id: SnowflakeType[Webhook],
      `type`: WebhookType,
      guildId: Option[GuildId],
      channelId: TextGuildChannelId,
      user: Option[User],
      name: Option[String],
      avatar: Option[String],
      token: Option[String]
  )

  implicit lazy val webhookCodec: Codec[Webhook] = CodecDeriver.deriver[Webhook].derive

  case class AuditLog(
      webhooks: Seq[Webhook],
      users: Seq[User],
      auditLogEntries: Seq[AuditLogEntry],
      integrations: Seq[PartialIntegration]
  )

  implicit lazy val auditLogDecoder: Decoder[AuditLog] = DecoderDerive.deriver[AuditLog].deriveProduct

  case class AuditLogEntry(
      targetId: Option[RawSnowflake],
      changes: Option[Seq[AuditLogChange[_]]],
      userId: UserId,
      id: RawSnowflake,
      actionType: Int,
      options: Option[OptionalAuditLogInfo],
      reason: Option[String]
  )

  implicit lazy val auditLogEntryDecoder: Decoder[AuditLogEntry] = DecoderDerive.deriver[AuditLogEntry].deriveProduct

  case class OptionalAuditLogInfo(
      deleteMemberDays: Option[String],
      membersRemoved: Option[String],
      channelId: Option[TextChannelId],
      messageId: Option[MessageId],
      count: Option[String],
      id: Option[UserOrRoleId],
      `type`: Option[PermissionOverwriteType],
      roleName: Option[String]
  )

  implicit lazy val optionalAuditLogInfoCodec: Codec[OptionalAuditLogInfo] =
    CodecDeriver.deriver[OptionalAuditLogInfo].derive

  case class PartialRole(
      name: String,
      id: RoleId
  )

  implicit lazy val partialRoleCodec: Codec[PartialRole] = CodecDeriver.deriver[PartialRole].derive

  sealed trait AuditLogChange[A] {

    def newValue: Option[A]
    def oldValue: Option[A]
  }
  object AuditLogChange {
    type MFALevel          = String
    type FilterLevel       = String
    type NotificationLevel = String

    case class Name(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class IconHash(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class SplashHash(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class OwnerId(oldValue: Option[UserId], newValue: Option[UserId]) extends AuditLogChange[UserId]

    case class Region(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class AfkChannelId(oldValue: Option[VoiceGuildChannelId], newValue: Option[VoiceGuildChannelId])
        extends AuditLogChange[VoiceGuildChannelId]

    case class AfkTimeout(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class MfaLevel(oldValue: Option[MFALevel], newValue: Option[MFALevel]) extends AuditLogChange[MFALevel]

    case class VerificationLevel(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class ExplicitContentFilter(oldValue: Option[FilterLevel], newValue: Option[FilterLevel])
        extends AuditLogChange[FilterLevel]

    case class DefaultMessageNotification(
        oldValue: Option[NotificationLevel],
        newValue: Option[NotificationLevel]
    ) extends AuditLogChange[NotificationLevel]

    case class VanityUrlCode(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class $Add(oldValue: Option[Seq[PartialRole]], newValue: Option[Seq[PartialRole]])
        extends AuditLogChange[Seq[PartialRole]]

    case class $Remove(oldValue: Option[Seq[PartialRole]], newValue: Option[Seq[PartialRole]])
        extends AuditLogChange[Seq[PartialRole]]

    case class PruneDeleteDays(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class WidgetEnabled(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class WidgetChannelId(oldValue: Option[GuildChannelId], newValue: Option[GuildChannelId])
        extends AuditLogChange[GuildChannelId]

    case class SystemChannelId(oldValue: Option[TextGuildChannelId], newValue: Option[TextGuildChannelId])
        extends AuditLogChange[TextGuildChannelId]

    case class Position(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class Topic(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class Bitrate(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class PermissionOverwrites(
        oldValue: Option[Seq[PermissionOverwrite]],
        newValue: Option[Seq[PermissionOverwrite]]
    ) extends AuditLogChange[Seq[PermissionOverwrite]]

    case class NSFW(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class ApplicationId(oldValue: Option[RawSnowflake], newValue: Option[RawSnowflake])
        extends AuditLogChange[RawSnowflake]

    case class RateLimitPerUser(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class Permissions(oldValue: Option[Permission], newValue: Option[Permission])
        extends AuditLogChange[Permission]

    case class Color(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class Hoist(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class Mentionable(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class Allow(oldValue: Option[Permission], newValue: Option[Permission]) extends AuditLogChange[Permission]

    case class Deny(oldValue: Option[Permission], newValue: Option[Permission]) extends AuditLogChange[Permission]

    case class Code(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class InviteChannelId(oldValue: Option[GuildChannelId], newValue: Option[GuildChannelId])
        extends AuditLogChange[GuildChannelId]

    case class InviterId(oldValue: Option[UserId], newValue: Option[UserId]) extends AuditLogChange[UserId]

    case class MaxUses(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class Uses(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class MaxAge(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class Temporary(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class Deaf(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class Mute(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class Nick(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class AvatarHash(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class Id(oldValue: Option[RawSnowflake], newValue: Option[RawSnowflake]) extends AuditLogChange[RawSnowflake]

    case class TypeInt(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class TypeString(oldValue: Option[String], newValue: Option[String]) extends AuditLogChange[String]

    case class EnableEmoticons(oldValue: Option[Boolean], newValue: Option[Boolean]) extends AuditLogChange[Boolean]

    case class ExpireBehavior(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]

    case class ExpireGracePeriod(oldValue: Option[Int], newValue: Option[Int]) extends AuditLogChange[Int]
  }

  implicit val auditLogChangeDecoder: Decoder[AuditLogChange[_]] = (c: ACursor) => {

    def mkChange[A: Decoder, B](create: (A, A) => B): Decoder.Result[B] =
      for {
        oldVal <- c.get[A]("old_value")
        newVal <- c.get[A]("new_value")
      } yield create(oldVal, newVal)

    c.get[String]("key").flatMap {
      case "name"                          => mkChange(AuditLogChange.Name)
      case "icon_hash"                     => mkChange(AuditLogChange.IconHash)
      case "splash_hash"                   => mkChange(AuditLogChange.SplashHash)
      case "owner_id"                      => mkChange(AuditLogChange.OwnerId)
      case "region"                        => mkChange(AuditLogChange.Region)
      case "afk_channel_id"                => mkChange(AuditLogChange.AfkChannelId)
      case "afk_timeout"                   => mkChange(AuditLogChange.AfkTimeout)
      case "mfa_level"                     => mkChange(AuditLogChange.MfaLevel)
      case "verification_level"            => mkChange(AuditLogChange.VerificationLevel)
      case "explicit_content_filter"       => mkChange(AuditLogChange.ExplicitContentFilter)
      case "default_message_notifications" => mkChange(AuditLogChange.DefaultMessageNotification)
      case "vanity_url_code"               => mkChange(AuditLogChange.VanityUrlCode)
      case "$add"                          => mkChange(AuditLogChange.$Add)
      case "$remove"                       => mkChange(AuditLogChange.$Remove)
      case "prune_delete_days"             => mkChange(AuditLogChange.PruneDeleteDays)
      case "widget_enabled"                => mkChange(AuditLogChange.WidgetEnabled)
      case "widget_channel_id"             => mkChange(AuditLogChange.WidgetChannelId)
      case "system_channel_id"             => mkChange(AuditLogChange.SystemChannelId)
      case "position"                      => mkChange(AuditLogChange.Position)
      case "topic"                         => mkChange(AuditLogChange.Topic)
      case "bitrate"                       => mkChange(AuditLogChange.Bitrate)
      case "permission_overwrites"         => mkChange(AuditLogChange.PermissionOverwrites)
      case "nsfw"                          => mkChange(AuditLogChange.NSFW)
      case "application_id"                => mkChange(AuditLogChange.ApplicationId)
      case "rate_limit_per_user"           => mkChange(AuditLogChange.RateLimitPerUser)
      case "permissions"                   => mkChange(AuditLogChange.Permissions)
      case "color"                         => mkChange(AuditLogChange.Color)
      case "hoist"                         => mkChange(AuditLogChange.Hoist)
      case "mentionable"                   => mkChange(AuditLogChange.Mentionable)
      case "allow"                         => mkChange(AuditLogChange.Allow)
      case "deny"                          => mkChange(AuditLogChange.Deny)
      case "code"                          => mkChange(AuditLogChange.Code)
      case "channel_id"                    => mkChange(AuditLogChange.InviteChannelId)
      case "inviter_id"                    => mkChange(AuditLogChange.InviterId)
      case "max_uses"                      => mkChange(AuditLogChange.MaxUses)
      case "uses"                          => mkChange(AuditLogChange.Uses)
      case "max_age"                       => mkChange(AuditLogChange.MaxAge)
      case "temporary"                     => mkChange(AuditLogChange.Temporary)
      case "deaf"                          => mkChange(AuditLogChange.Deaf)
      case "mute"                          => mkChange(AuditLogChange.Mute)
      case "nick"                          => mkChange(AuditLogChange.Nick)
      case "avatar_hash"                   => mkChange(AuditLogChange.AvatarHash)
      case "id"                            => mkChange(AuditLogChange.Id)
      case "type"             => mkChange(AuditLogChange.TypeInt).left.flatMap(_ => mkChange(AuditLogChange.TypeString))
      case "enable_emoticons" => mkChange(AuditLogChange.EnableEmoticons)
      case "expire_behavior"  => mkChange(AuditLogChange.ExpireBehavior)
      case "expire_grace_period" => mkChange(AuditLogChange.ExpireGracePeriod)
    }
  }

  case class RawBan(reason: Option[String], user: User)

  implicit lazy val rawBanCodec: Codec[RawBan] = CodecDeriver.deriver[RawBan].derive

  case class ClientStatus(
      desktop: Option[PresenceStatus],
      mobile: Option[PresenceStatus],
      web: Option[PresenceStatus]
  )

  implicit lazy val clientStatusCodec: Codec[ClientStatus] = CodecDeriver.deriver[ClientStatus].derive

  case class Team(
      icon: Option[String],
      id: SnowflakeType[Team],
      members: Seq[TeamMember],
      ownerUserId: UserId
  )

  implicit lazy val teamCodec: Codec[Team] = CodecDeriver.deriver[Team].derive

  case class TeamMember(
      membershipState: TeamMembershipState,
      permissions: Seq[String],
      teamId: SnowflakeType[Team],
      user: PartialUser
  )

  implicit lazy val teamMemberCodec: Codec[TeamMember] = CodecDeriver.deriver[TeamMember].derive

}
