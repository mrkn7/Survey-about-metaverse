library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyverse)
data <- read.csv("data.csv")
### at this cleaning part we basically combining turkish and english responses in a same column by converting same languages and 
#delete old data.
###################
#delete pre-undergraduate row
new <- slice(data, -c(218, 221, 224, 225, 228, 229, 230, 238, 368, 369, 370))
summary(new)
#first colum delete time
new$Zaman.damgası <- NULL
new$Thank.you.for.your.participation..Any.feedback.or.comments. <- NULL
#delete comment
new$Katılımınız.için.teşekkür.ederiz..Herhangi.bir.yorumunuz.veya..geri.bildiriminiz. <- NULL
# combining english and gender identity in a column
new$gender <- paste(new$X1.What.is.your.current.gender.identity., new$X1.Cinsiyet.kimliğiniz.nedir.)
new
# delete gender column
new <- select(new, -c(X1.What.is.your.current.gender.identity., X1.Cinsiyet.kimliğiniz.nedir.))

# combining english and turkish education level in a column
new$education <- paste(new$X2.What.is.your.current.level.of.education., new$X2.Şu.anki.eğitim.seviyeniz.nedir.)
# delete level of education 
new <- select(new, -c(X2.What.is.your.current.level.of.education., X2.Şu.anki.eğitim.seviyeniz.nedir.))

# department in a column
new$faculty <- paste(new$X3..Which.faculty.does.your.department.belong.to, new$X3..Bölümünüz.hangi.fakülteye.ait.)
#delete department 
new <- select(new, -c(X3..Which.faculty.does.your.department.belong.to, X3..Bölümünüz.hangi.fakülteye.ait.))
# have you ever heard about Metaverse concept in a column
new$metaverse.concept <- paste(new$X6..Have.you.heard.about.Metaverse.concept.before., new$X6..Metaverse.konseptini.daha.önce.duydunuz.mu.)
# delete concept 
new <- select(new, -c(X6..Have.you.heard.about.Metaverse.concept.before., X6..Metaverse.konseptini.daha.önce.duydunuz.mu.))

# VR- glasses in a column
new$virtualexperience <- paste(new$X7.Have.you.ever.had.virtual.reality.experience.before..i.e.VR.headset.., new$X7.Daha.önce.hiç.sanal.gerçeklik.deneyiminiz.oldu.mu...VR.gözlüğü.gibi..)
#delete VR- glasses
new <- select(new, -c(X7.Have.you.ever.had.virtual.reality.experience.before..i.e.VR.headset.., X7.Daha.önce.hiç.sanal.gerçeklik.deneyiminiz.oldu.mu...VR.gözlüğü.gibi..))

#metaverse general concept in a column
new$Metaversethougts <- paste(new$X8.Which.of.the.following.describe.your.thoughts.about.Metaverse., new$X8.Metaverse.hakkındaki.düşüncelerinizi.aşağıdakilerden.hangisi.açıklıyor.)
# delete metaverse general concept
new <- select(new, -c(X8.Which.of.the.following.describe.your.thoughts.about.Metaverse., X8.Metaverse.hakkındaki.düşüncelerinizi.aşağıdakilerden.hangisi.açıklıyor.))
# development area in a column
new$development.area <- paste(new$X9..If.the.Metaverse.spreads.to.the.whole.of.our.lives..in.which.areas.do.you.believe.human.beings.will.develop.more.,
                               new$X9..Metaverse.hayatımızın.tamamına.yayılırsa.insanlığın.hangi.alanda.daha.çok.gelişeceğini.düşünüyorsunuz.)
# delete development area
new <- select(new, -c(X9..If.the.Metaverse.spreads.to.the.whole.of.our.lives..in.which.areas.do.you.believe.human.beings.will.develop.more.,
                        X9..Metaverse.hayatımızın.tamamına.yayılırsa.insanlığın.hangi.alanda.daha.çok.gelişeceğini.düşünüyorsunuz.))

#metaverse leadership in a column
new$metaverseleader <- paste(new$X10.Which.company.that.you.want.lead.to.Metaverse.universe.,
                             new$X10.Hangi.şirket.Metaverse.e.öncülük.etmeli.)
#delete metaverse leadership 
new <- select(new, -c(X10.Which.company.that.you.want.lead.to.Metaverse.universe.,
                      X10.Hangi.şirket.Metaverse.e.öncülük.etmeli.))

#combine laws rule in a column
new$laws <- paste(new$X11...In.the.framework.of.the.Metaverse..should.the.government.or.companies.make.the.laws.and.rules.,
                   new$X11...Metaverse.çerçevesinde.kanun.ve.kuralları.devlet.mi.yoksa.şirketler.mi.oluşturmalı.)
#delete  laws rules 
new <- select(new, -c(X11...In.the.framework.of.the.Metaverse..should.the.government.or.companies.make.the.laws.and.rules.,
                        X11...Metaverse.çerçevesinde.kanun.ve.kuralları.devlet.mi.yoksa.şirketler.mi.oluşturmalı.))

# combining new social media in a column
new$which.social.media <- paste(new$X4.If.you.are.using.social.media..please.choose.the.applications.that.you.use...You.can.choose.more.than.one.,
                                 new$X4.Sosyal.medya.kullanıyorsanız.lütfen.kullandığınız.uygulamaları.seçiniz...Birden.fazla.seçebilirsiniz.)
#delete new social media
new <- select(new, -c(X4.If.you.are.using.social.media..please.choose.the.applications.that.you.use...You.can.choose.more.than.one.,
                        X4.Sosyal.medya.kullanıyorsanız.lütfen.kullandığınız.uygulamaları.seçiniz...Birden.fazla.seçebilirsiniz.))

#combining social media in a column 
new$socialmediatime <- paste(new$X5..How.many.hours.do.you.spend.on.social.media.per.day.,
                             new$X5..Günde.kaç.saat.sosyal.medyada.vakit.geçiriyorsunuz.)

#delete social media time 
new <- select(new, -c(X5..How.many.hours.do.you.spend.on.social.media.per.day.,
                      X5..Günde.kaç.saat.sosyal.medyada.vakit.geçiriyorsunuz.))

# economy would change in a column
new$human.rights.change <- paste(new$X12...I.think.laws...human.rights...economy.etc..would.change.in.the.Metaverse.positively.,
                                  new$X12...Metaverse.de.yasaların..insan.haklarının.ve.ekonominin.vb..nin.olumlu.yönde.değişeceğini.düşünüyorum.)
#delete economic
new <- select(new, -c(X12...I.think.laws...human.rights...economy.etc..would.change.in.the.Metaverse.positively.,
                        X12...Metaverse.de.yasaların..insan.haklarının.ve.ekonominin.vb..nin.olumlu.yönde.değişeceğini.düşünüyorum.))

##happiness and success relationship in a column
new$achievementshappiness <- paste(new$X13.My.achievements.in.the.Metaverse.world.make.me.as.happy.and.satisfied.as.in.the.real.world.,
                                   new$X13.Metaverse.dünyasındaki.başarılarım.beni.gerçek.dünyadaki.kadar.mutlu.ve.tatmin.eder.)

#delete happiness and success 
new <- select(new, -c(X13.My.achievements.in.the.Metaverse.world.make.me.as.happy.and.satisfied.as.in.the.real.world.,
                      X13.Metaverse.dünyasındaki.başarılarım.beni.gerçek.dünyadaki.kadar.mutlu.ve.tatmin.eder.))

# scares me that something might bad happen in a column
new$scares.me <- paste(new$X14.It.scares.me.that.something.bad.might.happen.in.the.Metaverse.,
                        new$X14.Metaverse.de.olabilecek.olumsuz.şeyler.beni.korkutur.)
#delete scares.me
new <- select(new, -c(X14.It.scares.me.that.something.bad.might.happen.in.the.Metaverse.,
                        X14.Metaverse.de.olabilecek.olumsuz.şeyler.beni.korkutur.))
###ethic metavese in column
new$metaverseethic <- paste(new$X15.I.think.the.Metaverse.concept.is.ethic..,
                            new$X15.Metaverse.kavramının.etik.olduğunu.düşünüyorum.)
## delete ethic metaverse
new <- select(new, -c(X15.I.think.the.Metaverse.concept.is.ethic..,
                      X15.Metaverse.kavramının.etik.olduğunu.düşünüyorum.))

# spent.time in a column
new$spend.my.time <- paste(new$X16.I.would.like.to.spend.most.of.my.time.in.Metaverse.,
                            new$X16.Zamanımın.çoğunu.Metaverse.de.geçirmek.istiyorum.)
# delete spent.time
new <- select(new, -c(X16.I.would.like.to.spend.most.of.my.time.in.Metaverse.,
                        X16.Zamanımın.çoğunu.Metaverse.de.geçirmek.istiyorum.))
###social-economic metaverse relationship in a column 
new$metaversesocioeconomic <- paste(new$X17..I.think.that.socio.economic.factors.will.affect.a.person.who.wants.to.join.to.a.Metaverse.,
                                    new$X17.Sosyo...ekonomik.faktörler.bir.kişinin.Metaverse.e.katılmasını.etkiler.)
## social-economic metaverse  in a column
new <- select(new, -c(X17..I.think.that.socio.economic.factors.will.affect.a.person.who.wants.to.join.to.a.Metaverse.,
                      X17.Sosyo...ekonomik.faktörler.bir.kişinin.Metaverse.e.katılmasını.etkiler.))
#adaptation problem in a column
new$compatibility.issues <- paste(new$X18.If.the.world.s.leading.companies.create.different.Metaverses..I.think.that.there.will.be.compatibility.issues.For.example.the.equipments.I.use.for.Facebook.Metaverse.would.not.work.well.in.Amazon.Metaverse.,
                                   new$X18.Dünyanın.önde.gelen.firmaları.farklı.Metaverseler.oluşturursa.uyumluluk.sorunları.olacağını.düşünüyorum..Örneğin.Facebook.Metaverse.için.kullandığım.ekipmanlar.Amazon.Metaverse.de.pek.işe.yaramaz.)
# delete adaptation
new <- select(new, -c(X18.If.the.world.s.leading.companies.create.different.Metaverses..I.think.that.there.will.be.compatibility.issues.For.example.the.equipments.I.use.for.Facebook.Metaverse.would.not.work.well.in.Amazon.Metaverse.,
                        X18.Dünyanın.önde.gelen.firmaları.farklı.Metaverseler.oluşturursa.uyumluluk.sorunları.olacağını.düşünüyorum..Örneğin.Facebook.Metaverse.için.kullandığım.ekipmanlar.Amazon.Metaverse.de.pek.işe.yaramaz.))


## Cyber-Syndrome metaverse relationship in a one column
new$cybercendrommetaverse <- paste(new$X19..I.think.I.would.suffer.from.cyber.syndrome.in.long.term.if.I.join.a.Metaverse...Cyber.syndrome.is.the.physical..social..and.mental.disorders.that.affect.the.human.being.due.to.the.excessive.interaction.with.the.internet.,
                                    new$X19..Bir.Metaverse.e.katılırsam.uzun.vadede.siber.sendromdan.muzdarip.olacağımı.düşünüyorum...Siber.sendrom..internet.ile.aşırı.etkileşim.nedeniyle.insanı.etkileyen.fiziksel..sosyal.ve.ruhsal.bozukluklardır.)
## Cyber-Syndrome metaverse relationship in a one column
new <- select(new, -c(X19..I.think.I.would.suffer.from.cyber.syndrome.in.long.term.if.I.join.a.Metaverse...Cyber.syndrome.is.the.physical..social..and.mental.disorders.that.affect.the.human.being.due.to.the.excessive.interaction.with.the.internet.,
                      X19..Bir.Metaverse.e.katılırsam.uzun.vadede.siber.sendromdan.muzdarip.olacağımı.düşünüyorum...Siber.sendrom..internet.ile.aşırı.etkileşim.nedeniyle.insanı.etkileyen.fiziksel..sosyal.ve.ruhsal.bozukluklardır.))

########################################################################################################










