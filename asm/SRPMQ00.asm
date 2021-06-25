*          DATA SET SRPMQ00    AT LEVEL 024 AS OF 05/11/16                      
*PHASE T17200A                                                                  
*        TITLE 'SRPMQ00 - PRINT/MQ INTERFACE'                                   
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - CHANGE LOG'                      
***********************************************************************         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* BOBY     06/05  ADD ESR PROCESSING                                            
*                                                                               
* BOBY     02/05  SEND ERROR MESSAGES VIA E-MAIL                                
*                                                                               
* BOBY     05/04  BIG BANG                                                      
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - INITIALIZATION'                  
***********************************************************************         
*                                                                     *         
*        INITIALIZATION - COPY MQ MESSAGE TO TWA                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SRPMQ00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**$PMQ**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    RD,SYSRD            SAVE INCOMING RD                             
         MVC   SRPARMS,0(R1)                                                    
*                                                                               
         USING SRPARMD,R1                                                       
         L     R9,SRQAUTL                                                       
         USING UTLD,R9             R9=A(UTL)                                    
         ST    R9,AUTL                                                          
*                                                                               
         L     RA,SRPATWA          RA=A(TWA)                                    
         USING TWAD,RA                                                          
*                                                                               
*        FIND VSSB FOR TRACING OPTION                                           
*                                                                               
         L     RF,SRQASYSF                                                      
         USING SYSFACD,RF                                                       
         ST    RF,ASYSFACS                                                      
*                                                                               
         L     RE,VSSB                                                          
         ST    RE,ASSB                                                          
*                                                                               
         USING SSBD,RE                                                          
         MVC   SVMQION,SSBMQION    TRACE OPTION                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
*        GET TBUFFER ADDRESS                                                    
******                                                                          
******   L     R1,SRPAUTL                                                       
******   ICM   R9,15,TBUFF-UTLD(R1)                                             
******   ST    R9,ATBUFF           SAVE TBUFF ADDRESS                           
******                                                                          
*                                                                               
*        MOVE MQ MESSAGE TO TIA - MAY BE TOO LONG FOR TBUFF                     
*                                                                               
         L     RE,ASYSFACS         POINT TO SYSTEM FACILITIES                   
         USING SYSFACD,RE          ESTABLISH SYSTEM FACILITIES                  
         ICM   RE,15,VSSB                                                       
         ICM   RE,15,SSBTKADR-SSBD(RE)                                          
         BNZ   *+6                                                              
         DC    H'0'                <== NEVER HAPPENS                            
*                                                                               
         USING TCBD,RE             ESTABLISH TCB                                
*                                                                               
         SAM31                     TURN ON 31 BIT ADDRESSABILITY                
         L     RE,TCBRBUFF         POINT TO HIGH CORE BUFFER                    
*                                                                               
         SHI   RE,2                BACKUP TO TCBRBUFF LENGTH                    
         SR    R1,R1                                                            
         ICM   R1,3,0(RE)          GET BUFFER LENGTH                            
*                                                                               
         LR    R2,R1               SAVE ORIGINAL MESSAGE LENGTH                 
*                                                                               
         CHI   R1,X'3C00'          LENGTH CAN'T EXCEED 15K                      
         BNH   *+8                                                              
         LHI   R1,X'3C00'                                                       
*                                                                               
         AHI   RE,2                RESTORE BUFFER ADDRESS                       
*                                                                               
         L     R0,SRPATIA          SAVE MESSAGE IN TIA                          
         LR    RF,R1               MESSAGE LENGTH                               
*                                                                               
         STH   RF,MQMSGLN          SAVE MESSAGE LENGTH                          
*                                                                               
         MVCL  R0,RE               SAVE INCOMING MESSAGE                        
*                                                                               
         SAM24                     RETURN TO 24 ADDRESSING                      
*                                                                               
         DROP  RE                                                               
*                                                                               
         L     R1,SRPATIA          A(SAVED MQ MESSAGE)                          
         AH    R1,MQMSGLN          ==> END OF BUFFER                            
         MVC   0(32,R1),=32C'0'    SET DELIMITER FIELD AT END OF BUFFER         
*                                                                               
         L     R9,SRPATIA          POINT TO MQ MESSAGE                          
*                                                                               
         BRAS  RE,SYSINIT          INITIALZE                                    
*                                                                               
         CHI   R2,X'3C00'          IF MSG LENGTH GREATER THAN 15K               
         BNH   *+12                                                             
         STH   R2,MQMSGLN             SAVE MESSAGE LENGTH                       
         B     MSGLNGER               SEND ERROR E-MAIL                         
*                                                                               
         LA    R7,MNBLKCB          POINT TO MINIO CONTROL BLOCK                 
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - MQCALL'                          
***********************************************************************         
*                                                                     *         
*        ANALIZE MQ CALL                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MQCALL   DS    0H                                                               
*                                                                               
         USING EZMQMSGD,R9         ESTABLISH MQ REPLY                           
*                                                                               
         CLC   SVMQION,=F'2'       SKIP IF NO TRACING                           
         BNE   MQTR1                                                            
*                                                                               
         GOTOR VDMGR,DMCB,=C'OPMSG',(70,EZMQMSGD)                               
         GOTOR VDMGR,DMCB,=C'OPMSG',(70,EZMQMSGD+70)                            
*                                                                               
MQTR1    DS    0H                                                               
*                                                                               
         CLC   EZMLABEL,=C'PRTADB'    DONE IF NOT FROM ADBUYER                  
         BNE   MQCALLX                                                          
*                                                                               
         CLC   =C'WEBIO',EZMUSER      IF FROM WEBIO                             
         BE    *+10                                                             
         CLC   =C'ESR  ',EZMUSER      IF FROM ESR                               
         BNE   *+12                                                             
         BRAS  RE,WEBIO               GO HANDLE                                 
         B     MQCALLX                                                          
*                                                                               
         BRAS  RE,EDICT            HANDLE EDICT REPLY                           
*                                                                               
MQCALLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
MSGLNGER DS    0H                  MESSAGE TOO LONG ERROR                       
         MVC   PERROR,=X'FE05'     SET ERROR CODE                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - EDICT'                           
***********************************************************************         
*                                                                     *         
*        HANDLE EDICT REPLY                                           *         
*              REPLY IS TO RETURN STATUS OF A FAX REQUEST             *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDICT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA                                                          
         USING WORKD,RC            RC=A(W/S)                                    
         USING EZMQMSGD,R9         ESTABLISH EDICT REPLY                        
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         LA    R2,EZMAPPL          POINT TO RETURNED ID DATA                    
         USING PPEDAREA,R2         ESTABLISH ID DATA                            
*                                                                               
         LHI   R8,EMLFLDS-WORKD    ESTABLISH E-MAIL FIELDS                      
         LA    R8,WORKD(R8)                                                     
         USING EMLFLDS,R8                                                       
*                                                                               
         LA    RF,PPEAGRP          ASSUME NEW FORMAT                            
         LA    R0,L'PPEAIO#        ASSUME LONGER IO# LENGTH                     
*                                                                               
         CLI   PPEIND,C'I'         IF NOT NEW FORMAT                            
         BE    *+12                                                             
         SHI   RF,3                   BACK UP 3 POSITIONS                       
         LA    R0,L'PPEAIO#-1         RESET IO# LENGTH                          
*                                                                               
         USING PPEAGRP,RF          ESTTABLISH LATTER PART OF AREA               
*                                                                               
         MVC   QAGY,PPEAAGY        SAVE AGENCY ALPHA                            
*                                                                               
         MVI   QTYP,WIOKRCDQ       ASSUME EIO                                   
         CLC   =C'ESR',PPEATYP     IF ESR                                       
         BNE   *+8                                                              
         MVI   QTYP,ESRKRCDQ          CHANGE TYPE                               
*                                                                               
         PACK  DUB,PPEAGRP         GET RETURNED GROUP NUMBER                    
         TP    DUB                 MUST BE A PACKED NUMBER                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CVB   RE,DUB                                                           
         STC   RE,QFGP#            SAVE GROUP NUMBER                            
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTOR SETDMGR             SET DATAMANAGER TO CORRECT FILES             
*                                                                               
         GOTOR PRSIO#,DMCB,((R0),PPEAIO#)   PARSE IO#/SR#                       
*                                                                               
         LHI   R0,L'PPEAPUB        MAX LENGTH OF PUB NUMBER                     
         LA    R1,PPEAPUB+L'PPEAPUB-1  POINT TO LAST POSITION                   
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK                          
         BH    *+12                                                             
         SHI   R1,1                BACK UP A POSITION                           
         BCT   R0,*-12                                                          
*                                  R0 HAS PUBCODE LENGTH                        
         GOTOR VALPUB,DMCB,((R0),PPEAPUB)   VALIDATE PUB                        
*                                                                               
         GOTOR MININIT             INITIALIZE MINIO BLOCK                       
*                                                                               
*        EIO AND ESR ARE ALMOST IDENTICAL IN STRUCTURE                          
*        WE WILL USE EIO FOR PROCESSING WITH SMALL TWEAKS FOR ESR               
*                                                                               
         LA    R4,MINMKEY          ESTABLISH MINIO MASTER KEY                   
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVC   WIOKRCD,QTYP        SET RECORD IDENTIFIER                        
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
         MVC   WIOKIO#,QIO#        SET IO#                                      
         MVC   WIOKRV#,QREV#       SET REVISION NUMBER                          
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO BLOCK             
         CLI   MINERR,0            CHECK FOR ERRORS                             
         BNE   EDTNFER             RECORD NOT FOUND                             
*                                                                               
         CLI   EZMSTAT,C'C'        IF CANCELLED                                 
         BNE   *+8                                                              
         MVI   EZMSTAT,WIOSUDLQ       MAKE UNDELIVERED                          
*                                                                               
         MVC   QSTAT,EZMSTAT       SAVE STATUS CODE                             
*                                                                               
         GOTOR VALSTCD,DMCB,0      VALIDATE STATUS CODE                         
*                                                                               
*        GET CURRENT DATE AND TIME                                              
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(25,QSTADATE)                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QSTATIME         ADJUST DDS HOURS TO REAL TIME                
         AHI   RF,6                                                             
         STC   RF,QSTATIME                                                      
*                                                                               
*        SET STATUS FOR INDIVIDUAL FAX NUMBER                                   
*                                                                               
EDCFAX   DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH FAX ELEMENT                        
         USING WIOFAXD,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
*                                                                               
         MVI   WIOFKTYP,WIOFKFXQ   SET FOR FAX ELEMENT                          
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY MATCH ON GROUP NUMBER                  
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX ELEMENT                       
         BNZ   EDCFAXDN            NOT FOUND                                    
*                                                                               
EDCFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT FAX ELM                          
         BNE   EDCFAXDN                                                         
*                                                                               
         CLC   WIOFKGP#,QFGP#      DONE IF NOT FAX GROUP                        
         BNE   EDCFAXDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKFXQ   DONE IF NOT FAX NUMBER                       
         BNE   EDCFAXDN                                                         
*                                                                               
*        STRIP DIGITS FROM FILE  FAX NUMBER                                     
*                                                                               
         GOTOR DGTFX#,DMCB,(L'WIOFX#,WIOFX#),(L'FX#FILE,FX#FILE)                
*                                                                               
*        STRIP DIGITS FROM INPUT FAX NUMBER                                     
*                                                                               
         GOTOR DGTFX#,DMCB,(L'EZDEST,EZDEST),(L'FX#INPUT,FX#INPUT)              
*                                                                               
         CLC   FX#FILE,FX#INPUT    MATCH ON RETURNED FAX #                      
         BE    EDCFAXFD                                                         
*                                                                               
EDCFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     EDCFAXLP                                                         
*                                                                               
EDCFAXDN DS    0H                                                               
*                                                                               
         B     EDICTX              FAX NUMBER NOT FOUND                         
*                                                                               
EDCFAXFD DS    0H                                                               
*                                                                               
         MVC   WIOFXSTA,QSTAT      SET NEW STATUS                               
         MVC   WIOFXDDT,QSTADATE   COPY DATE AND TIME TO ELM                    
         MVC   WIOFXDTM,QSTATIME                                                
*                                                                               
         MVC   SVFAXELM,WIOFKEY    SAVE THIS FAX ELEMENT                        
*                                                                               
         GOTOR WRTELM,DMCB,WIOFKEY RE-WRITE FAX ELEMENT ELEMENT                 
         BZ    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        SAVE ERROR MESSAGE IF UNDELIVERED                                      
*                                                                               
         CLI   QSTAT,WIOSUDLQ      SKIP IF NOT UNDELIVERED                      
         BNE   EDCUNDX                                                          
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT WORKAREA                        
         LA    R6,ELEMENT          ESTABLISH AS FAX ELEMENT                     
         MVC   WIOFKEY,SVFAXELM    COPY CURRENT FAX KEY                         
         MVI   WIOFKFTP,WIOFKFEQ   SET AS ERROR MSG ELEMENT                     
*                                                                               
         MVC   WIOFFXEM(L'EZMEMSG),EZMEMSG SAVE ERROR MESSAGE                   
         LA    RF,WIOFFXEL+L'EZMEMSG  ELEMENT LENGTH                            
         STC   RF,WIOFKLEN         SET ELEMENT LENGTH                           
*                                                                               
         GOTOR ADDELM,DMCB,ELEMENT  ADD TO MINIO SET                            
*                                                                               
EDCUNDX  DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         RE-POINT TO ORIGINAL FAX ELM                 
*                                                                               
         MVC   QFXTYP,WIOFXTYP     SAVE FAX TYPE                                
*                                                                               
         CLI   WIOFXTYP,C'F'       SKIP IF ONLY FYI FAX#                        
         BE    EDCGRPX                                                          
*                                                                               
*        UPDATE STATUS IN GROUP HEADER                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH AS FAX ELEMENT                     
         USING WIOFAXD,R6                                                       
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY SEARCH FOR HEADER           '          
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET AS GROUP HEADER                          
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND GROUP HEADER ELEMENT                    
         BE    *+6                 ELEMENT FOUND                                
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         OC    WIOFDDTE,WIOFDDTE   IF DELIVERY STATUS ALREADY SET               
         BZ    *+12                                                             
         MVI   QFXTYP,C'F'            STOP UPDATING RECORD                      
         B     EDCGRPX                SKIP                                      
*                                                                               
         MVC   WIOFSTAT,QSTAT      UPDATE STATUS                                
         MVC   WIOFDDTE,QSTADATE   SET DELIVERED/UNDELIVERED DATE               
         MVC   WIOFDTIM,QSTATIME   SET DELIVERED/UNDELIVERED TIME               
*                                                                               
         GOTOR WRTELM,DMCB,WIOFKEY RE-WRITE GROUP HEADER ELM                    
         BZ    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
EDCGRPX  DS    0H                                                               
*                                                                               
*        UPDATE CURRENT STATUS IN HEADER                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH HEADER ELEMENT                     
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET RECORD ID                                
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ HEADER                                  
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   QPRD,WIOHPRD        SAVE PRODUCT CODE                            
         MVC   QPER,WIOHSTRT       SAVE PERIOD                                  
         MVC   QSTEW,WIOHSTEW      SAVE STEWARDSHIP STATUS                      
*                                                                               
         CLI   QFXTYP,C'F'         SKIP IF NO LONGER UPDATING RECORD            
         BE    EDCHDRX                                                          
*                                                                               
         CLI   WIOHSTAT,WIOSAPPQ   SKIP UPDATE IF ALREADY APPROVED              
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSREJQ   OR REJECTED                                  
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSACCQ   OR ACCESSED                                  
         BE    EDCHDRX                                                          
*                                                                               
         MVC   WIOHSTAT,QSTAT      UPDATE WITH LATEST STATUS                    
*                                                                               
         GOTOR WRTELM,DMCB,WIOHDRD RE-WRITE ELEMENT                             
*                                                                               
EDCHDRX  DS    0H                                                               
*                                                                               
*        ADD NEW STATUS ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH STATUS ELEMENT                     
         USING WIOSTATD,R6                                                      
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT ELEMENT SAVEAREA                        
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT ID                               
         MVI   WIOSKLEN,1          SEARCH ON ANY STATUS ELEMENT                 
*                                                                               
         GOTOR GETELM,DMCB,WIOSKEY FIND FIRST STATUS ELEMENT                    
         BNZ   EDCSTADN            NOT FOUND                                    
*                                                                               
EDCSTALP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   EDCSTADN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVSTAELM(0),WIOSKEY SAVE STATUS ELEMENT                          
*                                                                               
EDCSTACN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY FIND NEXT ELEMENT                            
*                                                                               
         B     EDCSTALP                                                         
*                                                                               
EDCSTADN DS    0H                                                               
*                                                                               
         LA    R6,SVSTAELM         POINT TO MOST RECENT STAT ELM                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSKSQN         GET LATEST SEQ NUMBER                        
*                                  MAYBE 0 IF NO STATUS ELMS FOUND              
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         LA    R6,ELEMENT          POINT TO NEW ELM BUILD AREA                  
*                                                                               
         MVI   WIOSKLEN,WIOSDRLQ   SET ELEMENT LENGTH                           
         STC   RF,WIOSKSQN         SET SEQUENCE NUMBER                          
*                                                                               
         MVC   WIOSSTAT,QSTAT      SET NEW STATUS                               
*                                  GET CURRENT DATE AND TIME                    
         MVC   WIOSDATE,QSTADATE   COPY DATE AND TIME TO ELM                    
         MVC   WIOSTIME,QSTATIME                                                
*                                                                               
         CLI   QFXTYP,C'F'         SKIP IF NO LONGER UPDATING RECORD            
         BE    EDCACTX                                                          
*                                                                               
         GOTOR ADDELM,DMCB,WIOSKEY ADD NEW STATUS ELEMENT                       
         CLI   MINERR,MINEDUP      OKAY IF A DUPLICATE ELM                      
         BE    *+14                NO ERRORS                                    
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH1,WIOAFGPC   FAX GROUP CHANGED                              
         OI    SVACH3,WIOASADD   STATUS ADDED                                   
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
EDCACTX  DS    0H                                                               
*                                                                               
*        SEND E-MAIL TO FAX SENDER                                              
*                                                                               
*        FIND E-MAIL ELEMENT                                                    
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH GENERIC FAX ELEMENT                
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX ELEMENT                           
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKEMQ   SET AS E-MAIL ELEMENT                        
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND E-MAIL ELEMENT                          
         BNZ   EDCEMLX             SKIP IF NOT FOUND                            
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   EMLTOADR,WIOFEML    SET E-MAIL ADDRESS                           
         OC    EMLTOADR,=CL60' '   BLANK FILL                                   
*                                                                               
         MVI   EMLTOEND,X'FF'      SET END OF TO ADDRESSES                      
*                                                                               
*        FORMAT SUBJECT LINE                                                    
*                                                                               
         CLI   QTYP,ESRKRCDQ       IF NOT ESR                                   
         BE    ECUEML05                                                         
*                                                                               
         MVC   EMLSUBJ,=CL70'Response to Fax of Insertion Order'                
         MVC   EMLSUBJ+36(L'PPEAIO#),PPEAIO#                                    
*                                                                               
                                                                                
         CLI   QSTEW,C'S'          IF STEWARDSHIP IO, CHANGE MSG                
         BNE   *+16                                                             
         MVC   EMLSUBJ,=CL70'Response to Fax of Stewardship Order'              
         MVC   EMLSUBJ+38(L'PPEAIO#),PPEAIO#                                    
*                                                                               
*        FORMAT E-MAIL MESSAGE                                                  
*                                                                               
         MVC   EMLLIN1,=CL80'Your Fax of Insertion Order'                       
         MVC   EMLLIN1+29(L'PPEAIO#),PPEAIO#                                    
         MVC   EMLLIN1+48(2),=C'to'                                             
*                                                                               
         CLI   QSTEW,C'S'          IF STEWARDSHIP IO, CHANGE MSG                
         BNE   *+22                                                             
         MVC   EMLLIN1,=CL80'Your Fax of Stewardship Order'                     
         MVC   EMLLIN1+31(L'PPEAIO#),PPEAIO#                                    
         MVC   EMLLIN1+50(2),=C'to'                                             
*                                                                               
         B     ECUEML07                                                         
*                                                                               
ECUEML05 DS    0H                                                               
*                                                                               
         MVC   EMLSUBJ,=CL70'Response to Fax of Space Reservation'              
*                                                                               
         MVC   EMLSUBJ+36(3),=C'SR-'                                            
         MVC   EMLSUBJ+39(L'PPEAIO#),PPEAIO#                                    
*                                                                               
         CLI   QSTEW,C'S'          IF STEWARDSHIP SR, CHANGE MSG                
         BNE   *+22                                                             
         MVC  EMLSUBJ,=CL70'Response to Fax of Stewardship Reservation'         
         MVC   EMLSUBJ+44(3),=C'SR-'                                            
         MVC   EMLSUBJ+47(L'PPEAIO#),PPEAIO#                                    
*                                                                               
*        FORMAT E-MAIL MESSAGE                                                  
*                                                                               
         MVC   EMLLIN1,=CL80'Your Fax of Space Reservation'                     
         MVC   EMLLIN1+29(3),=C'SR-'                                            
         MVC   EMLLIN1+32(L'PPEAIO#),PPEAIO#                                    
         MVC   EMLLIN1+51(2),=C'to'                                             
*                                                                               
         CLI   QSTEW,C'S'          IF STEWARDSHIP IO, CHANGE MSG                
         BNE   *+28                                                             
         MVC   EMLLIN1,=CL80'Your Fax of Stewardship Reservation'               
         MVC   EMLLIN1+37(3),=C'SR-'                                            
         MVC   EMLLIN1+40(L'PPEAIO#),PPEAIO#                                    
         MVC   EMLLIN1+59(2),=C'to'                                             
*                                                                               
ECUEML07 DS    0H                                                               
*                                                                               
*        DISPLAY PUBLICATION                                                    
*                                                                               
         MVC   EMLLIN2(80),=CL80'Publication'                                   
         MVC   EMLLIN2+16(L'PUBNM),PUBNM                                        
*                                                                               
         LA    R1,EMLLIN2+16+L'PUBNM-1 FIND LAST CHARACTER                      
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   2(R1),C'('                                                       
         LA    R0,3(R1)            SAVE POINTER                                 
         GOTOR DISPUB,DMCB,(R0)    DISPLAY PUB NUMBER                           
*                                                                               
         LR    R1,R0               RESTORE POINTER                              
*                                                                               
         LA    R1,16(R1)           FIND LAST CHARACTER                          
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   1(R1),C')'                                                       
*                                                                               
*        FAX NUMBER                                                             
*                                                                               
         LA    R6,SVFAXELM         POINT TO SAVED FAX ELEMENT                   
*                                                                               
         MVC   EMLLIN3(80),=CL80'Fax Number'                                    
*                                                                               
         LA    R3,WIOFX#+L'WIOFX#-1  COUNT DIGITS                               
         LA    R0,L'WIOFX#                                                      
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     ECUEML20            NO FAX NUMBER                                
*                                                                               
         CHI   R0,10               IF NOT EXACTLY 10 DIGITS                     
         BE    ECUEML10                                                         
*                                                                               
         LR    RF,R0                  COPY LENGTH                               
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EMLLIN3+16(0),WIOFX# DISPLAY FAX NUMBER                          
*                                                                               
         B     ECUEML20                                                         
*                                                                               
ECUEML10 DS    0H                  EXACTLY 10 DIGITS                            
*                                                                               
         LA    R1,EMLLIN3+16       FORMAT FAX NUMBER                            
*                                                                               
         MVI   0(R1),C'('                                                       
         MVC   1(3,R1),WIOFX#      AREA CODE                                    
         MVI   4(R1),C')'                                                       
         MVC   5(3,R1),WIOFX#+3    EXCHANGE                                     
         MVI   8(R1),C'-'                                                       
         MVC   9(4,R1),WIOFX#+6    NUMBER                                       
*                                                                               
ECUEML20 DS    0H                                                               
*                                                                               
         MVC   EMLLIN4,=CL80'For'                                               
*                                                                               
*        CLIENT                                                                 
*                                                                               
         MVC   EMLLIN5,=CL80'Client'                                            
*                                                                               
         GOTOR DISCLT,DMCB,(L'QCLT,QCLT),EMLLIN5+16  CLIENT CODE                
         OC    CLTNM,SPACES        REMOVE NULLS                                 
         MVC   EMLLIN5+20(L'CLTNM),CLTNM  SET CLIENT NAME                       
*                                                                               
*        PRODUCT                                                                
*                                                                               
         MVC   EMLLIN6,=CL80'Product'                                           
*                                                                               
         GOTOR DISPRD,DMCB,(L'QPRD,QPRD),EMLLIN6+16  PRODUCT CODE               
         OC    PRDNM,SPACES        REMOVE NULLS                                 
         MVC   EMLLIN6+20(L'PRDNM),PRDNM  SET PRODUCT NAME                      
*                                                                               
*        PERIOD                                                                 
*                                                                               
         MVC   EMLLIN7,=CL80'Period'                                            
*                                                                               
         GOTOR DISPER,DMCB,QPER,EMLLIN7+16                                      
*                                                                               
*        STATUS                                                                 
*                                                                               
         CLI   WIOFXSTA,C'D'       IF DELIVERED                                 
         BNE   ECUEML40                                                         
*                                                                               
         MVC   EMLLIN8,SPACES                                                   
         MVC   EMLLIN8(21),=C'has been delivered on'                            
         GOTOR VDATCON,DMCB,(3,WIOFXDDT),(17,EMLLIN8+23)                        
         MVC   EMLLIN8+32(2),=C'at'                                             
         GOTOR DISTIM,DMCB,WIOFXDTM,EMLLIN8+35                                  
*                                                                               
         B     ECUEML50                                                         
*                                                                               
ECUEML40 DS    0H                                                               
*                                                                               
         MVC   EMLLIN8(25),=C'has been cancelled due to'                        
         MVC   EMLLIN8+27(L'EZMEMSG),EZMEMSG   EDICT ERROR MESSAGE              
*                                                                               
ECUEML50 DS    0H                                                               
*                                                                               
         MVI   EMLLIN9,X'FF'       END OF MESSAGE                               
*                                                                               
         LA    R1,SMTPC            ESTABLISH EMAIL PARAMETER LIST               
         USING SMTPD,R1                                                         
*                                                                               
         XC    SMTPC,SMTPC         INIT PARAMETER BLOCK                         
*                                                                               
         LA    RF,EMLTOADR         SET TO ADDRESS                               
         ST    RF,SMTPTO                                                        
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          SET SUBJECT ADDRESS                          
         LA    RF,EMLMSG           E-MAIL ADDRESS                               
         ST    RF,SMTPDATA                                                      
*                                                                               
         GOTOR AJESMAIL,(R1)        SEND E-MAIL                                 
*                                                                               
*        UPDATE STATUS TO NOTIFIED                                              
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH AS STATUS ELEMENT                  
         USING WIOSKEY,R6                                                       
*                                                                               
*        FIND LATEST STATUS ELEMENT                                             
*                                                                               
         MVI   WIOSKLEN,1          LOOK FOR ANY STATUS ELEMENT                  
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT STATUS ELEMENT                          
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT                                              
         BNZ   EDCNTFDN                                                         
*                                                                               
EDCNTFLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   EDCNTFDN                                                         
*                                                                               
         MVC   SVSTAELM,WIOSKEY    SAVE STATUS ELEMENT                          
*                                                                               
EDCNTFCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY FIND NEXT STATUS ELEMENT                     
*                                                                               
         B     EDCNTFLP                                                         
*                                                                               
EDCNTFDN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSKSQN         BUMP SEQUENCE NUMBER                         
         AHI   RF,1                                                             
         LA    R6,ELEMENT          POINT TO NEW ELEMENT                         
         STC   RF,WIOSKSQN                                                      
*                                                                               
         MVI   WIOSKLEN,WIOSDRLQ   SET ELEMENT LENGTH                           
*                                                                               
         MVI   WIOSSTAT,WIOSNTFQ   SET EMAIL AS SENDER NOTIFIED                 
*                                                                               
         MVC   WIOSDATE,QSTADATE   SET DATE                                     
         MVC   WIOSTIME,QSTATIME   SET TIME                                     
*                                                                               
         GOTOR ADDELM,DMCB,WIOSKEY ADD NEW STATUS ELEMENT                       
*                                                                               
EDCEMLX  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)  CLOSE MINIO BLOCK            
         CLI   MINERR,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDICTX   DS    0H                                                               
*                                                                               
         CLC   SVMQION,=F'2'       SKIP IF NO TRACING                           
         BNE   EDCTR3                                                           
*                                                                               
*******  GOTOR VDMGR,DMCB,=C'OPMSG',=C'AUTONOTE*BOBY:EDICT DONE'                
*                                                                               
EDCTR3   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
EDTNFER  DS    0H                  RECORD NOT FOUND ERROR                       
         MVC   PERROR,=X'FE04'     SET ERROR CODE                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WEBIO'                           
***********************************************************************         
*                                                                     *         
*        HANDLE MQ MESSAGES FROM WEB SERVER                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WEBIO    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING EZMQMSGD,R9         ESTABLISH MQ REPLY                           
*                                                                               
         MVI   QTYP,WIOKRCDQ       ASSUME EIO MESSAGE                           
*                                                                               
         CLC   =C'ESR',EZMUSER     IF ESR                                       
         BNE   *+8                                                              
         MVI   QTYP,ESRKRCDQ          CHANGE TYPE                               
*                                                                               
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         MVC   QAGY,WBMQAGY        SAVE AGENCY ALPHA                            
*                                                                               
         GOTOR SETDMGR             SET DATAMANAGER TO CORRECT FILES             
*                                                                               
         GOTOR PRSIO#,DMCB,(L'WBMQIO#,WBMQIO#)   PARSE IO#                      
*                                                                               
         LHI   R0,L'WBMQPUB        MAX LENGTH OF PUB NUMBER                     
         LA    R1,WBMQPUB+L'WBMQPUB-1  POINT TO LAST POSITION                   
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-BLANK                          
         BH    *+12                                                             
         SHI   R1,1                BACK UP A POSITION                           
         BCT   R0,*-12                                                          
*                                  R0 HAS PUBCODE LENGTH                        
         GOTOR VALPUB,DMCB,((R0),WBMQPUB)   VALIDATE PUB                        
*                                                                               
         GOTOR MININIT             INITIALIZE MINIO BLOCK                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH MINIO MASTER KEY                   
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVC   WIOKRCD,QTYP        SET RECORD IDENTIFIER                        
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
         MVC   WIOKIO#,QIO#        SET IO#                                      
         MVC   WIOKRV#,QREV#       SET REVISION NUMBER                          
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINOPN',MINBLKD)  OPEN MINIO BLOCK             
         CLI   MINERR,0            CHECK FOR ERRORS                             
         BNE   WIONFER             RECORD NOT FOUND                             
*                                                                               
         LA    R3,WBMQDATA         POINT TO DATA BEING PASSED BY WEBIO          
*                                                                               
WIOLOOP  DS    0H                                                               
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         CLC   DATAID,=10C'0'      DONE AT END OF TBUFFER                       
         BE    WIODONE                                                          
*                                                                               
*        FIND ROUTINE TO HANDLE THIS DATA                                       
*                                                                               
         LA    R5,WBTAB            POINT TO ROUTINE TABLE                       
*                                                                               
WBIDLOOP DS    0H                                                               
*                                                                               
         CLI   0(R5),X'FF'         CHECK FOR END OF TABLE                       
         BE    WBIDDONE                                                         
*                                                                               
         CLC   DATAID,0(R5)        MATCH ON DATA ID                             
         BE    WBIDFD                                                           
*                                                                               
WBIDCONT DS    0H                                                               
*                                                                               
         LA    R5,WBTABL(R5)       BUMP TO NEXT TABLE ENTRY                     
         B     WBIDLOOP                                                         
*                                                                               
WBIDDONE DS    0H                                                               
*                                                                               
         B     WIOERR2             UNKNOWN DATA TYPE                            
*                                                                               
WBIDFD   DS    0H                                                               
*                                                                               
         L     RF,08(R5)           GET ROUTINE ADDRESS                          
         A     RF,RELO             RE-LOCATE ADDRESS                            
*                                                                               
         BASR  RE,RF               GO HANDLE DATA                               
*                                                                               
WIOCONT  DS    0H                                                               
*                                                                               
         USING SUBDD,R3                                                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   ERROR IF NOT END OF DATA                     
         BNE   WIOERR3                                                          
*                                                                               
         LA    R3,10(R3)           BUMP TO NEXT PIECE OF DATA                   
*                                                                               
         B     WIOLOOP                                                          
*                                                                               
WIODONE  DS    0H                                                               
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)  CLOSE MINIO SET              
*                                                                               
WEBIOX   DS    0H                                                               
*                                                                               
WEBIOTR3 DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
WIOERR2  DS    0H                  UNKNOWN DATA TYPE                            
*                                                                               
         GOTOR VMINIO,WIOPARMS,('MINCLS',MINBLKD)  CLOSE MINIO BLOCK            
         CLI   MINERR,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,3,=X'FE02'                                                    
         B     WIOERR                                                           
*                                                                               
WIOERR3  DS    0H                  SHOULD BE AT END OF DATA SEGEMENT            
         ICM   RF,3,=X'FE03'                                                    
         B     WIOERR                                                           
*                                                                               
WIONFER  DS    0H                  RECORD NOT FOUND ERROR                       
         ICM   RF,3,=X'FE04'       SET ERROR CODE                               
         B     WIOERR                                                           
*                                                                               
WIOERR   DS    0H                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE                            
         GOTOR ERREXIT             TAKE ERROR EXIT                              
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBTAB'                           
***********************************************************************         
*                                                                     *         
*        TABLE OF ROUTINES FOR HANDLING DATA FROM MQ                  *         
*        DC    CL15'DATAID',AL1(SPARE),AL4(ROUTINE)                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBTAB    DS    0D                                                               
         DC    CL06'STATUS',AL2(0),A(WBSTAT)   STATUS UPDATE                    
WBTABL   EQU   *-WBTAB             LENGTH OF TABLE ENTRY                        
         DC    CL06'CMNT  ',AL2(0),A(WBCMNT)                                    
         DC    CL06'RECPT ',AL2(0),A(WBFXEM)                                    
         DC    CL06'SENDER',AL2(0),A(WBSEND)                                    
         DC    CL06'URL   ',AL2(0),A(WBURL)                                     
         DC    CL06'RESPTM',AL2(0),A(WBRTM)                                     
         DC    XL1'FF'             EOT                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBSTAT'                          
***********************************************************************         
*                                                                     *         
*        ADD NEW STATUS TO INSERTION ORDER                            *         
*                                                                     *         
*NTRY    R3    POINTS TO CURRENT DATA IN T BUFFER                     *         
*                                                                     *         
*                                                                     *         
*EXIT    R3    HAS BEEN UPDATED                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBSTAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
*        STATUS UPDATE                                                          
*                                                                               
         LA    R3,DATADATA         POINT TO INCOMING DATA                       
         USING SUBDD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBSTATX                                                          
*                                                                               
         XC    QGRPID,QGRPID       INIT GROUP ID                                
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBSHISTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBSTATX                                                          
*                                                                               
WBSHISTN DS    0H                                                               
*                                                                               
         GOTOR VALSTA,DMCB,(6,SUBDDATA),0      VALIDATE THE STATUS              
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF SENT                                      
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RESENT                                    
         BNE   WBSDTE10                                                         
*                                                                               
         B     WBSTATX                IGNORE                                    
*                                                                               
*        VB IS ONLY SOURCE OF SENT/RESENT STATUS                                
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(25,QSTADATE)   USE MF DATE/TIME              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QSTATIME         ADJUST DDS HOURS TO REAL TIME                
         AHI   RF,6                                                             
         STC   RF,QSTATIME                                                      
*                                                                               
         B     WBSDTEX                                                          
*                                                                               
WBSDTE10 DS    0H                                                               
*                                                                               
         CLC   SUBDDATA+6(16),SPACES SKIP IF NO DATE GIVEN                      
         BNH   WBSDTEX                                                          
*                                                                               
         GOTOR VDATCON,DMCB,(10,SUBDDATA+6),(3,QSTADATE)  CONVERT DATE          
*                                                                               
         PACK  DUB,SUBDDATA+17(2)  HOURS                                        
         TP    DUB                 SKIP IF NOT PACKED                           
         BNZ   WBSTIMX                                                          
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,QSTATIME                                                      
*                                                                               
         PACK  DUB,SUBDDATA+20(2)  MINUTES                                      
         TP    DUB                 SKIP IF NOT PACKED                           
         BNZ   WBSTIMX                                                          
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,QSTATIME+1                                                    
*                                                                               
WBSTIMX  DS    0H                                                               
*                                                                               
         MVI   QSTATIME+2,0        NO SECONDS                                   
*                                                                               
WBSDTEX  DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBSVALX                                                          
*                                                                               
*        RETRIEVE IP ADDRESS OF USERIDS                                         
*                                                                               
         CLC   =CL10'IPADDR',SUBDID   CHECK FOR IPADDR                          
         BNE   WBSIPIDN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QIPADDR(0),SUBDDATA       SAVE IP ADDRESS                        
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
WBSIPIDX DS    0H                                                               
*                                                                               
         B     WBSSGNN                                                          
*                                                                               
WBSIPIDN DS    0H                                                               
*                                                                               
         CLC   =CL10'SIGNON',SUBDID   CHECK FOR SIGN ON                         
         BNE   WBSSGNN                                                          
*                                                                               
         MVC   QUSERID,SUBDDATA    SAVE USER   ID                               
         MVC   QPERSID,SUBDDATA+10 SAVE PERSON ID                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT SUBDATA                         
*                                                                               
WBSSGNN  DS    0H                                                               
*                                                                               
WBSVALX  DS    0H                                                               
*                                                                               
*        SET STATUS AS THE CURRENT STATUS OF THE IO/SR                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOHDRD,R6          ESTABLISH HEADER ELEMENT KEY                 
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET ELEMENT ID                               
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY FIND HEADER ELEMENT                          
         BNZ   WBSTATX             NOT FOUND                                    
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        ANALYZE HIERARCHY OF STATUSES                                          
*                                                                               
         CLC   WIOHSTAT,QSTAT      SKIP IF NO CHANGE IN STATUS                  
         BE    WBSTATX                AB AND WS UPDATING AT SAME TIME           
*                                                                               
         CLI   WIOHSTAT,WIOSAPPQ   STATUS APPROVED                              
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSREJQ   OR REJECTED                                  
         BE    WBSHDRX                NOT REPLACED                              
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF INCOMING STATUS IS SENT                   
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RESENT                                    
         BNE   WBSHDR10                                                         
*                                                                               
         CLI   WIOHSTAT,WIOSSNTQ      IF CURRENTLY SENT                         
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSUDLQ      OR UNDELIVERED                            
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSGENQ      OR GENERATED                              
         BE    *+8                                                              
         CLI   WIOHSTAT,WIOSRSTQ      OR RESENT                                 
         BE    WBSHDR10                  UPDATE STATUS                          
*                                                                               
         B     WBSHDRX             ELSE NO UPDATE                               
*                                                                               
WBSHDR10 DS    0H                                                               
*                                                                               
         MVC   WIOHSTAT,QSTAT      UPDATE CURRENT IO STATUS                     
*                                                                               
         GOTOR WRTELM,DMCB,WIOHKEY RE-WRITE HEADER ELEMENT                      
         BZ    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
WBSHDRX  DS    0H                                                               
*                                                                               
*        ADD NEW STATUS ELEMENT                                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOSTATD,R6         ESTABLISH STATUS ELEMENT KEY                 
*                                                                               
         XC    SVSTAELM,SVSTAELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND CURRENT STATUS ELEMENT                                            
*                                                                               
         MVI   WIOSKCDE,WIOSKIDQ   SET ELEMENT ID                               
         MVI   WIOSKLEN,1          SEARCH ON ANY STATUS ELEMENT                 
*                                                                               
         GOTOR GETELM,DMCB,WIOSKEY FIND FIRST STATUS ELEMENT                    
         BNZ   WBSTADN             NOT FOUND                                    
*                                                                               
WBSTALP DS     0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOSKCDE,WIOSKIDQ   DONE IF NOT A STATUS ELEMENT                 
         BNE   WBSTADN                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVSTAELM(0),WIOSKEY SAVE STATUS ELEMENT                          
*                                                                               
WBSTACN DS     0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOSKEY FIND NEXT ELEMENT                            
*                                                                               
         B     WBSTALP                                                          
*                                                                               
WBSTADN DS     0H                                                               
*                                                                               
         LA    R6,SVSTAELM         POINT TO MOST RECENT STAT ELM                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOSKSQN         GET LATEST SEQ NUMBER                        
*                                  MAYBE 0 IF NO STATUS ELMS FOUND              
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         LA    R6,ELEMENT          POINT TO NEW ELM BUILD AREA                  
*                                                                               
         MVI   WIOSKLEN,WIOSDRLQ   SET ELEMENT LENGTH                           
         STC   RF,WIOSKSQN         SET SEQUENCE NUMBER                          
*                                                                               
         MVC   WIOSSTAT,QSTAT      SET NEW STATUS                               
         MVC   WIOSDATE,QSTADATE   COPY DATE AND TIME TO ELM                    
         MVC   WIOSTIME,QSTATIME                                                
*                                                                               
         LA    RF,WIOSDRLQ         BASIC ELEMENT LENGTH                         
*                                                                               
         OC    QIPADDR,QIPADDR     IF THERE IS AND IP ADDRESS                   
         BZ    WBSTA10                                                          
*                                                                               
         MVI   WIOSTRLR,WIOSTRIQ   IDENTIFY TRAILING DATA                       
         MVC   WIOSIPAD,QIPADDR    PUT IN ELEMENT                               
         AHI   RF,L'WIOSIPAD       ADJUST ELEMENT LENGTH                        
*                                                                               
         B     WBSTA30                                                          
*                                                                               
WBSTA10  DS    0H                                                               
*                                                                               
         OC    QUSERID,QUSERID     IF THERE IS A USER ID                        
         BZ    WBSTA30                                                          
*                                                                               
         MVI   WIOSTRLR,WIOSTRSQ      IDENTIFY TRAILING DATA                    
         MVC   WIOSUSR,QUSERID        PUT IN ELEMENT                            
         MVC   WIOSPERS,QPERSID       PUT IN ELEMENT                            
         AHI   RF,L'WIOSUSR+L'WIOSPERS   ADJUST ELEMENT LENGTH                  
*                                                                               
WBSTA30  DS    0H                                                               
*                                                                               
         STC   RF,WIOSKLEN         RESET ELEMENT LENGTH                         
*                                                                               
         GOTOR ADDELM,DMCB,WIOSKEY ADD NEW STATUS ELEMENT                       
         CLI   MINERR,MINEDUP      OKAY IF A DUPLICATE ELM                      
         BE    *+14                NO ERRORS                                    
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        UPDATE/ADD GROUP HEADER ELEMENT                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX/EMAIL HEADER KEY               
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND CURRENT FAX/EMAIL HEADER ELEMENT                                  
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKLEN,1          SEARCH ON ANY FAX/EMAIL ELEMENT              
*                                                                               
         MVI   HALF,X'FF'          INIT WORK AREA                               
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   WBSFAXDN            NOT FOUND                                    
*                                                                               
WBSFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   WBSFAXDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   IF NOT A HEADER ELEMENT                      
         BE    *+12                                                             
         MVI   HALF,X'FF'             SET SWITCH                                
         B     WBSFAXCN               AND SKIP                                  
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    WBSFAX10                                                         
*                                                                               
         CLC   WIOFGPID,QGRPID     IF STATUS IS FOR THIS GROUP                  
         BNE   *+12                                                             
         MVI   HALF,0                 NOT A NEW GROUP                           
         B     WBSFAXFD               USE THIS HEADER                           
*                                                                               
WBSFAX10 DS    0H                                                               
*                                                                               
         MVI   HALF,0              RE-SET SWITCH                                
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY SAVE FAX/EMAIL ELEMENT                       
*                                                                               
WBSFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     WBSFAXLP                                                         
*                                                                               
WBSFAXDN DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF STATUS SENT                               
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RE-SENT                                   
         BNE   WBSFAX01                                                         
*                                                                               
         CLI   HALF,X'FF'             IF SWITCH ON                              
         BE    WBSFAX05                  CREATE NEW GROUP                       
*                                                                               
WBSFAX01 DS    0H                                                               
*                                                                               
         OC    SVFAXELM,SVFAXELM   SKIP IF NO FAX ELEMENTS                      
         BZ    WBSFAXX                                                          
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY RE-READ ELEMENT                              
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         B     WBSFAX07                                                         
*                                                                               
WBSFAX05 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKGP#         GET LATEST GROUP NUMBER                      
*                                  MAYBE 0 IF NO FAX/EMAIL ELMS FOUND           
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         LA    R6,ELEMENT          POINT TO NEW ELM BUILD AREA                  
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX/EMAIL ELEMENT                     
         MVI   WIOFKLEN,WIOFHDRL   SET ELEMENT LENGTH                           
         STC   RF,WIOFKGP#         SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET GROUP HEADER ID                          
*                                                                               
         MVC   WIOFGPID,QGRPID     SET GROUP ID IN HEADER                       
*                                                                               
WBSFAX07 DS    0H                                                               
*                                                                               
WBSFAXFD DS    0H                                                               
*                                                                               
         CLI   WIOFSTAT,WIOSSNTQ   IF SENT                                      
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSRSTQ   OR RE-SENT                                   
         BNE   WBSFAX15                                                         
*                                                                               
         CLI   HALF,X'FF'          SKIP IF NOT NEW GROUP                        
         BNE   WBSFAX20                                                         
*                                  ELSE                                         
         MVC   WIOFSTAT,QSTAT         SET NEW STATUS                            
         GOTOR VDATCON,DMCB,(5,0),(25,WIOFSDTE) SET SENT DAY/TIME               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFSTIM         ADJUST DDS HOURS TO REAL TIME                
         AHI   RF,6                                                             
         STC   RF,WIOFSTIM                                                      
*                                                                               
         B     WBSFAX20                                                         
*                                                                               
WBSFAX15 DS    0H                                                               
*                                                                               
         MVC   WIOFSTAT,QSTAT      SET NEW STATUS                               
*                                                                               
         CLI   WIOFSTAT,WIOSDLVQ   IF DELIVERED                                 
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSUDLQ   OR UNDELIVERD                                
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSAPPQ   OR APPROVED                                  
         BE    *+8                                                              
         CLI   WIOFSTAT,WIOSREJQ   OR REJECTED                                  
         BNE   WBSFAX20                                                         
*                                                                               
         MVC   WIOFDDTE,QSTADATE      SET DATE AND TIME                         
         MVC   WIOFDTIM,QSTATIME                                                
*                                                                               
WBSFAX20 DS    0H                                                               
*                                                                               
         L     RF,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         CLI   QSTAT,WIOSSNTQ      IF STATUS SENT                               
         BE    *+8                                                              
         CLI   QSTAT,WIOSRSTQ      OR RE-SENT                                   
         BNE   WBSFAX22                                                         
*                                                                               
         MVC   WIOFSDTE,QSTADATE      SET DATE AND TIME                         
         MVC   WIOFSTIM,QSTATIME                                                
*                                                                               
         CLI   HALF,X'FF'          SKIP IF NOT NEW GROUP                        
         BNE   WBSFAX22                                                         
*                                                                               
         L     RF,ADDELM              ADD THE NEW ELEMENT                       
*                                                                               
WBSFAX22 DS    0H                                                               
*                                                                               
         GOTOR (RF),DMCB,WIOFKEY   PUT ELEMENT IN MINIO SET                     
         CLI   MINERR,MINEDUP      OKAY IF A DUPLICATE ELM                      
         BE    *+14                NO ERRORS                                    
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
WBSFAXX  DS    0H                                                               
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH3,WIOASADD   STATUS ADDED                                   
         OI    SVACH3,WIOAFGPC   FAX GROUP CHANGED                              
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBSTATX  DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBSTATXL DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBSTATXD                                                         
*                                                                               
WBSTATXC DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBSTATXL                                                         
*                                                                               
WBSTATXD DS    0H                                                               
*                                                                               
         XIT1  REGS=(R3)           PRESERVE R3                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBFXEM'                          
***********************************************************************         
*                                                                     *         
*        ADD FAX/EMAIL RECIPIENTS' ADDRESES TO FILE                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBFXEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         LA    R3,DATADATA         POINT TO INCOMING DATA                       
         USING SUBDD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBFXEMX                                                          
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBFXHSTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBFXEMX                                                          
*                                                                               
         CLC   SUBDID(4),=4X'00'   DONE AT END OF DATA                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
WBFXHSTN DS    0H                                                               
*                                                                               
*        FIND LATEST GROUP NUMBER                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX/EMAIL HEADER KEY               
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND LATEST FAX/EMAIL HEADER ELEMENT                                   
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKLEN,1          SEARCH ON ANY FAX/EMAIL ELEMENT              
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   WBFFAXDN            NOT FOUND                                    
*                                                                               
WBFFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   WBFFAXDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP IF NOT A HEADER ELEMENT                 
         BNE   WBFFAXCN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN            GET ELEMENT LENGTH                        
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY    SAVE FAX/EMAIL HEADER                     
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    WBFFAXCN                                                         
*                                                                               
         CLC   WIOFGPID,QGRPID        OKAY IF DATA FOR THIS GROUP               
         BE    WBFFAXFD                                                         
*                                                                               
WBFFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     WBFFAXLP                                                         
*                                                                               
WBFFAXDN DS    0H                  FAX GROUP NOT FOUND                          
*                                                                               
         OC    SVFAXELM,SVFAXELM   IF THERE IS AT LEAST ONE FAX HEADER          
         BZ    *+14                                                             
         OC    QGRPID,QGRPID          USE IT IF THERE IS NO GROUP ID            
         BZ    WBFFAXFD                                                         
*                                                                               
*        CREATE NEW GROUP                                                       
*                                                                               
         LA    R6,SVFAXELM         POINT TO LAST HEADER FOUND                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKGP#         GET LATEST GROUP NUMBER                      
*                                  MAYBE 0 IF NO FAX/EMAIL ELMS FOUND           
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         XC    SVFAXELM,SVFAXELM   CLEAR WORKAREA                               
*                                                                               
*        BUILD NEW FAX HEADER ELEMENT                                           
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX/EMAIL ELEMENT                     
         MVI   WIOFKLEN,WIOFHDRL   SET ELEMENT LENGTH                           
         STC   RF,WIOFKGP#         SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET GROUP HEADER ID                          
*                                                                               
         MVC   WIOFGPID,QGRPID     SET GROUP ID IN HEADER                       
*                                                                               
         GOTOR ADDELM,DMCB,WIOFAXD ADD ELEMENT TO RECORD                        
*                                                                               
WBFFAXFD DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         MVC   QFGP#,WIOFKGP#      SAVE LATEST GROUP NUMBER                     
         MVC   QSTAT,WIOFSTAT      SAVE HEADER STATUS                           
*                                                                               
         LA    R5,WBFX#TAB         POINT TO FAX # TABLE                         
         XC    0(L'WIOFX#,R5),0(R5) INIT FIRST ENTRY                            
*                                                                               
*        FIND NEXT AVAILABLE SQN IN GROUP                                       
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKFXQ   SET FOR FAX/EMAIL ADDRESSES                  
         MVI   WIOFKLEN,WIOFKTYP-WIOFKEY  MATCH ON GROUP                        
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   WBFSQNDN            NOT FOUND                                    
*                                                                               
WBFSQNLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   WBFSQNDN                                                         
*                                                                               
         CLC   WIOFKGP#,QFGP#      DONE IF NOT FOR GROUP                        
         BNE   WBFSQNDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKFXQ   SKIP IF NOT AN ADDRESS ELM                   
         BNE   WBFSQNCN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY SAVE FAX/EMAIL ELEMENT                       
*                                                                               
         OC    WIOFX#,WIOFX#       IF A FAX NUMBER                              
         BZ    WBFSQN10                                                         
*                                                                               
*        REDUCE FAX# TO ALL DIGITS AND SAVE IN TABLE                            
*                                                                               
         GOTOR DGTFX#,DMCB,(L'WIOFX#,WIOFX#),(L'WIOFX#,0(R5))                   
*                                                                               
         LA    R5,L'WIOFX#(R5)     BUMP TO NEXT ENTRY IN TABLE                  
         XC    0(L'WIOFX#,R5),0(R5) INIT THIS NEXT ENTRY                        
*                                                                               
WBFSQN10 DS    0H                                                               
*                                                                               
WBFSQNCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     WBFSQNLP                                                         
*                                                                               
WBFSQNDN DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO LAST FAX/EMAIL ELEMENT              
         MVC   QFSQN,WIOFKSQN      SAVE LATEST SEQUENCE NUMBER                  
*                                                                               
*        ADD ADDRESS ELEMENT FOR CURRENT INPUT                                  
*                                                                               
WBFDATLP DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBFDATDN                                                         
*                                                                               
*        BUILD NEW FAX/EMAIL ELEMENT                                            
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT BUILD AREA                      
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH AS FAX ELEMENT                     
         MVI   WIOFKCDE,WIOFKIDQ   ESTABLISH AS FAX/EMAIL ELEMENT               
         MVI   WIOFKLEN,WIOFFAXL   SET BASIC ELEMENT ADDRESS                    
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKFXQ   SET AS ADDRESS ELEMENT                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QFSQN            GET LATEST SEQ. #                            
         AHI   RF,1                BUMP TO NEXT AVAILBLE SQN                    
         STC   RF,QFSQN            SET NEW SQN                                  
         STC   RF,WIOFKSQN         SET NEW SQN                                  
*                                                                               
*        ANALYZE ADDRESS TYPE                                                   
*                                                                               
         CLC   SUBDID,=CL10'TYPE'  CHECK THAT DATA IS TYPE                      
         BNE   WBFDTYPN                                                         
*                                                                               
         MVC   WIOFXTP1,SUBDDATA   SAVE 'F' OR 'E' AS TYPE                      
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENEGTH                             
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
WBFDTYPN DS    0H                                                               
*                                                                               
*        ANALYZE ADDRESS DATA                                                   
*                                                                               
         CLC   SUBDID,=CL10'FYI'   ID DATA IS AGENCY FYI                        
         BNE   *+16                                                             
         MVI   WIOFXTYP,C'F'          SET AS FYI ADDRESS                        
         MVI   WIOFXAGV,C'A'          SET AS AGENCY ADDRESS                     
         B     WBFDADR1                                                         
*                                                                               
         CLC   SUBDID,=CL10'VFYI'  ID DATA IS VENDOR FYI                        
         BNE   *+16                                                             
         MVI   WIOFXTYP,C'F'          SET AS FYI ADDRESS                        
         MVI   WIOFXAGV,C'V'          SET AS VENDOR ADDRESS                     
         B     WBFDADR1                                                         
*                                                                               
         CLC   SUBDID,=CL10'VENDOR' OKAY IF VENDOR ADDRESS                      
         BNE   *+12                                                             
         MVI   WIOFXAGV,C'V'          SET AS VENDOR ADDRESS                     
         B     WBFDADR1                                                         
*                                                                               
         B     WBFDADRN            NO ADDRESS AVAILABLE                         
*                                                                               
WBFDADR1 DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R1,WIOFFEML         ASSUME E-MAIL ADDRESS                        
*                                                                               
         CLI   WIOFXTP1,C'E'       OKAY IF E-MAIL                               
         BE    *+8                                                              
         LA    R1,WIOFX#           ELSE FAX NUMBER                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SUBDDATA    SAVE FAX # OR E-MAIL ADDRESS                 
*                                                                               
         CLI   WIOFXTP1,C'E'       IF E-MAIL                                    
         BNE   WBFDADR2                                                         
*                                                                               
         SR    RE,RE               ADJUST ELEMENT LENGTH                        
         IC    RE,WIOFKLEN                                                      
         LA    RE,1(RF,RE)                                                      
         STC   RE,WIOFKLEN                                                      
*                                                                               
WBFDADR2 DS    0H                                                               
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT DATA ELEMENT                    
*                                                                               
WBFDADRN DS    0H                                                               
*                                                                               
*        SAVE ADDRESSEE'S NAME IF PRESENT                                       
*                                                                               
         CLC   SUBDID,=CL10'NAME'   SKIP IF NO NAME PRESENT                     
         BNE   WBFDNAMN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LR    RE,RF               SAVE REAL DATA LENGTH                        
*                                                                               
         CHI   RF,L'WIOFXNAM       MAKE SURE NAME NOT TOO LONG                  
         BNH   *+8                                                              
         LHI   RF,L'WIOFXNAM                                                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFXNAM(0),SUBDDATA                                             
*                                                                               
         LA    R3,SUBDDATA(RE)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
WBFDNAMN DS    0H                                                               
*                                                                               
*        HANDLE STYLE DATA TYPE                                                 
*                                                                               
         CLC   SUBDID,=CL10'STYLE'  SKIP IF NO STYLE PRESENT                    
         BNE   WBFDSTLN                                                         
*                                                                               
         CLC   =C'WCOST',SUBDDATA  FIND VALUE FOR INDICATOR                     
         BNE   *+12                                                             
         MVI   WIOFXSCS,0          WITH COSTS                                   
         B     WBFDSTLX                                                         
*                                                                               
         CLC   =C'WOCOST',SUBDDATA  FIND VALUE FOR INDICATOR                    
         BNE   *+12                                                             
         MVI   WIOFXSCS,C'Y'      COST SUPPRESSED                               
         B     WBFDSTLX                                                         
*                                                                               
WBFDSTLX DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBFDATCN                                                         
*                                                                               
WBFDSTLN DS    0H                                                               
*                                                                               
*        UNKNOWN DATATYPE                                                       
*                                                                               
         B     WBFDATDN                                                         
*                                                                               
WBFDATCN DS    0H                                                               
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(25,QSTADATE)   USE MF DATE/TIME              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QSTATIME         ADJUST DDS HOURS TO REAL TIME                
         AHI   RF,6                                                             
         STC   RF,QSTATIME                                                      
*                                                                               
         MVC   WIOFXFDT,QSTADATE   SET FAX DATE AND TIME                        
         MVC   WIOFXFTM,QSTATIME                                                
         MVC   WIOFXSTA,QSTAT      COPY GROUP HEADER'S STATUS                   
*                                                                               
         OC    WIOFX#,WIOFX#       IF THIS IS A FAX NUMBER                      
         BZ    WBFDAT10                                                         
*                                                                               
*        REDUCE FAX NUMBER TO DIGITS                                            
*                                                                               
         GOTOR DGTFX#,DMCB,(L'WIOFX#,WIOFX#),(L'FX#FILE,FX#FILE)                
         MVC   WIOFX#,FX#FILE                                                   
*                                                                               
         LA    R5,WBFX#TAB            SKIP IF ALREADY IN GROUP                  
*                                                                               
WBFCHKLP DS    0H                                                               
*                                                                               
         OC    0(L'WIOFX#,R5),0(R5) DONE AT END OF TABLE                        
         BZ    WBFCHKDN                                                         
*                                                                               
         CLC   WIOFX#,0(R5)           MATCH FAX NUMBERS                         
         BE    WBFCHKFD                                                         
*                                                                               
WBFCHKCN DS    0H                                                               
*                                                                               
         LA    R5,L'WIOFX#(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     WBFCHKLP                                                         
*                                                                               
WBFCHKFD DS    0H                  FAX NUMBER IN TABLE                          
*                                  MEANS WE ALREADY HAVE THIS                   
*                                  FAX ON FILE                                  
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QFSQN            GET LATEST SEQ. #                            
         SHI   RF,1                CANCEL BUMP UP FOR THIS ELEMENT              
         STC   RF,QFSQN            SET NEW SQN                                  
*                                                                               
         B     WBFDAT20            SKIP ADDING ELEMENT                          
*                                                                               
WBFCHKDN DS    0H                                                               
*                                                                               
WBFDAT10 DS    0H                                                               
*                                                                               
         GOTOR ADDELM,DMCB,WIOFAXD ADD ELEMENT TO RECORD                        
*                                                                               
WBFDAT20 DS    0H                                                               
*                                                                               
         B     WBFDATLP            GO PROCESS NEXT ADDRESS                      
*                                                                               
WBFDATDN DS    0H                                                               
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH3,WIOAFADD   ADDRESS ADDED                                  
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBFXEMX  DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBFXEMXL DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBFXEMXD                                                         
*                                                                               
WBFXEMXC DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBFXEMXL                                                         
*                                                                               
WBFXEMXD DS    0H                                                               
*                                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
WBFX#TAB DS    30XL(L'WIOFX#)      TABLE FOR FAX NUMBERS                        
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBCMNT'                          
***********************************************************************         
*                                                                     *         
*        ADD VARIOUS COMMENTS TO THE FILE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBCMNT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         LA    R3,DATADATA         POINT TO INCOMING DATA                       
         USING SUBDD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBCMNTX                                                          
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBCMHSTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBCMNTX                                                          
*                                                                               
WBCMHSTN DS    0H                                                               
*                                                                               
*        FIND LATEST GROUP NUMBER                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   SVNULLS,0           FORCE AND ENDING NULL FOR COMMENTS           
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX/EMAIL HEADER KEY               
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND LATEST FAX/EMAIL HEADER ELEMENT                                   
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKLEN,1          SEARCH ON ANY FAX/EMAIL ELEMENT              
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   WBCFAXDN            NOT FOUND                                    
*                                                                               
WBCFAXLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   WBCFAXDN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP IF NOT A HEADER ELEMENT                 
         BNE   WBCFAXCN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY SAVE FAX/EMAIL ELEMENT                       
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BNE   WBCFAX10                                                         
*                                                                               
         CLC   WIOFGPID,QGRPID     DONE IF GROUP IDS MATCH                      
         BE    WBCFAXDN                                                         
*                                                                               
WBCFAX10 DS    0H                                                               
*                                                                               
WBCFAXCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOFKEY FIND NEXT ELEMENT                            
*                                                                               
         B     WBCFAXLP                                                         
*                                                                               
WBCFAXDN DS    0H                                                               
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         MVC   QFGP#,WIOFKGP#      SAVE LATEST GROUP NUMBER                     
*                                                                               
*        ADD COMMENT ELEMENT FOR CURRENT INPUT                                  
*                                                                               
         XC    ELEMENT,ELEMENT     INIT ELEMENT BUILD AREA                      
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH AS FAX ELEMENT                     
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   ESTABLISH AS FAX/EMAIL ELEMENT               
         MVI   WIOFKLEN,WIOFCOML   SET BASIC ELEMENT ADDRESS                    
         MVC   WIOFKGP#,QFGP#      SET GROUP NUMBER                             
*                                                                               
WBCDATLP DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBCDATDN                                                         
*                                                                               
*        ANALYZE COMMENT TYPE                                                   
*                                                                               
         CLC   SUBDID,=CL10'EMAIL' EMAIL COMMENTS                               
         BNE   *+12                                                             
         MVI   WIOFKTYP,WIOFKECQ   SET AS EMAIL COMMENTS ELM                    
         B     WBCTYPX                                                          
*                                                                               
         CLC   SUBDID,=CL10'VENDOR'  EMAIL COMMENTS                             
         BNE   *+12                                                             
         MVI   WIOFKTYP,WIOFKVCQ   SET AS VENDOR COMMENTS ELM                   
         B     WBCTYPX                                                          
*                                                                               
         B     WBCMNTX             UNKNOWN COMMENTS                             
*                                                                               
WBCTYPX  DS    0H                                                               
*                                                                               
*        ADD COMMENTS TO ELEMENT                                                
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R2,SUBDDATA         START OF COMMENT INPUT                       
         LR    R0,RF               SAVE COMMENT LENGTH                          
*                                                                               
*        STORE COMMENTS IN 239 CHARACTER CHUNKS                                 
*                                                                               
WBCCOMLP DS    0H                                                               
*                                                                               
         CHI   RF,239              IF COMMENT TO LARGE FOR ONE ELM              
         BNH   *+8                                                              
         LHI   RF,239                 DEFAULT TO ONE ELM MAX                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFCOMM(0),0(R2)   SAVE COMMENT                                 
*                                                                               
         LA    RE,WIOFCOMM+1(RF)   POINT TO END OF ELEMENT                      
         MVI   0(RE),0             FORCE NULLS FOR MINIO                        
*                                                                               
         LA    RE,1+WIOFCOML(RF)   NEW LENGTH                                   
         STC   RE,WIOFKLEN         UPDATE ELM LENGTH                            
*                                                                               
         IC    RE,WIOFKSQN         BUMP SEQUENCE NUMBER                         
         AHI   RE,1                                                             
         STC   RE,WIOFKSQN                                                      
*                                                                               
         LA    R2,1(RF,R2)         BUMP TO NEXT PART OF COMMENT                 
         SR    R0,RF               DECREMENT LENGTH COUNTER                     
         BCTR  R0,0                                                             
*                                                                               
         GOTOR ADDELM,DMCB,WIOFKEY ADD ELEMENT TO RECORD                        
*                                                                               
WBCCOMCN DS    0H                                                               
*                                                                               
         LTR   RF,R0               GET REMAINING LENGTH OF COMMENT              
         BP    WBCCOMLP            MORE COMMENT TO STORE                        
*                                                                               
WBCCOMDN DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R3,SUBDDATA(RF)     NEXT ELEMENT LENGTH                          
*                                                                               
         B     WBCDATLP                                                         
*                                                                               
WBCDATDN DS    0H                                                               
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBCMNTX  DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBCMNTXL DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBCMNTXD                                                         
*                                                                               
WBCMNTXC DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBCMNTXL                                                         
*                                                                               
WBCMNTXD DS    0H                                                               
*                                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBSEND'                          
***********************************************************************         
*                                                                     *         
*        ADD NEW SENDER'S E-MAIL ADDRESS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBSEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         LA    R3,DATADATA         POINT TO INCOMING DATA                       
         USING SUBDD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBSENDX                                                          
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBSDHSTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
WBSDHSTN DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBSENDX                                                          
*                                                                               
         CLC   =C'SENDER',SUBDID   SKIP IF NOT A SEND EMAIL                     
         BNE   WBSENDX                                                          
*                                                                               
         GOTOR FNDGP#              FIND CURRENT GROUP NUMBER                    
*                                                                               
*        BUILD EMAIL ELEMENT                                                    
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          POINT TO NEW ELM BUILD AREA                  
         USING WIOFAXD,R6          ESTABLISH AS FAX ELEEMENT                    
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX ELEMENT                           
         MVI   WIOFKLEN,WIOFEMLL   SET BASIC ELEMENT LENGTH                     
         MVC   WIOFKGP#,QFGP#      SET FAX GROUP NUMBER                         
         MVI   WIOFKTYP,WIOFKEMQ   SET AS E-MAIL ELEMENT                        
*                                                                               
         SR    RF,RF                                                            
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LTR   RF,RF               MUST HAVE A LENGTH                           
         BZ    WBSENDX                                                          
*                                                                               
         BCT   RF,*+8              DECREMENT FOR EXECUTE                        
         B     *+8                 NO DATA AVAILABLE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOFEML(0),SUBDDATA SAVE E-MAIL ADDRESS                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,WIOFKLEN         ADJUST ELEMENT LENGTH                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,WIOFKLEN                                                      
*                                                                               
         GOTOR ADDELM,DMCB,WIOFKEY ADD NEW STATUS ELEMENT                       
         CLI   MINERR,MINEDUP      OKAY IF A DUPLICATE ELM                      
         BE    *+14                NO ERRORS                                    
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH2,WIOAFEAD   SENDER'S E-MAIL ADDRESS                        
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBSENDX  DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBSENDXL DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBSENDXD                                                         
*                                                                               
WBSENDXC DS    0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBSENDXL                                                         
*                                                                               
WBSENDXD DS    0H                                                               
*                                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBURL'                           
***********************************************************************         
*                                                                     *         
*        ADD URL DATA   TO INSERTION ORDER                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBURL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
*        BUILD SKELETON URL ELEMENT                                             
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOURLD,R6          ESTABLISH FAX/EMAIL ELEMENT                  
*                                                                               
*        FIND NEXT AVAILABLE SEQUENCE NUMBER                                    
*                                                                               
         MVI   WIOUKCDE,WIOUKIDQ   SET ELEMENT ID                               
         MVI   WIOUKLEN,1          SET FOR ANY URL ELEMENT                      
*                                                                               
         XC    SVURLELM,SVURLELM   INIT SAVE AREA                               
*                                                                               
         GOTOR GETELM,DMCB,WIOUKEY FIND FIRST URL ELEMENT                       
         BNZ   WBUSQNDN            NONE FOUND                                   
*                                                                               
WBUSQNLP DS    0H                                                               
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOUKCDE,WIOUKIDQ   DONE IF NOT A URL ELEMENT                    
         BNE   WBUSQNDN                                                         
*                                                                               
         MVC   SVURLELM,0(R6)      SAVE ELEMENT                                 
*                                                                               
WBUSQNCN DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,WIOUKEY FIND NEXT URL ELM                            
*                                                                               
         B     WBUSQNLP                                                         
*                                                                               
WBUSQNDN DS    0H                  END OF URL ELEMENTS                          
*                                                                               
         LA    R6,SVURLELM         POINT TO LAST FOUND URL ELEMENT              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOUKSQN         BUMP SEQUENCE NUMBER                         
         AHI   RF,1                                                             
         LA    R6,ELEMENT          POINT TO URL ELMENT BUILDAREA                
         STC   RF,WIOUKSQN         SET NEW SEQUENCE NUMBER                      
         MVI   WIOUKLEN,WIOURLL    SET MINIMUM LENGTH OF URL                    
*                                                                               
*        URL ELEMENT BUILD                                                      
*                                                                               
         LA    R3,DATADATA         POINT TO URL DATA                            
         USING SUBDD,R3            ESTABLISH SUBDATA DSECT                      
*                                                                               
WBURLLP  DS    0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBURLDN                                                          
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBURHSTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBURLX                                                           
*                                                                               
WBURHSTN DS    0H                                                               
*                                                                               
*        FIND SUBDATA TYPE                                                      
*                                                                               
         LA    R5,URLTAB                                                        
         USING URLTABD,R5          ESTABLISH URL DATA TYPE TABLE                
*                                                                               
WBUTYPLP DS    0H                                                               
*                                                                               
         CLI   0(R5),X'FF'         CHECK FOR END OF TABLE                       
         BE    WBUTYPDN                                                         
*                                                                               
         CLC   URLTTYPE,SUBDID     MATCH URL DATA TYPE                          
         BE    WBUTYPFD                                                         
*                                                                               
WBUTYPCN DS    0H                                                               
*                                                                               
         LA    R5,URLTABL(R5)      BUMP TO NEXT TYPE IN TABLE                   
*                                                                               
         B     WBUTYPLP                                                         
*                                                                               
WBUTYPDN DS    0H                                                               
*                                                                               
         B     WBURLCN             UNKNOWN DATA TYPE                            
*                                                                               
WBUTYPFD DS    0H                                                               
*                                                                               
         CLI   URLTDISP,WIOUKID-WIOURLD  CHECK FOR KID DATA TYPE                
         BE    WBUTKID                                                          
*                                  ELSE                                         
         CLI   URLTDISP,WIOUSTL-WIOURLD  CHECK FOR STYLE DATA TYPE              
         BE    WBUTSTL                                                          
*                                  ELSE                                         
         SR    R1,R1                                                            
         IC    R1,URLTDISP         GET DISPLACEMENT FOR DATA                    
         LA    R1,WIOURLD(R1)                                                   
*                                                                               
         MVC   0(1,R1),SUBDDATA      SAVE FIRST CH OF TYPE                      
*                                                                               
         B     WBURLCN                                                          
*                                                                               
WBUTSTL  DS    0H                                                               
*                                                                               
         CLC   =C'WCOST',SUBDDATA  FIND VALUE FOR INDICATOR                     
         BNE   *+12                                                             
         MVI   WIOUSTL,C'C'        WITH COSTS                                   
         B     WBUTSTLX                                                         
*                                                                               
         CLC   =C'WOCOST',SUBDDATA  FIND VALUE FOR INDICATOR                    
         BNE   *+12                                                             
         MVI   WIOUSTL,C'N'       WITHOUT COSTS                                 
         B     WBUTSTLX                                                         
*                                                                               
WBUTSTLX DS    0H                                                               
         B     WBURLCN                                                          
*                                                                               
WBUTKID  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         BCT   RF,*+8              DECREMENT FOR EXECUTE                        
         B     *+8                 NO DATA AVAILABLE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WIOUKID(0),SUBDDATA ADD URL TO ELEMENT                           
*                                                                               
         SR    RE,RE               UPDATE ELEMENT LENGTH                        
         IC    RE,WIOUKLEN                                                      
         LA    RE,1(RF,RE)                                                      
         STC   RE,WIOUKLEN                                                      
*                                                                               
WBUTKIDX DS    0H                                                               
*                                                                               
         B     WBURLCN                                                          
*                                                                               
WBUTKIDN DS    0H                                                               
*                                                                               
WBURLCN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT SUBDATA                         
*                                                                               
         B     WBURLLP                                                          
*                                                                               
WBURLDN  DS    0H                                                               
*                                                                               
         GOTOR ADDELM,DMCB,WIOUKEY ADD NEW URL ELEMENT                          
         CLI   MINERR,MINEDUP      OKAY IF A DUPLICATE ELM                      
         BE    *+14                NO ERRORS                                    
         CLI   MINERR,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH3,WIOAURL    URL ADDED                                      
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBURLX   DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBURLXL DS     0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBURLXD                                                          
*                                                                               
WBURLXC DS     0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBURLXL                                                          
*                                                                               
WBURLXD DS     0H                                                               
*                                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - URLTAB'                          
***********************************************************************         
*                                                                     *         
*        TABLE OF URL DATA TYPES                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
URLTAB   DS    0D                                                               
         DC    C'TYPE      ',AL1(WIOUTYP-WIOURLD)  TYPE                         
         DC    C'STYLE     ',AL1(WIOUSTL-WIOURLD)  STYLE                        
         DC    C'DELIVERY  ',AL1(WIOUDLM-WIOURLD)  DELIVERY                     
         DC    C'KID       ',AL1(WIOUKID-WIOURLD)  KID                          
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBRTM'                           
***********************************************************************         
*                                                                     *         
*        ADD REPONSE TIME TO INSERTION ORDER HEADER                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBRTM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
         USING WBMQD,R9            ESTABLISH WEBIO MQ DATA                      
*                                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
*                                                                               
*        READ HEADER ELEMENT                                                    
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET FOR HEADER ELEMENT                       
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY FIND HEADER ELEMENT                          
         BNZ   WBRTMX              NONE FOUND                                   
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        ADD RESPONSE TIME                                                      
*                                                                               
         LA    R3,DATADATA         POINT TO URL DATA                            
         USING SUBDD,R3            ESTABLISH SUBDATA DSECT                      
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBRTMX                                                           
*                                                                               
         CLC   =CL10'HISTID',SUBDID   CHECK FOR HISTORY ID                      
         BNE   WBRTHSTN                                                         
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
*                                                                               
         MVC   QGRPID,SPACES       INIT GROUP ID                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QGRPID(0),SUBDDATA  SAVE GROUP ID                                
*                                                                               
         LA    R3,SUBDDATA+1(RF)   BUMP TO NEXT SUBDATA                         
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE AT END OF DATA                          
         BE    WBRTMX                                                           
*                                                                               
WBRTHSTN DS    0H                                                               
*                                                                               
         CLC   SUBDID,=CL10'RESPTM'  MUST BE RESPTM DATA ELEMENT                
         BNE   WBRTMX                                                           
*                                                                               
         CLC   SUBDDATA(16),SPACES   SKIP IF NO DATE GIVEN                      
         BNH   WBRDTEX                                                          
*                                                                               
*        SAVE TIME TO REPLY                                                     
*                                                                               
         GOTOR VDATCON,DMCB,(10,SUBDDATA),(3,WIOHRPDT)  CONVERT DATE            
*                                                                               
         PACK  DUB,SUBDDATA+11(2)  HOURS                                        
         TP    DUB                 SKIP IF NOT PACKED                           
         BNZ   WBRTIMX                                                          
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,WIOHRPTM                                                      
*                                                                               
         PACK  DUB,SUBDDATA+14(2)  MINUTES                                      
         TP    DUB                 SKIP IF NOT PACKED                           
         BNZ   WBRTIMX                                                          
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,WIOHRPTM+1                                                    
*                                                                               
WBRTIMX  DS    0H                                                               
*                                                                               
WBRDTEX  DS    0H                                                               
*                                                                               
         GOTOR WRTELM,DMCB,WIOHKEY RE-WRITE HEADER ELM                          
         BZ    *+6                 NO ERRORS                                    
         DC    H'0'                                                             
*                                                                               
*        UPDATE ACTIVITY ELEMENTS                                               
*                                                                               
         OI    SVACH3,WIOARTM    TIME TO REPLY UPDATED                          
*                                                                               
         GOTOR ACTPUT              ADD ACTIVITY ELEMENT                         
*                                                                               
WBRTMX   DS    0H                                                               
*                                                                               
*        BUMP TO NEXT 10C'0'                                                    
*                                                                               
WBRTMXL DS     0H                                                               
*                                                                               
         CLC   SUBDID(2),=10C'0'   DONE IF END OF DATA                          
         BE    WBRTMXD                                                          
*                                                                               
WBRTMXC DS     0H                                                               
*                                                                               
         PACK  DUB,SUBDLEN         GET DATA LENGTH                              
         CVB   RF,DUB                                                           
         LA    R3,SUBDDATA(RF)     BUMP TO NEXT DATA ELEMENT                    
*                                                                               
         B     WBRTMXL                                                          
*                                                                               
WBRTMXD DS     0H                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - GETEL'                           
***********************************************************************         
*                                                                     *         
*        MACRO TO FIND NEXT ELEMENT IN RECORD - NON-MINIO             *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*        GETEL FINDS FIRST INSTANCE OF ELEMENT CODE                   *         
*        NXTEL FINDS NEXT INSTANCE OF ELEMENT                         *         
*                                                                     *         
*                                                                     *         
*NTRY    R6==> START OF RECORD OR CURRENT ELEMENT                     *         
*        ELCODE HAS CODE FOR ELEMENT                                  *         
*                                                                     *         
*EXIT    R6==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - ELEMENT NOT FOUND                                *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         GETEL (R6),33,ELCODE                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - SYSINIT'                         
***********************************************************************         
*                                                                     *         
*        INITIALIZE SYSTEM DEPENDENT VARIABLES                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7          ESTABLISH WORKING STORAGE                    
*                                                                               
*        SET UP ADDRESSES TO INCLUDED MODULES                                   
*                                                                               
         LA    R2,SYSV             ADDRS OF  INCLUDED ROUTINES                  
         LA    R3,SYSVCON          VCONS OF  INCLUDED ROUTINES                  
         LA    R4,NVTYPES          NUMBER OF INCLUDED ROUTINES                  
         LTR   R4,R4               SKIP IF NONE                                 
         BZ    SINVCOND                                                         
*                                                                               
SINVCONL DS    0H                                                               
*                                                                               
         L     R1,0(R3)            GET V(INCLUDED ROUTINE)                      
         A     R1,RELO             RELOCATE ADDRESS                             
         ST    R1,0(R2)            SAVE ADDRESS                                 
*                                                                               
SINVCONC DS    0H                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS STORAGE                 
         LA    R3,4(R3)            BUMP TO NEXT V-CON                           
         BCT   R4,SINVCONC         LOOP THROUGH LISTS                           
*                                                                               
SINVCOND DS    0H                                                               
*                                                                               
*        SET UP LIST OF COMMON ROUTINES                                         
*              ALL ARE LOCATED IN THIS MODULE                                   
*                                                                               
         LA    R2,VCOMMON          COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,SYSCOMM          START OF ADDRESS AVEAREA                     
         LA    R5,VCOUNT           SET NUMBER OF ROUTINES                       
*                                                                               
SINCOMLP DS    0H                                                               
*                                                                               
         ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
*                                                                               
SINCOMCN DS    0H                                                               
*                                                                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,SINCOMLP         NEXT ROUTINER5,                              
*                                                                               
SINCOMDN DS    0H                                                               
*                                                                               
*        LOAD CORE-RESIDENT MODULES                                             
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R2,CORETAB          CORE-RESIDENT IDS                            
         LA    R0,CORES            NUMBER OF CORE-RESIDENT PHASES               
         LA    R4,COREFACS         ADDRESS SAVEAREA                             
*                                                                               
         L     R1,SRPACOM          SAVE ADDRESS                                 
         USING COMFACSD,R1                                                      
*                                                                               
         ST    R1,ACOMFACS         SAVE COMFACS  ADDRESS                        
         MVC   VRECUP,CRECUP       SAVE RECUP    ADDRESS                        
         MVC   VDMGR,CDATAMGR      SAVE DATA MGR ADDRESS                        
         MVC   VPERVAL,CPERVAL     SAVE PERVAL   ADDRESS                        
         MVC   VDATCON,CDATCON     SAVE DATCON   ADDRESS                        
         MVC   VSWITCH,CSWITCH     SAVE SWITCH   ADDRESS                        
         MVC   AJESMAIL,CJESMAIL   SAVE JESMAIL  ADDRESS                        
         MVC   VGETTXT,CGETTXT     SAVE GETTXT   ADDRESS                        
*                                                                               
         L     RF,CCALLOV          A(CALLOV)                                    
         DROP  R1                                                               
*                                                                               
*        GET CORE RESIDENT ADDRESSES                                            
*                                                                               
         LA    R1,DMCB             INIT PHASE NAME AREA                         
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SINCORLP DS    0H                                                               
*                                                                               
         MVC   DMCB+7(1),0(R2)     SET PHASE ID                                 
*                                                                               
         GOTO1 (RF),(R1),0         LOAD PHASE                                   
*                                                                               
         CLI   DMCB+4,X'FF'        NO ERRORS TOLERATED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
*                                                                               
SINCORCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SINCORLP                                                      
*                                                                               
SINCORDN DS    0H                                                               
*                                                                               
         LHI   RF,IOAREA1-WORKD   SET IOAREA ADDRESSES                          
         AR    RF,RC                                                            
         ST    RF,AIO1                                                          
         AHI   RF,4096                                                          
         ST    RF,AIO2                                                          
         AHI   RF,4096                                                          
         ST    RF,AIO3                                                          
*                                                                               
         MVC   AIO,AIO1            SET DEFAULT IOAREA ADDRESS                   
*                                                                               
         MVC   QAGY,TWAAGY         SAVE AGENCY APLPHA                           
*                                                                               
         MVI   SPACES,C' '         FILL IN SPACES FIELD                         
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
SYSINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - CONSTANTS'                       
***********************************************************************         
*                                                                     *         
*        CONSTANTS TABLES, ETC                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSVCON  DS    0F                  INCLUDED MODULES                             
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4       NUMBER OF INCLUDED MODULES                   
*                                                                               
CORETAB  DS    0X                  CORE-RESIDENT PHASES                         
         DC    AL1(QMINIO)         MINIO                                        
         DC    AL1(QPUBVAL)        PUB VALIDATION                               
         DC    AL1(QPUBEDIT)       PUB CODE DISPLAY                             
CORES    EQU   (*-CORETAB)         NUMBER OF PHASES                             
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VCOMMON'                         
***********************************************************************         
*                                                                     *         
*        COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         SRL   RF,24               SHIFT ROUTINE ID TO RIGHT NYBBLE             
         L     RF,VBRANCH(RF)      GET A(ROUTINE)                               
         A     RF,RELO             RELOCATE ADDRESS                             
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
VCOMMONX DS    0H                                                               
         XIT1                      RETURN TO CALLER                             
*                                                                               
*        COMMON ROUTINE ADDRESSES                                               
*                                                                               
VBRANCH  DS    0D                  ALIGNMENT                                    
         DC    A(VVALMED)          VALIDATE MEDIA                               
         DC    A(VDISMED)          DISPLAY  MEDIA                               
         DC    A(VVALCLT)          VALIDATE CLIENT                              
         DC    A(VDISCLT)          DISPLAY  CLIENT                              
         DC    A(VVALPRD)          VALIDATE PRODUCT                             
         DC    A(VDISPRD)          DISPLAY  PRODUCT                             
         DC    A(VVALPER)          VALIDATE PERIOD                              
         DC    A(VDISPER)          DISPLAY  PERIOD                              
         DC    A(VVALPUB)          VALIDATE PUB                                 
         DC    A(VDISPUB)          DISPLAY  PUB                                 
         DC    A(VMININIT)         INITIALIZE MINIO SET                         
         DC    A(VADDELM)          ADD     ELEMENT TO   MINIO SET               
         DC    A(VWRTELM)          REPLACE ELEMENT IN   MINIO SET               
         DC    A(VDELELM)          DELETE  ELEMENT FROM MINIO SET               
         DC    A(VGETELM)          FIND          ELEMENT IN MINIO SET           
         DC    A(VNXTELM)          FIND NEXT     ELEMENT IN MINIO SET           
         DC    A(VPRVELM)          FIND PREVIOUS ELEMENT IN MINIO SET           
         DC    A(VERREXIT)         ERROR EXIT                                   
         DC    A(VPSSVS)           CREATE PASSIVES                              
         DC    A(VVALSTA)          VALIDATE STATUS                              
         DC    A(VVALSTCD)         VALIDATE STATUS CODE                         
         DC    A(VDISSTA)          DISPLAY  STATUS                              
         DC    A(VGETSCH)          GET SCHEMA RECORD                            
         DC    A(VFMTIO#)          FORMAT IO#                                   
         DC    A(VVALRV#)          VALIDATE REVISION NUMBER                     
         DC    A(VDISRV#)          DISPLAY  REVISION NUMBER                     
         DC    A(VACTPUT)          ADD ACTIVITY ELEMENT                         
         DC    A(VFNDIO#)          FIND IO# FOR DATE                            
         DC    A(VPRSIO#)          PARSE EXPANDED IO# INTO COMPONENTS           
         DC    A(VFNDRV#)          FIND LATEST REVISION #                       
         DC    A(VSETDMGR)         SET DATAMANAGER FILES                        
         DC    A(VDISTIM)          DISPLAY TIME                                 
         DC    A(VFNDGP#)          FIND CURRENT GROUP NUMBER                    
         DC    A(VDGTFX#)          DIGITIZE FAX NUMBER                          
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
*          DATA SET SRPMQ00    AT LEVEL 011 AS OF 09/07/04                      
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VCOMMON'                         
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VVALMED'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIA CODE                                          *         
*                                                                     *         
*NTRY    P0+0  INPUT LENGTH                                           *         
*        P0    A(MED)                                                 *         
*                                                                     *         
*EXIT    QMED   MEDIA CODE                                            *         
*        AIO1  A(PAGYREC)                                             *         
*        MEDNM  MEDIA NAME                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALMED  NTR1  BASE=*,LABEL=*      GET AGY REC IN MEDIA VALIDATION              
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R3)          GET INPUT LENGTH                             
         BZ    VMEDINVE                                                         
*                                                                               
         L     R2,0(R3)            POINT TO INPUT                               
*                                                                               
*                                                                               
         CLI   0(R2),1             INPUT LENGTH MUST BE 1                       
         BNE   VMEDERR                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY              ESTABLISH AGENCY RECORD KEY                  
         USING PAGYKEY,R4                                                       
*                                                                               
         MVC   PAGYKAGY,QAGY       AGENCY ALPHA                                 
         MVC   PAGYKMED,0(R2)      MEDIA                                        
         MVI   PAGYKRCD,X'01'      AGYREC ID                                    
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'PRTDIR',=CL8'DMRDHI',KEY,KEY                     
*                                                                               
         CLC   KEY(25),KEYSAVE     RECORD MUST BE FOUND                         
         BNE   VMEDINVE                                                         
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'PRTFILE',KEY+27,AIO1,      X        
               DMWORK                                                           
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VMEDINVE                                                         
*                                                                               
         L     R4,AIO1             POINT TO READ RECORD                         
*                                                                               
         MVC   QMED,PAGYKMED       SAVE MEDIA                                   
*                                                                               
         LR    R6,R4               POINT TO START OF RECORD                     
         MVI   ELCODE,X'01'        FIND AGENCY ELEMENT                          
*                                                                               
         BRAS  RE,GETEL            GO LOOK FOR ELEMENT                          
         BNE   VMEDINVE            MUST FIND ELEMENT                            
*                                                                               
         USING PAGYELEM,R6         ESTABLISH AS AGENCY ELEMENT                  
*                                                                               
         MVC   MEDNM,PAGYMED       MEDIA NAME                                   
*                                                                               
VVALMEDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VMEDINVE DS    0H                  INVALID MEDIA                                
         MVI   ERROR,PPEMEDNV      DEFAULT ERROR CODE                           
         B     VMEDERR                                                          
*                                                                               
VMEDERR  DS    0H                                                               
*                                                                               
         XC    QMED,QMED           CLEAR MEDIA SAVEAREA                         
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDISMED'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY MEDIA CODE                                           *         
*                                                                     *         
*NTRY    P0    A(MED CODE)                                            *         
*        P1    A(OUTPUT)                                              *         
*                                                                     *         
*EXIT          MED CODE FILLED IN                                     *         
*        AIO1  A(PAGYREC)                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISMED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             estABLISH TWA                                
         USING WORKD,RC            estABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R3)            POINT TO OUTPUT FIELD                        
         L     R1,4(R3)            A(OUTPUT)                                    
*                                                                               
         MVC   0(1,R1),0(R2)       DISPLAY MEDIA CODE                           
*                                                                               
         GOTOR VALMED,DMCB,0(1,R2)   VALIDATE MEDIA - FILLS IN NAME             
*                                                                               
VDISMEDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VVALCLT'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT CODE                                         *         
*                                                                     *         
*NTRY    P0+0  INPUT LENGTH                                           *         
*        P0    A(CLT)                                                 *         
*                                                                     *         
*EXIT    QCLT   CLIENT CODE                                           *         
*        AIO1   A(PCLTREC)                                            *         
*        CLTNM  CLIENT NAME                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALCLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R3)            POINT TO INPUT                               
*                                                                               
         CLI   0(R3),L'QCLT        CHECK ON CODE LENGTH                         
         BH    VCLTINVE                                                         
*                                                                               
*        READ CLIENT RECORD                                                     
*                                                                               
         XC    KEY,KEY             INIT KEY BUILD AREA                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4         ESTABLISH CLIENT RECORD KEY                  
*                                  BUILD CLIENT KEY                             
         MVC   PCLTKAGY,QAGY       AGENCY                                       
         MVC   PCLTKMED,QMED       MEDIA                                        
         MVI   PCLTKRCD,PCLTKIDQ   CLIENT RECORD CODE                           
         MVC   PCLTKCLT,0(R2)      CLIENT CODE                                  
         OC    PCLTKCLT,SPACES     SPACE FILL CLIENT CODE                       
*                                                                               
         CLC   =C'***',0(R2)       IF CLIENT VARIOUS                            
         BNE   VCLTVARN                                                         
*                                                                               
         MVC   CLTNM,=CL20'VARIOUS' FILL IN NAME                                
         XC    SVCPROF,SVCPROF     SET  CLIENT PROFILE                          
         XC    SVCLTOFC,SVCLTOFC   SET  CLIENT OFFICE                           
*                                                                               
         B     VVALCLTX                                                         
*                                                                               
VCLTVARN DS    0H                                                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=CL8'PRTDIR',KEY,KEY                     
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VCLTINVE                                                         
*                                                                               
         CLC   PCLTKEY,KEYSAVE     MUST FIND CLIENT RECORD                      
         BNE   VCLTINVE                                                         
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'PRTFILE',KEY+27,AIO1,      X        
               DMWORK                                                           
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VCLTINVE                                                         
*                                                                               
         L     R4,AIO1             POINT TO CLTHDR IN IAOA1                     
*                                                                               
         MVC   QCLT,PCLTKCLT       SET CLIENT CODE                              
         MVC   CLTNM,PCLTNAME      SET CLIENT NAME                              
*                                                                               
VCLTNAMX DS    0H                                                               
*                                                                               
VVALCLTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VCLTINVE MVI   ERROR,PPECLTNV     INVALID CLIENT                                
         B     VCLTERR                                                          
*                                                                               
VCLTSECE MVI   ERROR,PPESECLK     SECURITY LOCK-OUT                             
         B     VCLTERR                                                          
*                                                                               
VCLTERR  DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           CLEAR CLIENT SAVEAREA                        
         XC    CLTNM,CLTNM         CLEAR CLIENT SAVEAREA                        
*                                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDISCLT'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY CLIENT CODE AND NAME                                 *         
*                                                                     *         
*NTRY    P0    A(CLIENT CODE)                                         *         
*        P1    A(OUTPUT)                                              *         
*                                                                     *         
*EXIT    QCLT   CLIENT CODE                                           *         
*        AIO1   A(PCLTREC)                                            *         
*        CLTNM  CLIENT NAME                                           *         
*              CLIENT CODE FILLED IN                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISCLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R3)            POINT TO INPUT FIELD                         
         L     R1,4(R3)            A(OUTPUT)                                    
*                                                                               
         MVC   0(L'QCLT,R1),0(R2)  DISPLAY CLIENT CODE                          
*                                                                               
         GOTOR VALCLT,DMCB,(L'QCLT,0(R2)) VALIDATE CLIENT - FILLS NAME          
*                                                                               
VDISCLTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VVALPRD'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE PRODUCT CODE                                        *         
*                                                                     *         
*NTRY    P0+0  INPUT LENGTH                                           *         
*        P0    A(PRD)                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)          GET INPUT LENGTH                             
         BZ    VPRD2E              MUST HAVE INPUT                              
*                                                                               
         CHI   RF,2                LENGTH MUST BE 2 OR 3 CHARCTERS              
         BL    VPRD3E                                                           
         CHI   RF,3                                                             
         BH    VPRD3E                                                           
*                                                                               
         L     R2,0(R3)            POINT TO PRODUCT CODE                        
*                                                                               
         XC    KEY,KEY             BUILD PRODUCT RECORD KEY                     
         LA    R4,KEY                                                           
         USING PPRDKEY,R4          ESTABLISH PRODUCT RECORD KEY                 
*                                                                               
         MVC   PPRDKAGY,QAGY       AGENCY                                       
         MVC   PPRDKMED,QMED       MEDIA                                        
         MVI   PPRDKRCD,PPRDKIDQ   PRODUCT IDENTIFIER                           
         MVC   PPRDKCLT,QCLT       CLIENT                                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PPRDKPRD(0),0(R2)   PRODUCT CODE                                 
         OC    PPRDKPRD,SPACES     BLANK FILL                                   
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=CL8'PRTDIR',KEY,KEY                     
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VPRDINVE                                                         
*                                                                               
         CLC   PPRDKEY,KEYSAVE     PRODUCT NOT FOUND                            
         BNE   VPRDINVE                                                         
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'PRTFILE',KEY+27,AIO1,      X        
               DMWORK                                                           
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VPRDINVE                                                         
*                                                                               
         L     R4,AIO1             POINT TO PRDHDR IN IAOA1                     
*                                                                               
         LR    R6,R4               POINT TO STAT OF RECORD                      
         MVI   ELCODE,X'06'        FIND PRODUCT DESCRIPTION ELEMENT             
*                                                                               
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNE   VPRDINVE            MUST FIND ELEMENT                            
*                                                                               
         USING PPRDELEM,R6         ESTABLISH AS PRODUCT DESC ELM                
*                                                                               
         MVC   QPRD,PPRDKPRD       SAVE PRODUCT CODE                            
         MVC   PRDNM,PPRDNAME      SAVE PRODUCT NAME                            
*                                                                               
VVALPRDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPRDINVE MVI   ERROR,PPEPRDNV     INVALID PRODUCT                               
         B     VPRDERR                                                          
*                                                                               
VPRD2E   MVI   ERROR,PPEFLDNE     PRODUCT MISSING                               
         B     VPRDERR                                                          
*                                                                               
VPRD3E   MVI   ERROR,PPEPRDNV     INVALID PRODUCT                               
         B     VPRDERR                                                          
*                                                                               
VPRDERR  DS    0H                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDISPRD'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY PRODUCT CODE                                         *         
*                                                                     *         
*NTRY    P0    A(PRODDUCT CODE)                                       *         
*        P1    A(OUTPUT)                                              *         
*                                                                     *         
*EXIT    QPRD   PRODUCT CODE                                          *         
*        AIO1   A(PPRDREC)                                            *         
*        PRDNM  PRODUCT NAME                                          *         
*              PRODUCT CODE FILLED IN                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R3)            POINT TO PRODUCT CODE                        
         L     R1,4(R3)            A(OUTPUT)                                    
*                                                                               
         MVC   0(L'QPRD,R1),0(R2)  DISPLAY PRODUCT CODE                         
*                                                                               
         GOTOR VALPRD,DMCB,(L'QPRD,0(R2))  VALIDATE PROD - FILLS NAME           
*                                                                               
VDISPRDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VVALPUB'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE PUBLICATION CODE                                    *         
*                                                                     *         
*NTRY    P0+0  INPUT LENGTH                                           *         
*        P0    A(PUB)                                                 *         
*                                                                     *         
*EXIT    QPUB   PUBCODE                                               *         
*        PUBNM  PUB NAME                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         XC    QPUB,QPUB           INI PUB SAVEAREA                             
         XC    PUBNM,PUBNM         INIT PUB NAME                                
*                                                                               
         L     R2,0(R3)            GET PUB ADDRESS                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R3)          GET INPUT LENGTH                             
*                                                                               
         GOTO1 VPUBVAL,DMCB,((R0),(R2)),QPUB                                    
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    VPUBINVE                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH PUBREC KEY                         
         USING PUBRECD,R4                                                       
*                                  BUILD PUB REC KEY                            
         MVC   PUBKMED,QMED        MEDIA                                        
         MVC   PUBKPUB(6),QPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,QAGY        AGENCY                                       
         MVI   PUBKCOD,X'81'       RECORD CODE                                  
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=CL8'PUBDIR',KEY,KEY                     
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VPUBINVE                                                         
*                                                                               
         CLC   PUBKEY,KEYSAVE      ERROR IF PUB NOT ON FILE                     
         BNE   VPUBINVE                                                         
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'PUBFILE',KEY+27,AIO1,      X        
               DMWORK                                                           
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   VPUBINVE                                                         
*                                                                               
         L     R4,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               POINT TO START OF RECORD                     
         MVI   ELCODE,PUBNAMQ      SEARCH FOR PUBNAME ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING PUBNAMEL,R6         ESTABLISH NAME ELEMENT                       
         MVC   PUBNM,PUBNAME       SAVE PUB NAME                                
*                                                                               
         MVC   QPUB,PUBKPUB           SAVE PUB CODE                             
*                                                                               
VVALPUBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPUBINVE MVI   ERROR,PPEPUBNV      INVALID PUB                                  
         B     VPUBERR                                                          
*                                                                               
VPUBERR  DS    0H                                                               
*                                                                               
         XC    QPUB,QPUB           CLEAR PUB SAVEAREA                           
         XC    PUBNM,PUBNM         CLEAR PUB NAME                               
*                                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDISPUB'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY PUB CODE AND NAME                                    *         
*                                                                     *         
*NTRY    P0    A(PUB CODE)                                            *         
*        R1==> PUB CODE FIELD                                         *         
*                                                                     *         
*EXIT          PUB CODE FILLED IN                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             estABLISH TWA                                
         USING WORKD,RC            estABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R3)            POINT TO OUTPUT FIELD                        
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,QPUB),(C'S',0(R2)) DISPLAY FLD                  
*                                                                               
VDISPUBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VVALPER'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE PERIOD                                              *         
*                                                                     *         
*NTRY    P0+0  INPUT LENGTH                                           *         
*        P0    A(INPUT)                                               *         
*                                                                     *         
*EXIT    QSTART AND QEND FILLED IN                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R3)          GET INPUT LENGTH                             
         BZ    VPERINVE                                                         
*                                                                               
         L     R2,0(R3)            POINT TO INPUT                               
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,PVALBSTA)  TODAY'S DATE                    
*                                                                               
         GOTO1 VPERVAL,DMCB,((R0),0(R2)),('PVINTOD',(R4)) VAL PERIOD            
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VPERINVE                                                         
*                                                                               
         MVC   QSTART,PVALESTA    SAVE START AND END DATES                      
         MVC   QEND,PVALEEND          YYMMDD                                    
         MVC   QPER,PVALBSTA                                                    
*                                                                               
         CLC   QEND,QSTART         START MUST BE BEFORE END                     
         BL    VPERDTSE                                                         
*                                                                               
VVALPERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPERINVE MVI   ERROR,PPEDTENV      INVALID DATE                                 
         B     VPERERR                                                          
*                                                                               
VPERDTSE MVI   ERROR,PPESTEND      END DATE BEFORE START DATE                   
*                                                                               
VPERERR  DS    0H                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDISPER'                         
***********************************************************************         
*                                                                     *         
*        DISPLAY PERIOD                                               *         
*                                                                     *         
*NTRY    P0    A(START DATE) - BINARY YMD                             *         
*        P1==> A(OUTPUT)                                              *         
*                                                                     *         
*EXIT          PERIOD   FILLED IN                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         L     R2,0(R1)            POINT TO BINARY PERIOD DATES                 
         L     R4,4(R3)            POINT TO OUTPUT AREA                         
*                                                                               
         OC    0(3,R2),0(R2)       SKIP IF NO DATES GIVEN                       
         BZ    VDISPERX                                                         
*                                                                               
*        INPUT INDICATORS                                                       
*              X'80' - RETURN DATE IN P2(1)                                     
*              X'10' - START AND END DATES GIVEN                                
*              X'03' - DATES IN BINARY FORMAT                                   
*        OUTPUT INDICATORS                                                      
*              17    - MMMDD/YY-MMMDD/YY                                        
*                                                                               
         GOTO1 VDATCON,WIOPARMS,(X'93',0(R2)),(17,0(R4))                        
*                                                                               
VDISPERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VERREX'                          
***********************************************************************         
*                                                                     *         
*        INPUTS R2=A(SCREEN HEADER)                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VERREXIT NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         CLC   SVMQION,=F'2'       SKIP IF NO TRACING                           
         BNE   VERRTR3                                                          
*                                                                               
         GOTOR VDMGR,DMCB,=C'OPMSG',=C'WEBIO ERROR'                             
*                                                                               
VERRTR3  DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO PERROR IF REQUIRED                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   PERROR+1(1),ERROR      PUT IN NEW STYLE                          
*                                                                               
         CLC   PERROR,=X'FE04'     SKIP IF EIO/ESR MISSING                      
         BE    VERRX                                                            
*                                                                               
         CLI   PERROR,X'FE'        IF INTERNAL ERROR MESSAGE                    
         BNE   *+12                                                             
         BRAS  RE,SETERR              FIND MESSAGE                              
         B     VER10                                                            
*                                                                               
*        INIT GETTXT CONTROL BLOCK                                              
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
*                                                                               
         MVI   GTMSYS,4            MESSAGE SYSTEM IS PRINT                      
         MVC   GTMSGNO,PERROR      MESSAGE NUMBER                               
         MVI   GTMTYP,GTMERR       ERROR MESSAGES                               
*                                                                               
*        DISPLAY MESSAGE IN WORK AREA                                           
*                                                                               
         XC    WORK,WORK           INIT MESSAGE AREA                            
*                                                                               
         LA    RF,WORK             POINT TO ERROR MESSAGE AREA                  
         STCM  RF,7,GTAOUT         PASS TO GETTXT                               
*                                                                               
         L     RF,VDMGR            PASS V(DATAMGR)                              
         STCM  RF,7,GTADMCB                                                     
*                                                                               
         MVI   GTMAXL,60           SET MAX MESSAGE LENGTH                       
*                                                                               
         MVI   GT1INDS,GT1NOREF+GT1OWRK  NO REF # AND AOUT IS WORKAREA          
*                                                                               
         GOTOR VGETTXT             DISPLAY ERROR MESSAGE                        
*                                                                               
         DROP  R1                                                               
*                                                                               
VER10    DS    0H                                                               
*                                                                               
*        PUT OUT ERROR MESSAGE AS E-MAIL                                        
*                                                                               
         LHI   R8,EMLFLDS-WORKD    ESTABLISH E-MAIL FIELDS                      
         LA    R8,WORKD(R8)                                                     
         USING EMLFLDS,R8                                                       
*                                                                               
*        FORMAT SUBJECT LINE                                                    
*                                                                               
         MVC   EMLSUBJ,=CL70'MQ - ERROR IN PROCESSING'                          
*                                                                               
*        FORMAT E-MAIL MESSAGE                                                  
*                                                                               
*        FIND SYSTEM WHERE WE ARE                                               
*                                                                               
*        GO TO GETFACTS FOR SYSTEM NAME                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',0),F#SSBD,0   EXTRACT SSB DATA              
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING F@SSBD,R1           ESTABLISH SSB EXTRACT                        
*                                                                               
         MVC   EMLLIN1(20),=CL20'SYSTEM XXX'                                    
         MVC   EMLLIN1+7(3),F@SSYSNA  SYSTEM NAME                               
*                                                                               
         MVC   EMLLIN1+20(60),WORK DISPLAY ERROR MESSAGE                        
*                                                                               
*        DISPLAY DATA FROM EDICT OR WS                                          
*                                                                               
         L     RE,SRPATIA          START OF DATA                                
*                                                                               
         MVC   EMLLIN2(14),=C'TBUFF LENGTH ='                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,MQMSGLN        MQMESSAGE LENGTH LENGTH                      
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  EMLLIN2+16(7),DUB+4(4) PRINT LENGTH                              
*                                                                               
         LA    R0,EMLLIN3          START PRINTING ON LINE 3                     
*                                                                               
         LHI   R1,640              MAX DATA FOR E-MAIL                          
         LR    RF,R1                                                            
*                                                                               
         MVCL  R0,RE               MOVE DATA TO E-MAIL                          
*                                                                               
         MVI   EMLLIN10+80,X'FF'       END OF MESSAGE                           
*                                                                               
         LA    R1,SMTPC            ESTABLISH EMAIL PARAMETER LIST               
         USING SMTPD,R1                                                         
*                                                                               
         XC    SMTPC,SMTPC         INIT PARAMETER BLOCK                         
*                                                                               
         LA    RF,TOEMLST          SET TO ADDRESS                               
*                                                                               
         CLC   PERROR,=X'FE05'     IF MESSAGE TOO LONG                          
         BNE   *+8                                                              
         LA    RF,TOEMLST1            USE SECOND E-MAIL LIST                    
*                                                                               
         ST    RF,SMTPTO                                                        
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          SET SUBJECT ADDRESS                          
         LA    RF,EMLMSG           E-MAIL ADDRESS                               
         ST    RF,SMTPDATA                                                      
*                                                                               
         GOTOR AJESMAIL,(R1)        SEND E-MAIL                                 
*                                                                               
VERRX    DS    0H                                                               
         L     RD,SYSRD             RESTORE INCOMING RD                         
         XIT1                                                                   
*                                                                               
TOEMLST  DS    0C                  TO EMAIL LIST                                
         DC    CL60'kwang@mediaocean.com'                                       
         DC    X'FF'               END OF LIST                                  
TOEMLST1 DS    0C                  TO EMAIL LIST                                
         DC    CL60'kwang@mediaocean.com'                                       
         DC    CL60'ANTHONY.HYDE@mediaocean.com'                                
         DC    X'FF'               END OF LIST                                  
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - SETERR'                          
***********************************************************************         
*                                                                     *         
*        BUILD INTERNAL ERROR MESSAGE IN WORK                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETERR   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         XC    WORK,WORK           INIT WORKAREA                                
*                                                                               
         LA    R1,ERRLST           FIND ERROR IN LIST                           
*                                                                               
SETERRLP DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         DONE AT END OF TABLE                         
         BE    SETERRDN                                                         
*                                                                               
         CLC   PERROR,0(R1)        MATCH ON ERROR CODE                          
         BE    SETERRFD                                                         
*                                                                               
SETERRCN DS    0H                                                               
         LA    R1,82(R1)           NEXT ITEM IN LIST                            
         B     SETERRLP                                                         
*                                                                               
SETERRDN DS    0H                  ERROR NOT IN LIST                            
         B     SETERRX                                                          
*                                                                               
SETERRFD DS    0H                                                               
*                                                                               
         MVC   WORK(80),2(R1)      MOVE MESSAGE TO WORK                         
*                                                                               
         CLI   PERROR+1,X'01'      IF SYSTEM IS NOT AVAIALBE                    
         BNE   SETERR1N                                                         
*                                                                               
         UNPK  DUB(3),PRTSE(2)        UNPACK SYSTEM NUMBER                      
         TR    DUB(2),HEXTAB-C'0'     TRANSLATE TO HEX                          
*                                                                               
         MVC   WORK+6(2),DUB          DISPLAY IN MESSAGE                        
*                                                                               
         B     SETERRMX                                                         
*                                                                               
SETERR1N DS    0H                                                               
*                                                                               
         CLI   PERROR+1,X'02'      IF DATA TYPE UNKNOWN                         
         BNE   SETERR2N                                                         
*                                                                               
         MVC   WORK+19(6),0(R3)    SHOW DATA TYPE                               
*                                                                               
         B     SETERRMX                                                         
*                                                                               
SETERR2N DS    0H                                                               
*                                                                               
         CLI   PERROR+1,X'03'      IF NOT END OF DATA TYPE                      
         BNE   SETERR3N                                                         
*                                                                               
         MVC   WORK+28(10),0(R3)      SHOW DATA FOUND                           
*                                                                               
         B     SETERRMX                                                         
*                                                                               
SETERR3N DS    0H                                                               
*                                                                               
SETERRMX DS    0H                                                               
*                                                                               
SETERRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
ERRLST   DS    0C                  LIST OF ERROR MESSAGES                       
         DC    XL2'FE01',CL80'PRINT XX NOT AVAILABLE'                           
         DC    XL2'FE02',CL80'UNKNOWN DATA TYPE: '                              
         DC    XL2'FE03',CL80'SHOULD BE AT END DATA TYPE: '                     
         DC    XL2'FE04',CL80'EIO/ESR NOT ON MAINFRAME    '                     
         DC    XL2'FE05',CL80'MQ MESSAGE TOO LONG         '                     
         DC    X'FF'               END OF LIST                                  
*                                                                               
         DC    XL2'FE00',CL80' '                                                
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF'  HEX TRANSLATE TABLE                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VMININIT'                        
***********************************************************************         
*                                                                     *         
*        INITIALIZE MINIO BLOCK                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VMININIT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LA    R0,MNBLKCB          CLEAR MINBLOCK AREA                          
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MNRTAB,MNRTAB       CLEAR RECORD  TABLE                          
         XC    MNELEM,MNELEM       CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINCOMF,SRPACOM     A(COMFACS)                                   
         MVC   MINRECUP,VRECUP     V(RECUP)                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'PRTFILE' FILE NAME                                   
         MVC   MINDIR,=CL8'PRTDIR'  DIR NAME                                    
         MVI   MINFKLEN,L'WIOKEY   KEY LENGTH                                   
*                                                                               
         LA    R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT        IE. FULL RECORD RETAINS 75% OF IT'S          
*                                  ELEMENTS ON SPLITING                         
*                                                                               
         MVI   MINNCTL,L'WIODCNTL  2 CONTROL BYTES                              
         LHI   R1,2976                                                          
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH                            
*                                                                               
         MVI   MINEKLEN,L'WIOKELMK      ELEMENT KEY LENGTH                      
         MVI   MINEKDSP,WIOKELMK-WIOKEY ELEMENT KEY DISPLACEMENT                
*                                                                               
         MVC   MINBUFF,AIO2        A(FIRST MINIO BUFFER)                        
         MVI   MINNBUF,2           NUMBER OF BUFFERS                            
*                                                                               
         LA    R1,MNELEM           A(ELEMENT AREA)                              
         ST    R1,MINELEM                                                       
         LHI   R1,L'MNELEM         MAX ELEMENT/CLUSTER LENGTH                   
         STCM  R1,3,MINMAXEL                                                    
*                                                                               
         LA    R1,MNRTAB           A(RECORD TABLE)                              
         ST    R1,MINRTAB                                                       
         LHI   R1,L'MNRTAB                                                      
         STCM  R1,3,MINRTABL       LENGTH OF RECORD TABLE                       
*                                                                               
VMININIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VADDELM'                         
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(ELEMENT TO BE ADDED)                                *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT ADDED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VADDELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE ADDED                 
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         GOTO1 VMINIO,DMCB,('MINADD',MINBLKD)                                   
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
VADDELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDELELM'                         
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(ELEMENT TO BE DELETED)                              *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT DELETED                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDELELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE DELETED               
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE DELETED)                     
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MINEKLEN       GET KEY LENGTH                               
*                                                                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SET REST OF ELEMENT KEY                      
*                                                                               
         GOTO1 VMINIO,WIOPARMS,('MINDEL',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
DELELSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VGETELM'                         
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                         *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VGETELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINRD            DEFAULT TO DIRECT READ                       
*                                                                               
         CLI   1(R6),0             USE DEFAULT IF NO LENGTH GIVEN               
         BE    VGETELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       USE READ IF GREATER THAN KEY LENGTH          
         BNL   VGETELM1                                                         
*                                                                               
         LA    R0,MINHI            SET FOR READ HI/EQUAL                        
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VGETELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTO1 VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VGETELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VWRTELM'                         
***********************************************************************         
* ROUTINE TO REPLACE ELEMENT IN A MINIO SET                           *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE REPLACED)                      *         
*              MUST BE LAST ELEMENT READ                              *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT REPLACED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VWRTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE REPLACED              
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         GOTO1 VMINIO,WIOPARMS,('MINWRT',MINBLKD)                               
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VWRTELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VNXTELM'                         
***********************************************************************         
* ROUTINE TO GET NEXT ELEMENT IN A RECORD                             *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINSEQ           SEQUENTIAL READ                              
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VNXTELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VNXTELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTO1 VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VNXTELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VPRVELM'                         
***********************************************************************         
* ROUTINE TO GET PREVIOUS ELEMENT IN THE RECORD                       *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VPRVELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINBSQ           BACKWARD SEQUENTIAL READ                     
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VPRVELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VPRVELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTO1 VMINIO,WIOPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VPRVELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VNXTIO#'                         
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE INSORD  SERIAL NUMBER    *         
*                                                                     *         
*        IF THERE ARE NO INSORDS  ON FILE                             *         
*           START NUMBERING AT 1                                      *         
*                                                                     *         
*        ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS              *         
*           SERIAL NUMBERS IN 2'S COMPLEMENT. READS LOWEST NUMBER     *         
*           (REALLY HIGHEST) FOR UPDATE AND THEN ADDS POINTER FOR     *         
*           NEXT NUMBER. THIS RESERVES NEXT NUMBER FOR THIS CALL TO   *         
*           THE SUBROUTINE.                                           *         
*           IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS         *         
*           REPEATED.                                                 *         
*                                                                     *         
*        THE SCHEMA RECORD DESCRIBES THE RANGE WHERE                  *         
*              THE NUMBER IS UNIQUE                                   *         
*                                                                     *         
*EXIT    QIO#   =  FOUND NEW SERIAL NUMBER                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS INV IO# PASSIVE             
         USING WIO#IO#D,R4                                                      
*                                                                               
NXTSERLP DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   WIO#AGY,QAGY        SET AGENCY                                   
         MVC   WIO#MED,QMED        SET MEDIA                                    
         MVC   WIO#RCD,QTYP        SET RECORD TYPE                              
*                                                                               
*        CHECK SCHEMA HERE TO SEE IF CLIENT/PUB INCLUDED IN KEY                 
*          FOR NOW ASSUME CLIENT AND PUB ARE                                    
*                                                                               
         MVC   WIO#CLT,QCLT        SET CLIENT                                   
         MVC   WIO#PUB,QPUB        SET PUB                                      
*                                                                               
         MVC   WIO#IOYR,QPER       SET YEAR OF INSORD PERIOD                    
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
         CLC   WIO#KEY(WIO#IOSQ-WIO#IO#D),KEYSAVE SKIP IF ONE FOUND             
         BE    NXTSER1             FOR YEAR                                     
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   WIO#IOSQ,=X'000001'   START SEQUENCE AT 1                        
         XC    WIO#IOSQ,=X'FFFFFF'   2'S COMPLEMENT                             
*                                                                               
         B     NXTSER2                                                          
*                                                                               
*        READ RECORD AND RESERVE NEXT IO#                                       
*                                                                               
NXTSER1  DS    0H                                                               
*                                                                               
*                                  READ FOR UPDATE AND DELETED                  
*                                                                               
         GOTOR VDMGR,DMCB,(X'88',=CL8'DMREAD'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,WIO#IOSQ       GET CURRENT SEQ NUMBER                       
         SHI   RF,1                DECREMENT BY ONE                             
         STCM  RF,7,WIO#IOSQ       RESET SEQUENCE NUMBER                        
*                                                                               
NXTSER2  DS    0H                                                               
*                                                                               
         GOTOR VDMGR,DMCB,(X'00',=CL8'DMADD'),=CL8'PRTDIR',KEY,KEY              
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSERDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSERCN DS    0H                                                               
*                                                                               
         B     NXTSERLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSERDN DS    0H                                                               
*                                                                               
         MVC   QIO#IOYR,WIO#IOYR   SAVE IO# YEAR                                
         MVC   QIO#IOSQ,WIO#IOSQ   NEXT SERIAL NUMBER                           
         XC    QIO#IOSQ,=X'FFFFFF'   COMPLEMENT                                 
*                                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT KEY                          
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
VNXTSERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VPSSVS '                         
***********************************************************************         
*                                                                     *         
*        CREATE PASSIVE POINTERS                                      *         
*                                                                     *         
*NTRY    R7 ==> MINIO SET GETTING PASSIVES                            *         
*                                                                     *         
*EXIT                                                                 *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VPSSVS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT DIRECTORY KEY                   
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    PSVOPENX                                                         
*                                                                               
         GOTO1 VMINIO,WIOPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
PSVOPENX DS    0H                                                               
*                                                                               
*        FIND DISK ADDRESS OF MASTER RECORD                                     
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS MASTER KEY                      
         USING WIOKEY,R4                                                        
*                                                                               
         MVC   WIOKEY,MINMKEY      COPY MINIO MASTER KEY                        
         MVC   WIOKELMK,=8X'FF'    SET TO MAX ELEMENT KEY                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                  READ MASTER KEY                              
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
         CLC   WIOKEY,KEYSAVE      CHECK IF KEY FOUND                           
         BE    *+6                                                              
         DC    H'0'                MUST FIND KEY                                
*                                                                               
         MVC   QIO#,WIOKIO#        SAVE INVOICE SERIAL   #                      
         MVC   QREV#,WIOKRV#       SAVE INVOICE REVISION #                      
         MVC   QDISK,WIODDISK      SAVE DISK ADDR OF MASTER REC                 
*                                                                               
*        READ HEADER ELEMENT - USED FOR MAJOR KEY FIELDS                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH WORK ELM AS HEADER                 
         USING WIOHDRD,R6                                                       
*                                                                               
         MVI   WIOHKCDE,WIOHKIDQ   SET FOR HEADER ELEMENT                       
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ HEADER ELEMENT                          
         BZ    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        ADD FIRST PASSIVE KEY                                                  
                                                                                
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS 1ST PASSIVE                     
         USING WIO1KEY,R4                                                       
*                                                                               
         MVC   WIO1AGY,WIOKAGY-WIOKEY+MINMKEY  SET AGENCY                       
         MVC   WIO1MED,WIOKMED-WIOKEY+MINMKEY  SET MEDIA                        
         MVI   WIO1RCD,WIO1RCDQ    SET PASSIVE CODE                             
         MVC   WIO1CLT,WIOKCLT-WIOKEY+MINMKEY     SET CLIENT                    
         MVC   WIO1PRD,WIOHPRD                    SET PRODUCT                   
         MVC   WIO1PUB,WIOKPUB-WIOKEY+MINMKEY     SET PUB                       
         GOTOR VDATCON,DMCB,(3,WIOHEND),(2,WIO1END)    SET END   DATE           
         GOTOR VDATCON,DMCB,(3,WIOHSTRT),(2,WIO1STRT)  SET START DATE           
         MVC   WIO1IO#,WIOKIO#-WIOKEY+MINMKEY     SET IO#                       
         MVC   WIO1RV#,WIOKRV#-WIOKEY+MINMKEY     SET IO#                       
*                                  READ PASSIVE KEY                             
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
         CLC   WIO1KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   PSVPS1NF                                                         
                                                                                
         TM    WIO1CNTL,WIODDELQ      RESTORE IF DELETED                        
         BO    *+14                                                             
         CLC   WIO1DISK,QDISK         DONE IF DISK ADDR SAME                    
         BE    PSVPS1X                                                          
                                                                                
         MVC   WIO1DISK,QDISK         ELSE SET NEW DISK ADDR                    
         NI    WIO1CNTL,X'FF'-WIODDELQ     FORCE NON-DELETED                    
                                                                                
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMWRT'),=CL8'PRTDIR',KEY,KEY              
*                                                                               
         B     PSVPS1X                                                          
                                                                                
PSVPS1NF DS    0H                  PASSIVE NOT ON FILE                          
                                                                                
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVC   WIO1DISK,QDISK      SET DISK ADDRESS OF MASTER REC               
                                                                                
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMADD'),=CL8'PRTDIR',KEY,KEY              
*                                                                               
PSVPS1X  DS    0H                                                               
*                                                                               
         GOTO1 VMINIO,WIOPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
VPSSVSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO VALIDATE STATUS - VVALSTA'                      
***********************************************************************         
*                                                                     *         
*        DISPLAY  STATUS                                              *         
*                                                                     *         
*NTRY    P0+0  L'INPUT                                                *         
*        P0    A(INPUT)                                               *         
*        P1    A(OUTPUT)                                              *         
*                                                                     *         
*EXIT    QSTAT = STATUS CODE                                          *         
*              EXPANSION OF STATUS IN OUTPUT                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               COPY PARAMETERS POINTER                      
*                                                                               
         XC    QSTAT,QSTAT         INIT SAVEAREA                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)          GET INPUT LENGTH                             
         BNZ   *+6                 CODE MUST BE FOUND                           
         DC    H'0'                                                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         L     R2,0(R3)            POINT TO INPUT                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES      FORCE UPPERCASE                              
*                                                                               
*        FIND INPUT IN STATUS TABLE                                             
*                                                                               
         L     R4,=A(STATUSTB)          POINT R4 TO TABLE                       
         A     R4,RELO                                                          
         USING STATUSTD,R4         ESTABLISH TABLE ENTRY                        
*                                                                               
VSTALOOP DS    0H                                                               
*                                                                               
         CLI   STATCDE,X'FF'       END OF TABLE?                                
         BE    VSTANOTV            CODE NOT FOUND                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),STATEXP     CHECK IF IN TABLE                            
         BE    VSTAFD                                                           
*                                                                               
VSTACONT DS    0H                                                               
*                                                                               
         LA    R4,STATUSLQ(R4)      BUMP TO NEXT ENTRY                          
         B     VSTALOOP                                                         
*                                                                               
VSTAFD   DS    0H                  STATUS IS IN TABLE                           
*                                                                               
         MVC   QSTAT,STATCDE       SAVE STATUS CODE                             
*                                                                               
VSTAOK   DS    0H                                                               
*                                                                               
         ICM   R1,15,4(R3)         POINT TO OUTPUT AREA                         
         BZ    VSTAEXPX            OKAY IF NOT GIVEN                            
*                                                                               
         LHI   RF,L'STATEXP                                                     
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STATEXP                                                  
*                                                                               
VSTAEXPX DS    0H                                                               
*                                                                               
VVALSTAX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VSTANOTV DS    0H                                                               
         LHI   RF,PPESTANV     INVALID STATUS CODE                              
         B     VSTAERR                                                          
*                                                                               
VSTAERR  DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO VALIDATE STATUS - VVALSTCD'                     
***********************************************************************         
*                                                                     *         
*        VALIDATE STATUS CODE                                         *         
*                                                                     *         
*NTRY    P0    A(OUTPUT)                                              *         
*                                                                     *         
*EXIT    QSTAT = STATUS CODE                                          *         
*              EXPANSION OF STATUS IN OUTPUT                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALSTCD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               COPY PARAMETERS POINTER                      
*                                                                               
*        FIND CODE  IN STATUS TABLE                                             
*                                                                               
         L     R4,=A(STATUSTB)          POINT R4 TO TABLE                       
         A     R4,RELO                                                          
         USING STATUSTD,R4         ESTABLISH TABLE ENTRY                        
*                                                                               
VSTCLOOP DS    0H                                                               
*                                                                               
         CLI   STATCDE,X'FF'       END OF TABLE?                                
         BE    VSTCNOTV            CODE NOT FOUND                               
*                                                                               
         CLC   QSTAT,STATCDE       CHECK IF IN TABLE                            
         BE    VSTCFD                                                           
*                                                                               
VSTCCONT DS    0H                                                               
*                                                                               
         LA    R4,STATUSLQ(R4)      BUMP TO NEXT ENTRY                          
         B     VSTCLOOP                                                         
*                                                                               
VSTCFD   DS    0H                  STATUS IS IN TABLE                           
*                                                                               
         ICM   R1,15,0(R3)         POINT TO OUTPUT AREA                         
         BZ    VSTCEXPX            OKAY IF NOT GIVEN                            
*                                                                               
         LHI   RF,L'STATEXP                                                     
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STATEXP                                                  
*                                                                               
VSTCEXPX DS    0H                                                               
*                                                                               
VVALSTCX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VSTCNOTV DS    0H                                                               
         LHI   RF,PPESTANV     INVALID STATUS CODE                              
         B     VSTCERR                                                          
*                                                                               
VSTCERR  DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO DISPLAY STATUS - VDISSTA'                       
***********************************************************************         
*                                                                     *         
*        DISPLAY STATUS NAME                                          *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
*        P0+0 = AL1(L'OUTPUT)                                         *         
*        P0==> A(OUTPUT)                                              *         
*                                                                     *         
*        QSTAT = STATUS CODE                                          *         
*                                                                     *         
*EXIT          STATUS      FILLED IN FOR SCREEN FIELD                 *         
*              STATUS      FIELD SET AS PREVIOUSLY VALIDATED          *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDISSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         OC    QSTAT,QSTAT         SKIP IF NO CODE AVAILABLE                    
         BZ    DSTAOK                                                           
*                                                                               
         L     R4,=A(STATUSTB)          POINT R3 TO TABLE                       
         A     R4,RELO                                                          
         USING STATUSTD,R4         ESTABLISH STATUS TABLE                       
*                                                                               
*        FIND CODE IN TABLE                                                     
*                                                                               
DSTALOOP DS    0H                                                               
*                                                                               
         CLI   STATCDE,X'FF'       END OF TABLE?                                
         BE    DSTAOK              CODE NOT IN TABLE                            
*                                                                               
         CLC   STATCDE,QSTAT       TEST FOR MATCH                               
         BE    DSTAFD                                                           
*                                                                               
DSTACONT DS    0H                                                               
         LA    R4,STATUSLQ(R4)     BUMP TO NEXT ENTRY                           
         B     DSTALOOP                                                         
*                                                                               
DSTAFD   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         L     R1,0(R3)               POINT TO OUTPUT AREA                      
*                                                                               
         IC    RF,0(R3)               GET LENGTH OF OUTPUT                      
*                                                                               
         CHI   RF,L'STATEXP        MAKE SURE EXPANSION FITS                     
         BNH   *+8                                                              
         LHI   RF,L'STATEXP                                                     
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STATEXP                                                  
*                                                                               
DSTAOK   DS    0H                                                               
*                                                                               
VDISSTAX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - STATUSTB'                        
***********************************************************************         
*                                                                     *         
*        STATUS TABLE                                                 *         
*              CL1    CODE                                            *         
*              CL15   EXPANSION                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
STATUSTB DS    0CL116                                                           
         DC    AL1(WIOSGENQ),CL15'GENERATED' DEFAULT                            
         DC    AL1(0),CL15'GENERATED'      DEFAULT                              
         DC    AL1(WIOSSNTQ),CL15'SENT '                                        
         DC    AL1(WIOSFWDQ),CL15'FORWARD'                                      
         DC    AL1(WIOSEXPQ),CL15'RESPTM '                                      
         DC    AL1(WIOSAPPQ),CL15'APRVD    '                                    
         DC    AL1(WIOSAPPQ),CL15'APPROVED '                                    
         DC    AL1(WIOSDLVQ),CL15'DELIVERED'                                    
         DC    AL1(WIOSREJQ),CL15'REJECTED '                                    
         DC    AL1(WIOSREJQ),CL15'REJCTD   '                                    
         DC    AL1(WIOSRSTQ),CL15'RESENT  '                                     
         DC    AL1(WIOSRSTQ),CL15'RE-SENT '                                     
         DC    AL1(WIOSUDLQ),CL15'UNDELIVERED'                                  
         DC    AL1(WIOSACCQ),CL15'ACCESS     '                                  
STATUSTX DC    X'FF'               END OF TABLE                                 
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO GET SCHEMA RECORD - VGETSCH'                    
***********************************************************************         
*                                                                     *         
*        READ SCHEMA RECORD AND SAVE VARIABLES                        *         
*                                                                     *         
*NTRY    QAREA OF WORKING STORAGE HAS REQUIRED DATA                   *         
*                                                                     *         
*        USES IOAREA1                                                 *         
*                                                                     *         
*EXIT    QAREA FIELDS FILLED IN                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VGETSCH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         XC    QSCHEMA(QSCHEMAL),QSCHEMA INIT SCHEMA DATA                       
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS SCHEMA KEY                      
         USING SCHKKEY,R4                                                       
*                                                                               
         MVC   SCHKAGY,QAGY        SET AGENCY                                   
         MVC   SCHKMED,QMED        MEDIA                                        
         MVI   SCHKRCD,SCHKRCDQ    RECORD CODE                                  
*                                                                               
         MVC   SCHKCLT,QCLT        CLIENT                                       
*                                                                               
         OC    SCHKCLT,SCHKCLT     IF ALL CLIENTS                               
         BNZ   *+10                                                             
         MVC   SCHKCLT,=8X'FF'        SET TO HIGH VALUES                        
*                                                                               
         MVC   SCHKPRD,QPRD        PRODUCT                                      
*                                                                               
         OC    SCHKPRD,SCHKPRD     IF ALL PRODUCTS                              
         BNZ   *+10                                                             
         MVC   SCHKPRD,=8X'FF'        SET TO HIGH VALUES                        
*                                                                               
         MVC   SCHKEST,QEST        ESTIMATE                                     
*                                                                               
         OC    SCHKEST,SCHKEST     IF ALL ESTIMATES                             
         BNZ   *+10                                                             
         MVC   SCHKEST,=8X'FF'        SET TO HIGH VALUES                        
*                                                                               
         MVC   SCHKPUB,QPUB        PUB                                          
*                                                                               
         OC    SCHKPUB,SCHKPUB     IF ALL PUBS                                  
         BNZ   *+10                                                             
         MVC   SCHKPUB,=8X'FF'        SET TO HIGH VALUES                        
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,DUB)  TODAY                                
         XC    DUB(3),=8X'FF'      COMPLEMENT                                   
*                                                                               
*                                  READ SCHEMA RECORD                           
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
GSCHLOOP DS    0H                                                               
*                                                                               
         CLC   SCHKKEY(SCHKCLT-SCHKKEY),KEYSAVE DONE AT END OF MEDIA            
         BNE   GSCHDONE                                                         
*                                                                               
         CLC   SCHKCLT,QCLT        MATCH ON CLIENT                              
         BE    *+14                                                             
         OC    SCHKCLT,SCHKCLT     OR ALL CLIENTS                               
         BNZ   GSCHCONT                                                         
*                                                                               
         B     GSCHFD                                                           
*                                                                               
GSCHCONT DS    0H                                                               
*                                                                               
*                                  READ NEXT RECORD                             
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMSEQ '),=CL8'PRTDIR',KEY,KEY             
*                                                                               
         B     GSCHLOOP                                                         
*                                                                               
GSCHDONE DS    0H                                                               
*                                                                               
         B     GSCHX               ALL DONE                                     
*                                                                               
GSCHFD   DS    0H                  HAVE SCHEMA RECORD                           
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'PUBFILE',KEY+27,AIO1,      X        
               DMWORK                                                           
*                                                                               
         L     R4,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               FIND HEADER ELEMENT                          
         MVI   ELCODE,SCHHDRQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   GSCHX               NOT FOUND                                    
*                                                                               
         USING SCHHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   QACCTM,SCHACCTM     SAVE ACCESS TIMEOUT                          
         MVC   QACTTM,SCHACTTM     SAVE ACTION TIME OUT                         
         MVC   QPERTYP,SCHPER      SAVE PERIOD TYPE                             
         MVC   QACTDT,SCHACTVD     SAVE ACTIVATION DATE                         
         MVC   QIO#DEP,SCHIO#TP    SAVE IO# DEPENDENCIES                        
*                                                                               
GSCHX    DS    0H                                                               
*                                                                               
         MVC   KEY,SVKEY           RESTORE ORIGINAL KEY                         
*                                  RESET DMGR POINTERS                          
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
VGETSCHX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO GET SCHEMA RECORD - VFMTIO#'                    
***********************************************************************         
*                                                                     *         
*        FORMAT IO# FOR PRINTING                                      *         
*                                                                     *         
*NTRY    QAREA OF WORKING STORAGE HAS REQUIRED DATA                   *         
*                                                                     *         
*EXIT    QAREA FIELDS FILLED IN                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VFMTIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         MVC   QIO#EXP,SPACES      INIT OUTPUT AREA                             
         LA    R2,QIO#EXP          START OF IO #                                
*                                                                               
         MVC   0(1,R2),QMED        SET MEDIA                                    
         MVI   1(R2),C'-'                                                       
         AHI   R2,2                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QIO#IOYR         GET IO# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R2),DUB         SET YEAR                                     
*                                                                               
         AHI   R2,2                BUMP TO CLIENT PART                          
         MVC   0(3,R2),QCLT        SET CLIENT                                   
         AHI   R2,2                BUMP POINTER                                 
         CLI   0(R2),C' '          IF NOT EMPTY                                 
         BNH   *+8                                                              
         AHI   R2,1                   BUMP PAST LAST OF CODE                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,QIO#IOSQ       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                ASSUME 4 DIGIT NUMBER                        
         CP    DUB,=P'9999'        IF 5 DIGIT NUMBER                            
         BNH   *+8                                                              
         LA    RF,5                   RESET LENGTH REGISTER                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R2),DUB         SEQUENCE NUMBER                              
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBBLE                
*                                                                               
         LA    R2,1(RF,R2)         NEXT OUTPUT AREA                             
*                                                                               
         OC    QREV#,QREV#         SKIP IF NO REVISION NUMBER                   
         BZ    FIO#50                                                           
*                                                                               
         MVI   0(R2),C'-'          SET DASH                                     
*                                                                               
         AHI   R2,1                NEXT OUTPUT AREA                             
*                                                                               
         MVC   0(3,R2),=C'REV'     REVISION INDICATOR                           
         AHI   R2,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QREV#          GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R2),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R2,3                NEXT OUTPUT AREA                             
*                                                                               
FIO#50   DS    0H                                                               
*                                                                               
VFMTIO#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO HEADER MAINT/LIST - VALRV#'                     
***********************************************************************         
*                                                                     *         
*        VALIDATE REVISION NUMBER                                     *         
*                                                                     *         
*NTRY    P0+0  L'INPUT                                                *         
*        P0    A(INPUT)                                               *         
*                                                                     *         
*EXIT    QREV# REVISION NUMBER                                        *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALRV#  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R3)          GET INPUT LENGTH                             
         BZ    VVALRV#X            SKIP IF NOT ENTERED                          
*                                                                               
         L     R1,0(R3)            POINT TO INPUT                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              CVD                                          
         B     *+10                                                             
         PACK  DUB,0(0,R1)         PACK                                         
*                                                                               
         TP    DUB                 MAKE SURE ITS A PACKED NUMBER                
         BNZ   VALRVER1            INVALID IF NOT                               
*                                                                               
         CVB   RF,DUB                                                           
*                                                                               
         CHI   RF,255              MAX NUMBER ALLOWED                           
         BH    VALRVER2                                                         
*                                                                               
         STC   RF,QREV#                                                         
*                                                                               
VVALRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
VALRVER1 DS    0H                                                               
         LHI   RF,PPEFLDNV     INVALID REVISION                                 
         B     VALRVERR                                                         
*                                                                               
VALRVER2 DS    0H                                                               
         LHI   RF,PPEFLDNV     INVALID REVISION                                 
         B     VALRVERR                                                         
*                                                                               
VALRVERR DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO HEADER MAINT/LIST - DISRV#'                     
***********************************************************************         
*                                                                     *         
*        DISPLAY  REVISION NUMBER                                     *         
*                                                                     *         
*NTRY    QREV#  REVISION NUMBER                                       *         
*        P0     OUTPUT                                                *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDISRV#  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETERS POINTER                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QREV#          GET REVISON NUMBER                           
         BZ    VDISRV#X            OKAY IF NONE                                 
*                                                                               
         CVD   RF,DUB              CVD                                          
*                                                                               
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         L     R1,0(R3)            POINT TO OUTPUT                              
*                                                                               
         UNPK  0(3,R1),DUB         PACK                                         
*                                                                               
VDISRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E10 - WEB IO HEADER MAINT/LIST - ACTPUT'                     
***********************************************************************         
*                                                                     *         
*        ADD ACTIVITY ELEMENT TO MINIO SET                            *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VACTPUT  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING WIOKEY,R4                                                        
*                                                                               
         XC    SVACTELM,SVACTELM   INIT NEXT ACTIVITY ELEMENT                   
*                                                                               
*        SCAN FOR MOST RECENT ACTIVITY ELEMENT                                  
*                                                                               
         LA    R6,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING WIOACTHD,R6          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   WIOAKCDE,WIOAKACQ    ACTIVITY ELEMENT ID                         
         MVI   WIOAKLEN,1           READ FOR ANY ACTIVITY ELEMENT               
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ FIRST ACTIVITY ELEMENT                 
         BNZ   ACPDONE             END OF ACTIVITY ELEMENTS                     
*                                                                               
ACPLOOP  DS    0H                                                               
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO ELEMENT                         
*                                                                               
         CLI   WIOAKCDE,WIOAKACQ   END OF ACTIVITY ELEMENTS                     
         BNE   ACPDONE                                                          
*                                                                               
         ZIC   RF,WIOAKLEN          MOVE LENGTH TO RF                           
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVACTELM(0),WIOAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
ACPCONT  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         B     ACPLOOP              IF FOUND GO INTO LOOP                       
*                                                                               
ACPDONE  DS    0H                                                               
*                                                                               
         LA    R6,SVACTELM         ESTABLISH AS ACTIVITY ELEMENT                
*                                                                               
         OC    SVACTELM,SVACTELM   IF OLD ELEMENT FOUND                         
         BZ    ACPHDR10                                                         
*                                                                               
         GOTOR GETELM,DMCB,WIOAKEY RE-READ ELM                                  
*                                                                               
         B     ACPHDRX                                                          
*                                                                               
ACPHDR10 DS    0H                                                               
*                                                                               
*        BUILD A NEW ACTIVITY ELEM                                              
*                                                                               
         XC    SVACTELM,SVACTELM   INIT ELEMENT                                 
*                                                                               
         MVI   WIOAKCDE,WIOAKACQ   SET ACTIVITY ELM ID                          
         MVI   WIOAKLEN,WIOACTLQ   SET ACTIVITY ELM LENGTH                      
*                                                                               
ACPHDRX  DS    0H                                                               
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,DUB)  TODAY                                
*                                                                               
         L     RF,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         CLC   WIOAPID,SVWIOPID    SKIP SQN BUMP IF SAME PID                    
         BNE   *+10                                                             
         CLC   WIOADTE,DUB         AND TODAY                                    
         BE    ACPSQNX                                                          
*                                                                               
*        BUMP SEQUENCE NUMBER                                                   
*                                                                               
         SR    RF,RF                CLEAR RF                                    
         ICM   RF,3,WIOAKSQN        INSERT SEQ NUMBER INTO RF                   
         AHI   RF,1                 INCREAMENT SEQ BY ONE                       
*                                                                               
         STCM  RF,3,WIOAKSQN        SEQ NUMBER INCREAMENTED BY ONE              
*                                                                               
         XC    WIOACHGS,WIOACHGS    INIT CHANGE INDICATORS                      
*                                                                               
         MVC   WIOAPID,SVWIOPID     PID OF CHANGER                              
         MVC   WIOADTE,DUB          DATE OF CHANGE - BINARY                     
*                                                                               
         L     RF,ADDELM           SET TO ADD NEW ELEMENT                       
*                                                                               
ACPSQNX  DS    0H                                                               
*                                                                               
         OC    WIOACHGS,SVACHGS     RECORD CHANGES                              
*                                                                               
         GOTOR (RF),DMCB,WIOAKEY    ADD HEADER ELEMENT                          
*                                                                               
VACTPUTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VFNDIO#'                         
***********************************************************************         
*                                                                     *         
*        FIND IO# FOR A GIVEN DATE USUALLY THAT OF AN INSERTION       *         
*                                                                     *         
*        PERIOD DATES WILL BE UPDATED TO THAT OF IO.                  *         
*                                                                     *         
*NTRY    R7 ==>    MINIO BLOCK                                        *         
*        P1     =  A(DATE)                                            *         
*                                                                     *         
*EXIT    QIO#   =  FOUND NEW SERIAL NUMBER                            *         
*        QREV#  =  CORRECT REVISION NUMBER                            *         
*        QDISK  =  DISK ADDRESS OF MASTER MINIO RECORD                *         
*        CC        ZERO     - IO FOUND                                *         
*                  NON ZERO - IO NOT FOUND                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VFNDIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         L     RF,0(R3)            POINT TO DATE                                
*                                                                               
         GOTOR VDATCON,DMCB,(0,0(RF)),(2,HALF) COMPRESS DATE                    
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS PERIOD PASSIVE              
         USING WIO1KEYD,R4                                                      
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         XC    QIOKEY,QIOKEY       INIT IOKEY SAVEAREA                          
*                                                                               
         MVC   WIO1AGY,QAGY        SET AGENCY                                   
         MVC   WIO1MED,QMED        SET MEDIA                                    
         MVI   WIO1RCD,WIO1RCDQ    SET RECORD TYPE                              
*                                                                               
         MVC   WIO1CLT,QCLT        SET CLIENT                                   
         MVC   WIO1PRD,QPRD        SET PRODUCT                                  
         MVC   WIO1PUB,QPUB        SET PUB                                      
*                                                                               
         MVC   WIO1END,HALF        SET DATE                                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=CL8'PUBDIR',KEY,KEY                     
*                                                                               
FNDIO#LP DS    0H                                                               
*                                                                               
         CLC   WIO1KEY(WIO1END-WIO1KEYD),KEYSAVE DONE IF IO NOT FOUND           
         BNE   FNDIO#DN            FOR CLT/PRD/PUB                              
*                                                                               
         CLC   HALF,WIO1END        DATE MUST BE IN IO PERIOD                    
         BH    FNDIO#DN                                                         
         CLC   HALF,WIO1STRT                                                    
         BL    FNDIO#DN                                                         
*                                                                               
         OC    QIO#,QIO#           SKIP IF NO IO# GIVEN                         
         BZ    *+14                                                             
         CLC   WIO1IO#,QIO#        FILTER ON IO#                                
         BNE   FNDIO#CN                                                         
*                                                                               
         MVC   QIOKEY,WIO1KEY      SAVE KEY                                     
*                                                                               
FNDIO#CN DS    0H                                                               
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'DMSEQ',=CL8'PUBDIR',KEY,KEY                      
*                                                                               
         B     FNDIO#LP                                                         
*                                                                               
FNDIO#DN DS    0H                                                               
*                                                                               
         OC    QIOKEY,QIOKEY       SKIP IF KEY FOUND                            
         BNZ   *+12                                                             
         LHI   R0,1                SET RETURN INDICATOR                         
         B     FNDIOX                                                           
*                                                                               
         LA    R4,QIOKEY           POINT TO FOUND RECORD                        
*                                                                               
         MVC   QDISK,WIO1DISK      SAVE MASTER RECORD DISK ADDR                 
         MVC   QIO#,WIO1IO#        SET IO#                                      
         MVC   QREV#,WIO1RV#       SET REVISION #                               
*                                                                               
         SR    R0,R0               SET RETURN CODE                              
*                                                                               
FNDIOX   DS    0H                                                               
*                                                                               
         LTR   R0,R0               SET RETURN CODE                              
*                                                                               
VFNDIO#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDELPSSV'                        
***********************************************************************         
*                                                                     *         
*        DELPSSV- DELETE PASSIVE KEYS                                 *         
*                                                                     *         
*               - ON INPUT R7 POINTS TO MINIOBLOCK                    *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDELPSSV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE INSORD KEY                 
         USING WIOKEY,R4                                                        
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    DPSOPENX                                                         
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         MVC   WIOKEY,QIOKEY       SET MASTER KEY                               
*                                                                               
         GOTOR VMINIO,DMCB,('MINOPN',MINBLKD)  OPEN MINIO SET                   
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
DPSOPENX DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR ELM AREA                              
         USING WIOHDRD,R6           INSORD HEADER ELEM                          
         MVI   WIOHKCDE,WIOHKIDQ    ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,WIOHKEY  GET ELEMENT TO RECORD                       
         BNZ   DELPSSVX                                                         
*                                                                               
         ICM   R6,15,MINELEM        POINT TO FOUND ELEMENT                      
*                                                                               
         XC    KEY,KEY             GET PASSIVE 1 KEY                            
         LA    R4,KEY              ESTABLISH PASSIVE 1 RECORD KEY               
         USING WIO1KEY,R4                                                       
*                                                                               
         MVC   WIO1AGY,WIOKAGY-WIOKEY+MINMKEY  SET AGENCY                       
         MVC   WIO1MED,WIOKMED-WIOKEY+MINMKEY  SET MEDIA                        
         MVI   WIO1RCD,WIO1RCDQ                SET PASSIVE CODE                 
         MVC   WIO1CLT,WIOKCLT-WIOKEY+MINMKEY  SET CLIENT                       
         MVC   WIO1PRD,WIOHPRD                 SET PRODUCT                      
         MVC   WIO1PUB,WIOKPUB-WIOKEY+MINMKEY  SET PUB                          
         GOTOR VDATCON,DMCB,(3,WIOHEND),(2,WIO1END)    SET END   DATE           
         GOTOR VDATCON,DMCB,(3,WIOHSTRT),(2,WIO1STRT)  SET START DATE           
         MVC   WIO1IO#,WIOKIO#-WIOKEY+MINMKEY     SET IO#                       
         MVC   WIO1RV#,WIOKRV#-WIOKEY+MINMKEY     SET RV#                       
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=CL8'PUBDIR',KEY,KEY                     
*                                                                               
         CLC   WIO1KEY,KEYSAVE     SKIP IF RECORD NOT FOUND                     
         BNE   DPSWRTX                                                          
*                                                                               
         TM    WIO1CNTL,WIODDELQ   SKIP IF ITS ALREADY DELETED                  
         BO    DPSWRTX                                                          
*                                                                               
         OI    WIO1CNTL,WIODDELQ   DELETE PASSIVE                               
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'DMWRT',=CL8'PUBDIR',KEY,KEY                      
*                                                                               
DPSWRTX  DS    0H                                                               
*                                                                               
DELPSSVX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41E00 - WEB IO PARSE IO# - VPRSIO#'                            
***********************************************************************         
*                                                                     *         
*        PARSE    WEB IO #                                            *         
*                                                                     *         
*NTRY    R2==> IO# ON SCREEN OF FORM M-CCC-YYNNNN-REVNNNN             *         
*        P0+0  L'INPUT  IF R2=0                                       *         
*        P0    A(INPUT) IF R2=0                                       *         
*                                                                     *         
*EXIT          QIOMED  - MEDIA CODE                                   *         
*              QCLT    - CLIENT CODE                                  *         
*              QIO#    - INTERNAL IO#                                 *         
*              QREV#   - REVISION NUMBER                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VPRSIO#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE A(PARAMTER LIST)                        
*                                                                               
         XC    QMED,QMED           INIT OUTPUT FIELDS - MEDIA                   
         XC    QCLT,QCLT           CLIENT                                       
         XC    QIO#,QIO#           IO#                                          
         XC    QREV#,QREV#         REVISION #                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,0(R3)             GET INPUT LENGTH                          
         BZ    PRSIO#X                NO INPUTA                                 
         L     R4,0(R3)               POINT TO INPUT                            
*                                                                               
*        MEDIA CODE IS FIRST LETTER                                             
*                                                                               
         MVC   QMED,0(R4)          SAVE MEDIA CODE                              
*                                                                               
         LA    R4,2(R4)            BUMP TO IO#                                  
*                                                                               
*        YEAR IS NEXT 2 DIGITS                                                  
*                                                                               
         PACK  DUB,0(2,R4)         PACK YEAR                                    
         AP    DUB,=P'100'         ADJUST FOR 21ST CENTURY                      
         CVB   RE,DUB              CVB                                          
         STC   RE,QIO#IOYR         SAVE YEAR                                    
*                                                                               
         LA    R4,2(R4)            BUMP TO CLIENT CODE                          
*                                                                               
*        CLIENT CODE IS NEXT 2 OR 3 POSITIONS                                   
*                                                                               
         MVC   QCLT,0(R4)          SAVE CLIENT CODE                             
*                                                                               
         CLI   QCLT+2,C'-'         IF DASH                                      
         BNE   *+8                                                              
         MVI   QCLT+2,C' '         BLANK OUT THIRD LETTER                       
*                                                                               
         LA    R4,3(R4)            BUMP TO SQN                                  
*                                                                               
*        IO# IS NEXT 4 OR 5 DIGITS                                              
*                                                                               
         LA    RF,4                ASSUME 4 DIGIT SQN                           
         CLI   4(R4),C'0'          IF FIFTH POSITION IS NUMERIC                 
         BL    *+8                                                              
         LA    RF,5                   ALLOW FOR 5 DIGITS                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         PACK                                         
*                                                                               
         TP    DUB                 MAKE SURE IT IS A PACKED NUMBER              
         BNZ   PRSIO#X                                                          
*                                                                               
         CVB   RE,DUB              CVB                                          
         STCM  RE,7,QIO#IOSQ       SAVE SEQUENCE NUMBER                         
*                                                                               
         LA    R4,1(RF,R4)         BUMP TO REVISION                             
*                                                                               
         CLC   =C'-REV',0(R4)      CHECK FOR A REVISION NUMBER                  
         BNE   PIO#OK                                                           
*                                                                               
         LA    R4,4(R4)            BUMP TO ACTUAL NUMBER                        
         PACK  DUB,0(3,R4)         PACK                                         
         TP    DUB                 MAKE SURE IT IS A PACKED NUMBER              
         BNZ   PRSIO#X                                                          
         CVB   RE,DUB              CVB                                          
         STC   RE,QREV#            SAVE REVISION NUMBER                         
*                                                                               
PIO#OK   DS    0H                                                               
*                                                                               
PRSIO#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VFNDRV#'                         
***********************************************************************         
*                                                                     *         
*        FIND LATEST REVISION NUMBER FOR AN IO                        *         
*                                                                     *         
*NTRY    R7 ==>    MINIO BLOCK                                        *         
*        QIO#   =  IO NUMBER                                          *         
*                                                                     *         
*EXIT    QREV#  =  CORRECT REVISION NUMBER                            *         
*        QDISK  =  DISK ADDRESS OF MASTER MINIO RECORD                *         
*        CC        ZERO     - RV FOUND                                *         
*                  NON ZERO - RV NOT FOUND                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VFNDRV#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         LR    R3,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         LA    R4,KEY              ESTABLISH WIO KEY                            
         USING WIOKEY,R4                                                        
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         XC    QIOKEY,QIOKEY       INIT IOKEY SAVEAREA                          
*                                                                               
         MVC   WIOKAGY,QAGY        SET AGENCY                                   
         MVC   WIOKMED,QMED        SET MEDIA                                    
         MVC   WIOKRCD,QTYP        SET RECORD TYPE                              
*                                                                               
         MVC   WIOKCLT,QCLT        SET CLIENT                                   
         MVC   WIOKPUB,QPUB        SET PUB                                      
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMRDHI'),=CL8'PRTDIR',KEY,KEY             
*                                                                               
FNDRV#LP DS    0H                                                               
*                                                                               
         CLC   WIOKEY(WIOKRV#-WIOKEY),KEYSAVE DONE IF IO NOT FOUND              
         BNE   FNDRV#DN            FOR CLT/PRD/PUB                              
*                                                                               
         MVC   QIOKEY,WIOKEY       SAVE KEY                                     
*                                                                               
FNDRV#CN DS    0H                                                               
*                                                                               
         GOTOR VDMGR,DMCB,(X'08',=CL8'DMSEQ'),=CL8'PRTDIR',KEY,KEY              
*                                                                               
         B     FNDRV#LP                                                         
*                                                                               
FNDRV#DN DS    0H                                                               
*                                                                               
         OC    QIOKEY,QIOKEY       SKIP IF KEY FOUND                            
         BNZ   *+12                                                             
         LHI   R0,1                SET RETURN INDICATOR                         
         B     FNDRVX                                                           
*                                                                               
         LA    R4,QIOKEY           POINT TO FOUND RECORD                        
*                                                                               
         MVC   QDISK,WIODDISK      SAVE MASTER RECORD DISK ADDR                 
         MVC   QREV#,WIOKRV#       SET REVISION #                               
*                                                                               
         SR    R0,R0               SET RETURN CODE                              
*                                                                               
FNDRVX   DS    0H                                                               
*                                                                               
         LTR   R0,R0               SET RETURN CODE                              
*                                                                               
VFNDRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VSETDMGR'                        
***********************************************************************         
*                                                                     *         
*        FIND SE NUMBER AND SWITCH TO THOSE FILES FOR DMGR CALLS      *         
*                                                                     *         
*NTRY    QAGY   =  2 CH ALPHA FOR AGENCY                              *         
*                                                                     *         
*EXIT    PROGRAM HAS BEEN SWITCHED TO READ CORRECT PRINT FILES        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VSETDMGR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
*        SWITCH TO CONTROL FILE                                                 
*                                                                               
         MVI   PRTSE,X'0A'         SAVE CURRENT SE                              
         MVI   DMCB,X'0A'          SET FOR CONTROL SYSTEM                       
         MVC   DMCB+1(3),=3X'FF'   INDICATE HARD SWITCH                         
*                                                                               
         GOTO1 VSWITCH,DMCB,,0     SWITCH TO CONTROL SYSTEM                     
         CLI   4(R1),0                                                          
         BNE   SETDERR1               CAN'T SWITCH                              
*                                                                               
*        READ SYSTEM ACCESS RECORD FOR PASSED AGENCY                            
*                                                                               
         LA    R4,KEY              ESTABLISH CT5REC (SYSTEM ACCESS REC)         
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,QAGY       SET AGENCY CODE                              
*                                                                               
         GOTO1 VDMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',KEY,AIO1,0 READ             
         CLI   8(R1),0             NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,CT5DATA          POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
CTAUTHLP DS    0H                                                               
*                                                                               
         USING CTSYSD,R6           ESTABLISH SYS AUTH ELEMENT                   
*                                                                               
         CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   CTAUTHCN                                                         
*                                                                               
         CLI   CTSYSNUM,4          MUST BE FOR PRINT SYSTEM                     
         BE    CTAUTHFD                                                         
*                                                                               
CTAUTHCN DS    0H                                                               
*                                                                               
         IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     CTAUTHLP                                                         
*                                                                               
CTAUTHFD DS    0H                                                               
*                                                                               
         MVC   PRTSE,CTSYSSE       SAVE PRINT SE NUMBER                         
*                                                                               
*        SWITCH TO PRINT FILE                                                   
*                                                                               
         MVC   DMCB(1),PRTSE       SET FOR PRINT SYSTEM                         
         MVC   DMCB+1(3),=3X'FF'   INDICATE HARD SWITCH                         
*                                                                               
         GOTO1 VSWITCH,DMCB,,0     SWITCH TO PRINT SYSTEM                       
         CLI   4(R1),0                                                          
         BNE   SETDERR1               CAN'T SWITCH                              
*                                                                               
SETDMGRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
SETDERR1 DS    0H                  SYSTEM UNAVAILABLE                           
*                                                                               
         MVC   PERROR,=X'FE01'     SET ERROR NUMBER                             
*                                                                               
         GOTOR VERREXIT                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDITIM'                          
***********************************************************************         
*                                                                     *         
*        DISPLAYS BINARY XL3 TIME AS HH:MM:SS                         *         
*                                                                     *         
*NTRY   P0      A(BINARY TIME FIELD)                                  *         
*       P1      A(OUTPUT)                                             *         
*                                                                     *         
*EXIT           TIME AS HH:MM:SS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISTIM  NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         L     R3,0(R1)            A(BINARY TIME)                               
         L     R2,4(R1)            GET A(OUTPUT)                                
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         IC    RF,0(R3)            GET HOURS                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C':'                                                       
*                                                                               
         IC    RF,1(R3)            GET MINUTES                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  3(2,R2),DUB                                                      
         MVI   5(R2),C':'                                                       
*                                                                               
         IC    RF,2(R3)            GET SECONDS                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  6(2,R2),DUB                                                      
*                                                                               
VDISTIMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VFNDGP#'                         
***********************************************************************         
*                                                                     *         
*        FIND MOST RECENT FAX/EMAIL GROUP #                           *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
*                                                                     *         
*EXIT    QFGP# SET                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VFNDGP#  NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7                                                       
*                                                                               
*        FIND LAST GROUP HEADER ELEMENT                                         
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING WIOFAXD,R6          ESTABLISH FAX/EMAIL HEADER KEY               
*                                                                               
         XC    SVFAXELM,SVFAXELM   INIT ELEMENT SAVEAREA                        
*                                                                               
*        FIND CURRENT FAX/EMAIL HEADER ELEMENT                                  
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET ELEMENT ID                               
         MVI   WIOFKGP#,1          LOOK FOR FIRST HEADER                        
         MVI   WIOFKTYP,WIOFKHDQ   LOOK FOR HEADER ELEMENT                      
         MVI   WIOFKLEN,WIOFKSQN-WIOFKEY MATCH ON GROUP HEADER TYPE             
*                                                                               
FNDGP#LP DS    0H                                                               
*                                                                               
         GOTOR GETELM,DMCB,WIOFKEY FIND FIRST FAX/EMAIL ELEMENT                 
         BNZ   FNDGP#DN            NOT FOUND                                    
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLI   WIOFKCDE,WIOFKIDQ   DONE IF NOT A FAX/EMAIL ELEMENT              
         BNE   FNDGP#DN                                                         
*                                                                               
         CLI   WIOFKTYP,WIOFKHDQ   SKIP IF NOT A FAX/EMAIL HEADER               
         BNE   FNDGP#CN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKLEN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVFAXELM(0),WIOFKEY SAVE FAX/EMAIL ELEMENT                       
*                                                                               
         OC    WIOFGPID,WIOFGPID   IF THERE IS A GROUP ID                       
         BZ    FNDGP#CN                                                         
*                                                                               
         CLC   WIOFGPID,QGRPID        OKAY IF GRP IDS MATCH                     
         BE    FNDGP#FD                                                         
*                                                                               
FNDGP#CN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKGP#         GET FOUND GROUP NUMBER                       
         LA    R6,ELEMENT          RE-POINT TO SEARCH KEY                       
         AHI   RF,1                BUMP TO NEXT GROUP NUMBER                    
         STC   RF,WIOFKGP#         SET NEXT POSSIBLE GROUP NUMBER               
*                                                                               
         B     FNDGP#LP            SEARCH FOR NEXT GROUP                        
*                                                                               
FNDGP#DN DS    0H                  FAX GROUP NOT FOUND                          
*                                                                               
         OC    SVFAXELM,SVFAXELM   IF THERE IS AT LEAST ONE FAX HEADER          
         BZ    *+14                                                             
         OC    QGRPID,QGRPID          USE IT IF THERE IS NO GROUP ID            
         BZ    FNDGP#FD                                                         
*                                                                               
*        CREATE NEW GROUP                                                       
*                                                                               
         LA    R6,SVFAXELM         POINT TO LAST HEADER FOUND                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,WIOFKGP#         GET LATEST GROUP NUMBER                      
*                                  MAYBE 0 IF NO FAX/EMAIL ELMS FOUND           
         AHI   RF,1                BUMP SQN BY ONE                              
*                                                                               
         XC    SVFAXELM,SVFAXELM   CLEAR WORKAREA                               
*                                                                               
*        BUILD NEW FAX HEADER ELEMENT                                           
*                                                                               
         MVI   WIOFKCDE,WIOFKIDQ   SET AS FAX/EMAIL ELEMENT                     
         MVI   WIOFKLEN,WIOFHDRL   SET ELEMENT LENGTH                           
         STC   RF,WIOFKGP#         SET GROUP NUMBER                             
         MVI   WIOFKTYP,WIOFKHDQ   SET GROUP HEADER ID                          
*                                                                               
         MVC   WIOFGPID,QGRPID     SET GROUP ID IN HEADER                       
*                                                                               
         GOTOR ADDELM,DMCB,WIOFAXD ADD ELEMENT TO RECORD                        
*                                                                               
FNDGP#FD DS    0H                  FAX GROUP FOUND                              
*                                                                               
         LA    R6,SVFAXELM         POINT TO MOST RECENT HEADER ELM              
*                                                                               
         MVC   QFGP#,WIOFKGP#      SAVE CURRENT GROUP NUMBER                    
*                                  MAYBE 0 IF NO FAX/EMAIL ELMS FOUND           
VFNDGP#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - VDGTFX#'                         
***********************************************************************         
*                                                                     *         
*        MAKE FAX # ALL DIGITS                                        *         
*                                                                     *         
*NTRY    P0+0  LENGTH OF INPUT NUMBER                                 *         
*        P0    A(FAX #)                                               *         
*        P1+0  LENGTH OF OUTPUT AREA                                  *         
*        P1    A(OUTPUT)                                              *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*EXIT    OUTPUT HAS FAX# AS ALL NUMBERS LEFT JUSTIFIED                *         
*        P1+0   LENGTH OF OUTPUT                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDGTFX#  NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING MINBLKD,R7                                                       
*                                                                               
         LR    R9,R1               SAVE A(PARAMETER LIST)                       
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R9)          GET LENGTH OF INPUT                          
         L     RE,4(R9)            POINT TO OUTPUT AREA                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,4(R9)            GET LENGTH OF OUTPUT AREA                    
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       INIT OUTPUT AREA                             
*                                                                               
         L     RF,0(R9)            POINT TO INPUT                               
*                                                                               
DGFX#LP  DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    DGFX#CN                                                          
         CLI   0(RF),C'9'                                                       
         BH    DGFX#CN                                                          
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
DGFX#CN  DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R1,DGFX#LP                                                       
*                                                                               
DGFX#DN  DS    0H                                                               
*                                                                               
         LA    RF,4(R9)            CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
         STC   RE,4(R9)            SAVE INPUT FAX # LENGTH                      
*                                                                               
VDGTFX#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - STATUSTD'                        
***********************************************************************         
*                                                                     *         
*        STATUS TABLE DSECT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STATUSTD DSECT                     STATUS TABLE DSECT                           
STATCDE  DS    CL1                 STATUS CODE                                  
STATEXP  DS    CL15                STATUS EXPANSION                             
STATUSLQ EQU   *-STATUSTD          LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - URLTABD'                         
***********************************************************************         
*                                                                     *         
*        URL DATA TYPE TABLE DSECT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
URLTABD  DSECT                     URL DATA TYPE TABLE DSECT                    
URLTTYPE DS    CL10                DATA TYPE                                    
URLTDISP DS    AL1                 DISP TO DATA SAVEAREA                        
URLTABL  EQU   *-URLTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - SCRTBLD'                         
***********************************************************************         
*                                                                     *         
*        SCREEN TABLE DSECTS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SCRTBLD  DSECT                     SCREEN TABLE ENTRY                           
SCTBPF1  DS    AL1                 FIRST PFKEY                                  
SCTBPF2  DS    AL1                 SECOND PFKEY                                 
SCTBREC  DS    CL8                 RECORD                                       
SCTBACT  DS    CL8                 ACTION                                       
SCRTBLLQ EQU   *-SCRTBLD           ENTRY LENGTH                                 
*                                                                               
         TITLE 'PRINT MQ SERVICE CALL - EZMQMSGD'                               
***********************************************************************         
*                                                                     *         
*        EDICT RETURNED MQ MESSAGE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EZMQMSGD DSECT                     DATA RETURNED BY EDICT VIA MQ                
EZMLABEL DS    CL6                 PRTADB                                       
         DS    CL2                 SPARE                                        
EZMUSER  DS    CL10                REPORT ID - WIO,123                          
EZMSTAT  DS    CL1                 D FOR DELIVERY, C FOR CANCEL                 
EZMDATE  DS    CL5                 MMMDD                                        
EZMTIME  DS    CL4                 HHMM                                         
EZMAPPL  DS    CL58                APPLICATION INFO FROM TRN CARD               
EZMEMSG  DS    CL24                ERROR MESSAGE                                
EZDEST   DS    CL16                DESTINATION                                  
EZMQMSGL EQU   *-EZMQMSGD                                                       
*                                                                               
         TITLE 'PRINT MQ SERVICE CALL - WORKD'                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
SYSRD    DS    F                   INCOMING RD SAVEAREA                         
*                                                                               
SRPARMS  DS    0XL32               SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TRANSLATOR I/O BLOCK)                      
*                                                                               
RELO     DS    A                                                                
*                                                                               
DMCB     DS    6F                  DATAMGR CONTROL BLOCK                        
DMWORK   DS    12D                 DATAMGR WORKAREA                             
*                                                                               
MQMSGLN  DS    H                   MQ MESSAGE LENGTH                            
         DS    H                   SPARE                                        
*                                                                               
KEY      DS    XL32                KEY                                          
KEYSAVE  DS    XL32                                                             
*                                                                               
AIO      DS    A                   A(CURRENT IOAREA)                            
AIO1     DS    A                   A(IOAREA 1)                                  
AIO2     DS    A                   A(IOAREA 2)                                  
AIO3     DS    A                   A(IOAREA 3)                                  
*                                                                               
WORK     DS    XL256               WORKAREA                                     
*                                                                               
ELEMENT  DS    XL256               ELEMENT WORKAREA                             
SVNULLS  DS    XL1                 ENDING NULLS FOR COMMENTS                    
*                                                                               
ELCODE   DS    XL1                 WORKAREA FOR GETEL                           
*                                                                               
ERROR    DS    XL1                 1-BYTE ERROR CODE                            
PERROR   DS    XL2                 2-BYTE ERROR CODE                            
*                                                                               
FX#FILE  DS    XL32                STORAGE FOR FILE  FAX # AS DIGITS            
FX#INPUT DS    XL32                STORAGE FOR INPUT FAX # AS DIGITS            
*                                                                               
         TITLE 'PRINT MQ SERVICE CALL - WORKD'                                  
***********************************************************************         
*                                                                     *         
*        WEB IO SYSTEM WORKING STORAGE                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSV     DS    0D                  EXTERNAL ADDRESSES                           
VRECUP   DS    V                                                                
VDMGR    DS    V                                                                
VPERVAL  DS    V                                                                
VDATCON  DS    V                                                                
VSWITCH  DS    V                                                                
AJESMAIL DS    V                                                                
ACOMFACS DS    V                                                                
VGETTXT  DS    V                                                                
         DS    10V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
SYSCOMM  DS    0A                  COMMON FACILITIES FOR SYSTEM                 
VALMED   DS    V                   VALIDATE MEDIA                               
DISMED   DS    V                   DISPLAY  MEDIA                               
VALCLT   DS    V                   VALIDATE CLIENT                              
DISCLT   DS    V                   DISPLAY  CLIENT                              
VALPRD   DS    V                   VALIDATE PRODUCT                             
DISPRD   DS    V                   DISPLAY  PRODUCT                             
VALPER   DS    V                   VALIDATE PERIOD                              
DISPER   DS    V                   DISPLAY  PERIOD                              
VALPUB   DS    V                   VALIDATE PUB                                 
DISPUB   DS    V                   DISPLAY  PUB                                 
MININIT  DS    V                   INITIALIZE MINIO SET                         
ADDELM   DS    V                   ADD ELEMENT TO MINIO SET                     
WRTELM   DS    V                   REPLACE ELEMENT IN MINIO SET                 
DELELM   DS    V                   DELETE ELEMENT FROM MINIO SET                
GETELM   DS    V                   GET ELEMENT IN MINIO SET                     
NXTELM   DS    V                   FIND NEXT ELEMENT IN MINIO SET               
PRVELM   DS    V                   FIND PREVIOUS ELEMENT IN MINIO SET           
ERREXIT  DS    V                   ERROR HANDLING W/CURSOR POSITIONING          
PSSVS    DS    V                   CREATE PASSIVE POINTERS                      
VALSTA   DS    V                   VALIDATE STATUS                              
VALSTCD  DS    V                   VALIDATE STATUS CODE                         
DISSTA   DS    V                   DISPLAY  STATUS                              
GETSCH   DS    V                   GET SCHEMA RECORD                            
FMTIO#   DS    V                   FORMAT IO#                                   
VALRV#   DS    V                   VALIDATE REVISION #                          
DISRV#   DS    V                   DISPLAY REVISION #                           
ACTPUT   DS    V                   ADD ACTIVITY ELEMENT                         
FNDIO#   DS    V                   FIND IO# FOR DATE                            
PRSIO#   DS    V                   PARSE EXPANDED IO# INTO COMPONENTS           
FNDRV#   DS    V                   FIND LATEST REVISION # FOR IO                
SETDMGR  DS    V                   SET DATAMGR TO PRINT FILES                   
DISTIM   DS    V                   DISPLAY TIME                                 
FNDGP#   DS    V                   FIND CURRENT GROUP NUMBER                    
DGTFX#   DS    V                   MAKE FAX# ALL DIGITS                         
         DS    38V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
COREFACS DS    0A                  CORE-RESIDENT PHASES                         
VMINIO   DS    V                   MINIO                                        
VPUBVAL  DS    V                   PUBVAL                                       
VPUBEDIT DS    V                   PUBEDIT                                      
         DS    20V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
ATBUFF   DS    A                   A(T BUFFER)                                  
ASYSFACS DS    A                                                                
ASSB     DS    A                                                                
AUTL     DS    A                                                                
*                                                                               
*        MINIO CONTROL BLOCK                                                    
*                                                                               
MNBLKCB  DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
         DS    0D                  ALIGNMENT                                    
MNRTAB   DS    CL256               MINIO RECORD TABLE                           
MNELEM   DS    CL256               MINIO ELEMENT AREA                           
*                                                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
WIOPARMS DS    XL24                PARAMETER LIST                               
GETTXTCB DS    XL(L'GTBLOCK)       GETTXT WORKAREA                              
*                                                                               
* FIELD VALIDATION STORAGE                                                      
*                                                                               
USEIONUM DS    X                   INDICATOR FOR WHICH AIO TO USE               
*                                                                               
PRTSE    DS    XL1                 SE NUMBER FOR PRINT FILES                    
*                                                                               
SVMQION  DS    F                   TRACE SWITCH                                 
*                                                                               
         DS    CL45                SPARE                                        
*                                                                               
SPACES   DS    XL132                                                            
*                                                                               
WBTLEN   DS    PL4                 TOTAL LENGTH OF WEBIO DATA                   
*                                                                               
* EXTRACT AREAS                                                                 
*                                                                               
*        THESE VALUES NEED TO BE MAINTAINED WITH THE CURRENT                    
*        VALUES                                                                 
*                                                                               
         DS    0D                  ALIGNMENT                                    
QAGY     DS    CL2                 AGENCY ALPHA                                 
QTYP     DS    XL1                 MINIO RECORD CODE FOR EIO OR ESR             
QMED     DS    CL1                 MEDIA CODE                                   
QCLT     DS    CL3                 CLIENT CODE                                  
QPRD     DS    CL3                 PRODUCT CODE                                 
QEST     DS    CL3                 ESTIMATE RANGE START                         
QESTEND  DS    CL3                 ESTIMATE RANGE END                           
QPUB     DS    XL6                 PUB NUMBER                                   
QDIV     DS    CL3                 DIVISION CODE                                
QREG     DS    CL3                 REGION CODE                                  
QDST     DS    CL3                 DISTRICT CODE                                
QREP     DS    CL4                 REP      CODE                                
QSTART   DS    CL6                 YYMMDD START DATE                            
QEND     DS    CL6                 YYMMDD END DATE                              
QPER     DS    XL6                 PERIOD START-END - BINARY                    
QRDATE   DS    XL3                 INSORD RUN DATE                              
QSTAT    DS    CL1                 STATUS                                       
QSTADATE DS    XL3                 STATUS DATE - BINARY                         
QSTATIME DS    XL3                 STATUS TIME - BINARY - HH:MM:SS              
QIPADDR  DS    CL16                IPADDRESS                                    
QUSERID  DS    CL10                SIGN ON USER ID                              
QPERSID  DS    CL10                PERSON ID                                    
QADCODE  DS    CL6                                                              
QIOKEY   DS    CL32                CURRENT MINIO MASTER KEY                     
*                                                                               
QIO#     DS    0XL4                WEB IO SERIAL NUMBER                         
QIO#IOYR DS    XL1                 WEB IO SERIAL NUMBER - YEAR                  
QIO#IOSQ DS    XL3                 WEB IO SERIAL NUMBER - SEQ #                 
*                                                                               
QREV#    DS    XL1                 WEB IO REVISION NUMBER                       
QFGP#    DS    XL1                 FAX GROUP NUMBER                             
QFSQN    DS    XL2                 FAX SEQUENCE NUMBER                          
*                                                                               
QIO#SHRT DS    CL18                IO# - SHORT FORM                             
*                                  MYYCCC###-###                                
*                                  CCC - CLIENT CODE                            
*                                                                               
QIO#EXP  DS    CL20                EXPANDED IO#                                 
*                                  M-YYCCC0000(0)-REV000                        
*                                                                               
QSTEW    DS    XL1                 STEWARDSHIP OPTION                           
         DS    XL7                                                              
QDSQN    DS    XL2                 DETAIL SEQUENCE NUMBER                       
QDISK    DS    XL4                 DISK ADDR OF MINIO MASTER REC                
*                                                                               
QSSQN    DS    XL2                 STATUS SEQUENCE NUMBER                       
*                                                                               
QFXTYP   DS    XL1                 FAX TYPE - FYI?                              
*                                                                               
QGRPID   DS    XL12                GROUP ID                                     
         DS    XL35                SPARE                                        
*                                                                               
QACTCHGS DS    0XL6                CHANGES INDICATORS                           
QACTCHG1 DS    X                                                                
QACTADD  EQU   X'80'               IO HEADER ADDED                              
QACTDEL  EQU   X'40'               IO HEADER DELETED                            
QACTRST  EQU   X'20'               IO RESTORED                                  
QACTSTAT EQU   X'10'               TOTAL TYPE  G/N                              
QACTVCCL EQU   X'08'               VENDOR CONTACT LIST                          
QACTACCL EQU   X'04'               AGENCY CONTACT LIST                          
*                                                                               
QACTCHG2 DS    X                                                                
QACTCHG3 DS    X                                                                
QACTCHG4 DS    X                                                                
QACTCHG5 DS    X                                                                
QACTCHG6 DS    X                                                                
*                                                                               
         DS    XL48                SPARE                                        
*                                                                               
*        SCHEMA RECORD FIELDS                                                   
*                                                                               
         DS    0D                  ALIGNMENT                                    
QSCHEMA  DS    0X                  SCHEMA DATA                                  
QACCTM   DS    XL3                 ACCESS   TIMEOUT PERIOD                      
QACTTM   DS    XL3                 ACTIVATE TIMEOUT PERIOD                      
QPERTYP  DS    CL1                 IO PERIOD TYPE                               
QACTDT   DS    XL3                 ACTIVATION DATE - BINARY                     
QIO#DEP  DS    XL1                 IO # DEPENDENCIES                            
         DS    XL23                SPARE                                        
QSCHEMAL EQU   *-QSCHEMA           LENGTH OF SCHEMA DATA                        
*                                                                               
QRECORD  DS    XL8                 RECORD TYPE BEING PROCESSED                  
         DS    XL48                SPARE                                        
*                                                                               
MEDNM    DS    CL10                MEDIA NAME                                   
CLTNM    DS    CL20                CLIENT NAME                                  
PRDNM    DS    CL20                PRODUCT NAME                                 
ESTNM    DS    CL20                ESTIMATE NAME                                
PUBNM    DS    CL20                PUB NAME                                     
REPNM    DS    CL20                REP NAME                                     
DDCOMSW  DS    CL1                 C'Y' - COMMENTS TO BE ADDED                  
*                                  C'C' - COMMENTS TO BE ADDED NOW              
*                                  C'D' - COMMENTS ADDED                        
         DS    XL119               SPARE                                        
*                                                                               
SVSECAGY DS    XL2                 SECURITY AGENCY                              
SVWIOPID DS    XL2                 PERSONAL ID                                  
SVWIOID  DS    CL8                 PERSON   ID IN CHARACTER                     
*                                                                               
SVKEY    DS    XL32                USE WHEN BREAKING READ SEQUENCE              
SVPROF   DS    CL16                                                             
SVUSER   DS    CL66                SAVE AREA FOR AGYNAME/ADDR                   
*                                                                               
SVCPROF  DS    CL32                                                             
SVCLTOFC DS    CL1                 CLIENT OFFICE SAVEAREA                       
*                                                                               
SVAGYPF  DS    CL30                AGENCY PROFILE                               
SVACTELM DS    XL256               ACTIVITY ELEMENT BUILD AREA                  
SVACHGS  DS    0XL6                ACTIVITY ACCUMULATORS                        
SVACH1   DS    XL1                 ACTIVITY ACCUMULATOR 1                       
SVACH2   DS    XL1                 ACTIVITY ACCUMULATOR 2                       
SVACH3   DS    XL1                 ACTIVITY ACCUMULATOR 3                       
SVACH4   DS    XL1                 ACTIVITY ACCUMULATOR 4                       
SVACH5   DS    XL1                 ACTIVITY ACCUMULATOR 5                       
SVACH6   DS    XL1                 ACTIVITY ACCUMULATOR 6                       
*                                                                               
SVSTAELM DS    XL256               STATUS ELEMENT SAVEAREA                      
SVURLELM DS    0XL256              URL    ELEMENT SAVEAREA                      
SVFAXELM DS    XL256               FAX    ELEMENT SAVEAREA                      
*                                                                               
*        JESMAIL PARAMTER BLOCK                                                 
*                                                                               
         DS    0F                                                               
SMTPC    DS    XL(SMTPDQ)          PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
*        E-MAIL FIELDS                                                          
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL60                TO: E-MAIL ADDRESS                           
EMLTOEND DS    XL1                 X'FF' END OF LIST                            
EMLSUBJ  DS    CL70                SUBJECT                                      
EMLMSG   DS    10CL80              MESSAGE                                      
         ORG   EMLMSG                                                           
EMLLIN1  DS    CL80                  LINE 1                                     
EMLLIN2  DS    CL80                  LINE 2                                     
EMLLIN3  DS    CL80                  LINE 3                                     
EMLLIN4  DS    CL80                  LINE 4                                     
EMLLIN5  DS    CL80                  LINE 5                                     
EMLLIN6  DS    CL80                  LINE 6                                     
EMLLIN7  DS    CL80                  LINE 7                                     
EMLLIN8  DS    CL80                  LINE 8                                     
EMLLIN9  DS    CL80                  LINE 9                                     
EMLLIN10 DS    CL80                  LINE 10                                    
         DS    XL1                 POSSIBLE EOL MARKER                          
         ORG                                                                    
SYSSPARE EQU   *                                                                
         DS    CL(6000-(*-WORKD))                                               
*                                                                               
IOAREA1  DS    XL4096              I/O AREAS                                    
IOAREA2  DS    XL4096                                                           
IOAREA3  DS    XL4096                                                           
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - WBMQD'                           
***********************************************************************         
*                                                                     *         
*        LAYOUT OF DATA FROM WEBIO                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WBMQD    DSECT                                                                  
WBMQ     DS    CL8'PRTADB'         PRINTPAK IDENTIFIER                          
WBMQWBID DS    CL5'WEBIO'          WEBIO DATA INDICATOR                         
*                                                                               
WBMQAGY  DS    CL2                 AGENCY                                       
WBMQIO#  DS    CL20                IO#                                          
WBMQPUB  DS    CL20                PUB NUMBER                                   
WBMQDATA DS    0C                  START OF DATA                                
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - DATAD'                           
***********************************************************************         
*                                                                     *         
*        LAYOUT OF DATA FROM WEBIO                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DATAD    DSECT                                                                  
DATAID   DS    CL6                 DATA IDENTIFIER                              
DATADATA DS    0C                  START OF DATA                                
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - SUBDD'                           
***********************************************************************         
*                                                                     *         
*        LAYOUT OF SUB DATA FROM WEBIO                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBDD    DSECT                                                                  
SUBDID   DS    CL10                SUBDATA IDENTIFIER                           
SUBDLEN  DS    CL4                 DATA LENGTH                                  
SUBDDATA DS    0C                  START OF DATA                                
*                                                                               
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - DSECTS'                          
***********************************************************************         
*                                                                     *         
*        VARIOUS DSECTS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
** DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
** PPEDICT                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPEDICT                                                        
         PRINT ON                                                               
** DDCOREQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
** PPERREQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS                                                      
         PRINT ON                                                               
** FAFACTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
** DDGLOBEQUS                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
** DDOFFICED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
** PRGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
** DDMINBLK                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
** FAGETTXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
** FASECRETD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
** FAJESMAILD                                                                   
         PRINT OFF                                                              
       ++INCLUDE FAJESMAILD                                                     
         PRINT ON                                                               
** DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
** PPMAPEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPMAPEQUS                                                      
         PRINT ON                                                               
** CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
** SEACSFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SRPMQ00   05/11/16'                                      
         END                                                                    
