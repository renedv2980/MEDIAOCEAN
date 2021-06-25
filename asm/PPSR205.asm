*          DATA SET PPSR205    AT LEVEL 014 AS OF 09/02/14                      
*PHASE T42105A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42105 - ENHANCED SPACE RESERVATION, ROUTINES'                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 06/19/14 VENDOR CONTACT LIST - HONOR ALL PUB LEVEL                       
*                                                                               
* KWAN 07/08/11 ESR BY PERIOD FIX - DATE RANGE IS LESS THAN 30 DAYS             
*                                                                               
* KWAN 06/01/11 DATE RANGE PASSIVE KEY FIX FOR ESR BY EST OR BY PERIOD          
*                                                                               
* KWAN 05/18/11 REQUEST DATES NOT STARTING 1ST & LAST DAY OF MONTH FIX          
*                                                                               
* KWAN 03/22/11 ESR BY ESTIMATE PERIOD                                          
*                                                                               
* KWAN 11/25/08 AB2 PROFILE FOR PUB E-MAILS AND FAXES                           
*                                                                               
* KWAN 05/30/07 IO# CHANGES FOR ESR BY ESTIMATE                                 
*                                                                               
* KWAN 05/24/06 SR# CHANGES FOR STEWARD SPACE RESERVATIONS                      
*                                                                               
* KWAN 11/08/05 FIX CONTRACT INFORMATION REPLY                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T42105   CSECT                                                                  
         NMOD1 PPSR205X-PPSR205D,T42105,RR=RE,CLEAR=YES                         
*                                                                               
         LR    R9,RC                                                            
         USING PPSR205D,R9                                                      
*                                                                               
         ST    RE,RELO05                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         LR    R8,RC                                                            
         A     R8,=A(POLWRKX-POLWRKD)                                           
         USING ESRWORKD,R8                                                      
         USING T421FFD,RA                                                       
*                                                                               
         BRAS  RE,INITWKST                                                      
*                                                                               
         CLI   8(R1),ACCLISTQ      AGENCY CONTACT LIST?                         
         BE    ACCLISTR                                                         
         CLI   8(R1),VCCLISTQ      VENDOR CONTACT LIST?                         
         BE    VCCLISTR                                                         
         CLI   8(R1),SETUPRCQ      SETUP RECORD?                                
         BE    SETUPRCR                                                         
         CLI   8(R1),PRCESR#Q      WEB IO NUMBER AND REVISION?                  
         BE    PRCESR#R                                                         
         CLI   8(R1),PUTESR#Q      WRITE BACK WEB IO RECORD?                    
         BE    PUTESR#R                                                         
*                                                                               
         DC    H'0'                INVALID CALL                                 
*                                                                               
ACCLISTR BRAS  RE,GETACCLS         GET FYI (AGENCY) CONTACT LIST                
         B     EXXMODX                                                          
*                                                                               
VCCLISTR BRAS  RE,GETVCCLS         GET VENDOR CONTACT LIST                      
         B     EXXMODX                                                          
*                                                                               
SETUPRCR BRAS  RE,GETSETUP         GET SETUP RECORD                             
         B     EXXMODX                                                          
*                                                                               
PRCESR#R BRAS  RE,GETESR#R         GET WEB IO NUMBER AND REVISION               
         B     EXXMODX                                                          
*                                                                               
PUTESR#R BRAS  RE,WRIESR#R         WRITE WEB IO RECORD                          
         B     EXXMODX                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXXMODX  XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTEL                                                            
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVMODE,8(R1)      SAVE CALLING MODE                            
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         ST    RF,VMINIO                                                        
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1-PPSR205D)                                           
         ST    RE,AWKAIO1                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO2-PPSR205D)                                           
         ST    RE,AWKAIO2                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO3-PPSR205D)                                           
         ST    RE,AWKAIO3                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO4-PPSR205D)                                           
         ST    RE,AWKAIO4                                                       
*                                                                               
         OC    QSTART,QSTART       DATES ARE SET?                               
         BZ    INITWKX                                                          
         GOTOR DATCON,DMCB,(0,QSTART),(3,QPERSTR)                               
         GOTOR DATCON,DMCB,(0,QEND),(3,QPEREND)                                 
         GOTOR DATCON,DMCB,(0,QSTART),(2,QPERCSTR)                              
         GOTOR DATCON,DMCB,(0,QEND),(2,QPERCEND)                                
*                                                                               
INITWKX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETACCLS NTR1  BASE=*,LABEL=*       GET FYI (AGENCY) CONTACT LIST               
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVDMIN,DMINBTS                                                 
         NI    DMINBTS,X'FF'-X'08' NO DELETED RECORDS                           
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SRAKEY,R2                                                        
         MVC   SRAKAGY,QAGENCY      CK FOR MED/CLT/PRD/PUB                      
         MVC   SRAKMED,QMEDIA                                                   
         MVI   SRAKRCD,SRAKRCDQ                                                 
         MVC   SRAKCLT,QCLIENT                                                  
         MVC   SRAKPRD,QPRODUCT                                                 
         MVC   SRAKPUB,BPUB                                                     
         MVC   WKTMP1(L'KEY),KEY                                                
*                                                                               
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/CLT/PRD/ALL PUB (X'FF')                  
         MVC   SRAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/CLT/PRD/ALL PUB (X'00')                  
         XC    SRAKPUB,SRAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          CK MED/CLT/ALL PRD/PUB                       
         MVC   SRAKPRD,=6X'FF'                                                  
         MVC   SRAKPUB,BPUB                                                     
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/CLT/ALL PRD/ALL PUB (X'FF')              
         MVC   SRAKPRD,=6X'FF'                                                  
         MVC   SRAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/CLT/ALL PRD/ALL PUB (X'00')              
         MVC   SRAKPRD,=6X'FF'                                                  
         XC    SRAKPUB,SRAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          CK MED/ALL CLT/ALL PRD/PUB                   
         MVC   SRAKCLT,=6X'FF'                                                  
         MVC   SRAKPRD,=6X'FF'                                                  
         MVC   SRAKPUB,BPUB                                                     
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
*                                                                               
         MVC   KEY,WKTMP1          MED/ALL CLT/ALL PRD/ALL PUB (X'FF')          
         MVC   SRAKCLT,=6X'FF'                                                  
         MVC   SRAKPRD,=6X'FF'                                                  
         MVC   SRAKPUB,=6X'FF'                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BE    GTACC30                                                          
         MVC   KEY,WKTMP1          MED/ALL CLT/ALL PRD/ALL PUB (X'00')          
         MVC   SRAKCLT,=6X'FF'                                                  
         MVC   SRAKPRD,=6X'FF'                                                  
         XC    SRAKPUB,SRAKPUB                                                  
         GOTOR HIGH                                                             
         CLC   KEY(SRAKPID-SRAKEY),KEYSAVE                                      
         BNE   GTACCX                                                           
*                                                                               
GTACC30  BRAS  RE,RPYCCLST         REPLY CONTACT LIST                           
*                                                                               
GTACCX   MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
         MVC   DMINBTS,WKSVDMIN                                                 
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETVCCLS NTR1  BASE=*,LABEL=*      GET VENDOR CONTACT LIST                      
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING SRVKEY,RE                                                        
         MVC   SRVKMED,QMEDIA                                                   
         MVC   SRVKPUB,BPUB                                                     
         MVC   SRVKAGY,QAGENCY                                                  
         MVI   SRVKRCD,SRVKRCDQ                                                 
         MVC   SRVKCLT,QCLIENT                                                  
         MVC   SRVKPRD,QPRODUCT                                                 
*                                                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(SRVKPID-SRVKEY),KEYSAVE                                      
         BE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL PRODUCT                           
         XC    KEY,KEY                                                          
         MVC   SRVKMED,QMEDIA                                                   
         MVC   SRVKPUB,BPUB                                                     
         MVC   SRVKAGY,QAGENCY                                                  
         MVI   SRVKRCD,SRVKRCDQ                                                 
         MVC   SRVKCLT,QCLIENT                                                  
         MVC   SRVKPRD,=X'FFFFFF'                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(SRVKPRD-SRVKEY),KEYSAVE                                      
         BE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL CLIENT                            
         XC    KEY,KEY                                                          
         MVC   SRVKMED,QMEDIA                                                   
         MVC   SRVKPUB,BPUB                                                     
         MVC   SRVKAGY,QAGENCY                                                  
         MVI   SRVKRCD,SRVKRCDQ                                                 
         MVC   SRVKCLT,=X'FFFFFF'                                               
         GOTOR HIGHPB                                                           
         CLC   KEY(SRVKCLT-SRVKEY),KEYSAVE                                      
         JE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR ALL PUB, CLT LEVEL                    
         XC    KEY,KEY                                                          
         MVC   SRVKMED,QMEDIA                                                   
         MVC   SRVKPUB,=6X'FF'                                                  
         MVC   SRVKAGY,QAGENCY                                                  
         MVI   SRVKRCD,SRVKRCDQ                                                 
         MVC   SRVKCLT,QCLIENT                                                  
         GOTOR HIGHPB                                                           
         CLC   KEY(SRVKPRD-SRVKEY),KEYSAVE                                      
         JE    GTVCC30                                                          
*                                                                               
         LA    RE,KEY              CK FOR PUB,ALL ZONE/EDIT, CLT LEVEL          
         XC    KEY,KEY                                                          
         MVC   SRVKMED,QMEDIA                                                   
         MVC   SRVKPBCD,BPUB       BASE PUB                                     
         MVI   SRVKZONE,X'FF'      ALL ZONE                                     
         MVI   SRVKEDN,X'FF'       ALL EDITION                                  
         MVC   SRVKAGY,QAGENCY                                                  
         MVI   SRVKRCD,SRVKRCDQ                                                 
         MVC   SRVKCLT,QCLIENT                                                  
         GOTOR HIGHPB                                                           
         CLC   KEY(SRVKPRD-SRVKEY),KEYSAVE                                      
         JE    GTVCC30                                                          
*                                                                               
         JNE   GTVCC50                                                          
*                                                                               
GTVCC30  BRAS  RE,RPYCCLST         REPLY CONTACT LIST                           
*                                                                               
GTVCC50  MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
*                                                                               
         BRAS  RE,RPYRPINF         REPLY REP/PUB INFO                           
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYRPINF NTR1  BASE=*,LABEL=*      REPLY CONTRACT REP/PUB INFORMATION           
*                                                                               
         LR    R7,R8                                                            
         A     R7,=A(ESRWORKX-ESRWORKD)                                         
         LA    R6,1(R7)                                                         
         LA    R6,4095(R6)                                                      
         USING POLFILE,R7,R6                                                    
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         XC    WORK,WORK           GET PROFILE                                  
         MVC   WORK(4),=C'PAB2'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,WKTMP1,VDATAMGR                               
         CLI   WKTMP1+2,C'S'                                                    
         JE    EXIT                NO NEED TO REPLY PUB INFO                    
         DROP  RF                                                               
*                                                                               
         MVI   ADRTYP,C'C'         CONTRACT ADDRESS                             
         MVC   CLTAGY,QAGENCY                                                   
         MVC   CLTMED,QMEDIA                                                    
         MVC   CLTCOD,PCLTKCLT                                                  
         MVC   CLTOFF,PCLTOFF                                                   
*                                                                               
         MVI   CONINFSW,0          INIT CONTRACT INFO REPLY SWITCH              
         XC    SVCNALVL,SVCNALVL   INIT CONTRACT ADDRESS LEVEL                  
         MVC   WKSVAREC,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         LA    RE,PUBREC+33                                                     
         STCM  RE,15,SVPUBNEL      SAVE ADDRESS OF PUB NAME ELEM                
*                                                                               
         GOTOR APPGETAD,DMCB,(ADRTYP,CLTDATA),PUBREC,VDATAMGR                   
         CLI   0(R1),X'0A'         CONTRACT ADDRESS REC FOUND ?                 
         BNE   RPINF40                                                          
*                                                                               
         MVC   SVCNALVL,1(R1)      SAVE CONTRACT ADDRESS LEVEL                  
         OI    CONINFSW,C_CADDRQ   CONTRACT ADDRESS FOUND                       
         L     R5,4(R1)                                                         
         USING PGETADRD,R5                                                      
         XC    SVADRNAM,SVADRNAM   INIT REPLY FIELDS                            
         XC    SVADRFAX,SVADRFAX                                                
         XC    SVADREML,SVADREML                                                
         MVC   SVADRNAM,PGADNAME                                                
         MVC   SVADRFAX,PGADFAX                                                 
         MVC   SVADREML,PGADEADD                                                
*                                                                               
RPINF40  ICM   R5,15,SVPUBNEL      POINT TO PUB NAME ELEM                       
         USING PUBREPEL,R5                                                      
         MVI   ELCODE,X'14'        CLIENT/OFFICE REPS ELEM CODE                 
RPINF42  BRAS  RE,NXTEL                                                         
         BNE   RPINF60                                                          
         CLC   PUBRPOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    RPINF46                                                          
         CLC   PUBRPOFF,PCLTKCLT   CLIENT SPECIFIC?                             
         BE    RPINF44                                                          
         CLI   PUBRPOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   RPINF42                                                          
         CLC   PUBRPOFF+1(L'PCLTOFF),PCLTOFF                                    
         BNE   RPINF42                                                          
*                                                                               
RPINF44  OC    PUBPAREP(12),PUBPAREP                                            
         BZ    RPINF42                                                          
*                                                                               
RPINF46  CLC   PUBCNREP,=4C'0'     CONTRACT REP CODE PRESENT?                   
         BE    RPINF60                                                          
         OC    PUBCNREP,PUBCNREP                                                
         BZ    RPINF60                                                          
         OC    SVCNALVL,SVCNALVL   ANYTHING IN SAVED ADDRESS LEVEL?             
         BZ    *+14                                                             
         CLC   SVCNALVL,PUBRPOFF   ADDR IS MORE SPECIFIC THAN REP?              
         BL    RPINF60                                                          
         MVC   SVCNREPC,PUBCNREP                                                
*                                                                               
         XC    KEY,KEY             READ REP RECORD                              
         LA    R5,KEY                                                           
         USING PREPKEY,R5                                                       
         MVC   PREPKAGY,QAGENCY                                                 
         MVC   PREPKMED,QMEDIA                                                  
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,SVCNREPC                                                
         GOTOR HIGH                                                             
         CLC   KEY(L'PREPKEY),KEYSAVE                                           
         BNE   RPINF60                                                          
         GOTOR GETPRT                                                           
         L     R5,AWKAIO1                                                       
         LA    R5,(PREPELEM-PREPREC)(R5)                                        
         CLI   0(R5),X'11'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID REP RECORD                           
*                                                                               
         USING PREPELEM,R5                                                      
         XC    SVADRNAM,SVADRNAM   INIT REPLY FIELDS                            
         XC    SVADRFAX,SVADRFAX                                                
         XC    SVADREML,SVADREML                                                
         MVC   SVADRNAM,PREPNAME                                                
         MVC   SVADRFAX,PREPFAX                                                 
         OI    CONINFSW,C_CREPCQ   PUB CONTRACT REP INFO FOUND                  
*                                                                               
RPINF60  CLI   CONINFSW,0          ANY CONTRACT INFO FOUND?                     
         BNE   RPINF80                                                          
         XC    SVADRNAM,SVADRNAM   INIT REPLY FIELDS                            
         XC    SVADRFAX,SVADRFAX                                                
         XC    SVADREML,SVADREML                                                
         ICM   R5,15,SVPUBNEL      POINT TO PUB NAME ELEM                       
         CLI   0(R5),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID PUB RECORD                           
         USING PUBNAMEL,R5                                                      
         MVC   SVADRNAM,PUBNAME                                                 
*                                                                               
         MVI   ELCODE,X'11'        PUBLICATION SUPPL ADDRESS ELEM CODE          
         USING PUBSADEL,R5                                                      
RPINF64  BRAS  RE,NXTEL                                                         
         BNE   RPINF68                                                          
         CLC   PUBSAOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    RPINF66                                                          
         CLC   PUBSAOFF,PCLTKCLT   CLIENT SPECIFIC?                             
         BE    RPINF66                                                          
         CLI   PUBSAOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   RPINF64                                                          
         CLC   PUBSAOFF+1(L'PCLTOFF),PCLTOFF                                    
         BNE   RPINF64                                                          
*                                                                               
RPINF66  MVC   SVADRFAX,PUBSFAXN                                                
*                                                                               
RPINF68  ICM   R5,15,SVPUBNEL      POINT TO PUB NAME ELEM                       
         MVI   ELCODE,X'70'        PUBLICATION WEB ADDRESS ELEM CODE            
         BRAS  RE,NXTEL                                                         
         BNE   RPINF80                                                          
         USING PUBWEBEL,R5                                                      
         IC    R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVADREML(0),PUBWEBS                                              
*                                                                               
RPINF80  MVC   WKRPNAM(L'SVADRNAM),SVADRNAM                                     
         OC    WKRPNAM(L'SVADRNAM),SPACES                                       
         CLC   WKRPNAM(L'SVADRNAM),SPACES                                       
         BNE   *+10                                                             
         MVC   WKRPNAM(13),=C'** NO NAME **'                                    
         MVC   SVADRNAM,WKRPNAM                                                 
         MVI   WKRPNAML,L'SVADRNAM                                              
         MVI   WKRPTYP,C'F'                                                     
         MVC   WKRPADR(L'SVADRFAX),SVADRFAX                                     
         OC    WKRPADR(L'SVADRFAX),SPACES                                       
         CLC   WKRPADR(L'SVADRFAX),SPACES                                       
         BE    RPINF88             NO FAX, SKIP ENTIRE CONTACT REPLY            
         MVC   SVADRFAX,WKRPADR                                                 
         MVI   WKRPADRL,L'SVADRFAX                                              
         BRAS  RE,CKCTFFAX                                                      
         BE    RPINF86                                                          
         MVC   WKRPADR(08),=C'ERROR - '                                         
         MVC   WKRPADR+08(L'CTFXCODE),SVADRFAX+3                                
         MVI   WKRPADRL,08+L'CTFXCODE                                           
RPINF86  BRAS  RE,RPINF_LK                                                      
*                                                                               
RPINF88  MVC   WKRPADR(L'SVADREML),SVADREML                                     
         OC    WKRPADR(L'SVADREML),SPACES                                       
         CLC   WKRPADR(L'PGADEADD),SPACES                                       
         BE    RPINF90                                                          
         MVC   WKRPNAM(L'SVADRNAM),SVADRNAM                                     
         MVI   WKRPNAML,L'SVADRNAM                                              
         MVI   WKRPTYP,C'E'                                                     
         MVI   WKRPADRL,L'SVADREML                                              
         BRAS  RE,RPINF_LK                                                      
*                                                                               
RPINF90  MVC   AREC,WKSVAREC       RESTORE PREVIOUS AREC POINTER                
*                                                                               
         J     EXIT                                                             
*                                                                               
RPINF_LK ST    RE,WKFULL2                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#VNDCON)              
         SR    RF,RF                                                            
         IC    RF,WKRPNAML                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTNAME),    +        
               ('LD_CHARQ',WKRPNAM),((RF),0)                                    
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_1),    +        
               ('LD_CHARQ',WKRPTYP),(L'WKRPTYP,0)                               
         SR    RF,RF                                                            
         IC    RF,WKRPADRL                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTADDR),    +        
               ('LD_CHARQ',WKRPADR),((RF),0)                                    
         L     RE,WKFULL2                                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R7,R6,R5,R3                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCTFFAX NTR1  BASE=*,LABEL=*      CKING FOR FAX CODE ON CONTROL FILE           
*                                                                               
         CLC   WKRPADR(3),=C'FX='                                               
         JNE   SETCCEQ                                                          
*                                                                               
         MVC   CKCTFWK,KEY         SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYALPHA                                                 
         MVC   CTFXCODE,WKRPADR+3                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,CKCTFIO               
*                                                                               
         MVC   KEY,CKCTFWK         RESTORE KEY                                  
*                                                                               
         CLC   CKCTFIO(18),KEYSAVE                                              
         JNE   SETCCNEQ            FAX CODE IS NOT ON FILE                      
         LA    R4,CKCTFIO                                                       
         LA    R6,CTFXEL1                                                       
         ST    R6,WKFULL2                                                       
         B     CKCTF40                                                          
*                                                                               
CKCTF20  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
CKCTF40  CLI   0(R6),0                                                          
         JE    SETCCNEQ            FAX CODE IS NOT ON FILE                      
         CLI   0(R6),CTFX1ELQ                                                   
         BNE   CKCTF20                                                          
         XC    WKRPADR,WKRPADR                                                  
         SR    RE,RE                                                            
         IC    RE,CTFX1LEN                                                      
         SHI   RE,2+1                                                           
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKRPADR(0),CTFX1NUM                                              
         AHI   RE,1                                                             
         STC   RE,WKRPADRL                                                      
*                                                                               
         L     R6,WKFULL2          NOW LOOK FOR ATTENTION NAME                  
         USING CTFXATT,R6                                                       
         B     *+12                                                             
CKCTF60  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    CKCTF90             ATTENTION NAME NOT FOUND                     
         CLI   0(R6),CTFX2ELQ                                                   
         BNE   CKCTF60                                                          
         XC    WKRPNAM,WKRPNAM                                                  
         SR    RE,RE                                                            
         IC    RE,CTFX2LEN                                                      
         SHI   RE,2+1                                                           
         CHI   RE,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKRPNAM(0),CTFX2ATT                                              
         AHI   RE,1                                                             
         STC   RE,WKRPNAML                                                      
*                                                                               
CKCTF90  J     SETCCEQ             GOT FAX NUMBER FROM FAX CODE                 
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSETUP NTR1  BASE=*,LABEL=*      GET SETUP RECORD                             
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4                                                        
         MVC   H_TIMACC,=X'010000' DEFAULT RESPONSE PERIOD TO 1 DAY             
*                                                                               
GTSUP80  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         MVC   WKBYTE1,H_TIMACC+0                                               
         EDIT  (B1,WKBYTE1),(2,WKTMP1),0,ALIGN=LEFT,FILL=0                      
         MVC   WKTMP1+2(4),=C'0000'                                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RSPPER),    +        
               ('LD_CHARQ',WKTMP1),(6,0)                                        
*                                                                               
GTSUP_X  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RDSETUPR NTR1  BASE=*,LABEL=*      RETRIEVE & SAVE SETUP REC INFO               
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAREC,AREC                                                    
         XC    SVEACMOS,SVEACMOS                                                
         MVI   SVESRESW,0                                                       
         XC    KEY,KEY             CK SETUP RECORD AT CLIENT LEVEL              
         LA    R2,KEY                                                           
         USING SCHKKEY,R2                                                       
         MVC   SCHKAGY,QAGENCY                                                  
         MVC   SCHKMED,QMEDIA                                                   
         MVI   SCHKRCD,SCHKRCDQ                                                 
         MVC   SCHKCLT,QCLIENT                                                  
         GOTOR HIGH                                                             
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BE    RDSETR30                                                         
         XC    KEY,KEY             CK SETUP RECORD FOR ALL CLIENT               
         MVC   SCHKAGY,QAGENCY                                                  
         MVC   SCHKMED,QMEDIA                                                   
         MVI   SCHKRCD,SCHKRCDQ                                                 
         GOTOR HIGH                                                             
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BNE   RDSETR_X                                                         
*                                                                               
RDSETR30 MVC   AREC,AWKAIO1                                                     
         GOTOR GETPRT                                                           
         L     R5,AWKAIO1                                                       
         LA    R5,(SCHFIRST-SCHREC)(R5)                                         
         USING SCHHDRD,R5                                                       
         CLI   SCHHDRCD,SCHHDRQ                                                 
         BNE   RDSETR_X                                                         
         MVC   SVEACMOS,SCHESRD    ESR BY EST ACTIVATION DATE                   
         MVC   SVESRESW,SCHESRE    C'Y' - ALLOW ESR BY EST                      
*                                                                               
RDSETR_X MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY AND AIO                 
         MVC   AREC,WKSVAREC                                                    
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R5                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NOTE: VENDOR AND AGENCY CONTACT ELEMS ARE IDENTICAL                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCCLST NTR1  BASE=*,LABEL=*      REPLY CONTACT LIST                           
*                                                                               
         MVC   WKSVAREC,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         CLI   WKSVMODE,ACCLISTQ   DOING AGENCY CONTACT LIST?                   
         BE    RPYCLS12                                                         
         CLI   WKSVMODE,VCCLISTQ   DOING VENDOR CONTACT LIST?                   
         BE    RPYCLS10                                                         
         DC    H'0'                INVALID CALLING MODE                         
RPYCLS10 GOTOR GETPUB                                                           
         B     RPYCLS14                                                         
RPYCLS12 GOTOR GETPRT                                                           
RPYCLS14 MVC   AREC,WKSVAREC       RESTORE PREVIOUS AREC POINTER                
*                                                                               
         L     R5,AWKAIO1          POINT TO CONTACT RECORD                      
         LA    R5,SRVFIRST-SRVREC(R5)                                           
         CLI   0(R5),SRVEBIDQ                                                   
         BE    *+6                                                              
         DC    H'0'                BASE ELEM IS NOT THERE                       
         USING SRVEBELM,R5                                                      
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         SR    RE,RE                                                            
         IC    RE,SRVEBLEN                                                      
         SHI   RE,SRVEBLQ          MINUS OVERHEAD                               
         STC   RE,WKBYTE1          SAVE LENGTH OF E-MAIL BASE                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP1(0),SRVEBASE  SAVE E-MAIL BASE                             
*                                                                               
         MVI   ELCODE,SRVLIDQ      VENDOR CONTACT LIST ELEM CODE                
RPYCLS30 BRAS  RE,NXTEL                                                         
         BNE   RPYCLSX                                                          
         USING SRVLELM,R5                                                       
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         LHI   RF,E#AGYCON                                                      
         CLI   WKSVMODE,VCCLISTQ   VENDOR CONTACT LIST?                         
         BNE   *+8                                                              
         LHI   RF,E#VNDCON                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTNAME),    +        
               ('LD_CHARQ',SRVLNAME),(L'SRVLNAME,0)                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_1),    +        
               ('LD_CHARQ',SRVLTYPE),(L'SRVLTYPE,0)                             
*                                                                               
         CLI   SRVLTYP2,0                                                       
         BE    RPYCLS34                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TYPE_2),    +        
               ('LD_CHARQ',SRVLTYP2),(L'SRVLTYP2,0)                             
*                                                                               
RPYCLS34 XC    WKTMP2,WKTMP2                                                    
*                                                                               
         CLI   SRVLTYPE,C'F'       FAX?                                         
         BE    RPYCLS40                                                         
         SR    RF,RF                                                            
         IC    RF,SRVLLEN                                                       
         SHI   RF,SRVLSTLQ         MINUS OVERHEAD                               
         LR    R0,RF                                                            
         LA    RE,SRVLADDR                                                      
RPYCLS36 CLI   0(RE),C'@'                                                       
         BE    RPYCLS38            FOUND @, NO NEED TO USE E-MAIL BASE          
         LA    RE,1(RE)            POINT TO NEXT CHARACTER                      
         BCT   RF,RPYCLS36                                                      
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),SRVLADDR                                               
         LA    RE,WKTMP2                                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'@'                                                       
         AHI   R0,1                                                             
         SR    RF,RF                                                            
         IC    RF,WKBYTE1          LENGTH OF E-MAIL BASE                        
         AR    R0,RF               ADD IT TO CREATE LENGTH                      
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WKTMP1      GET SAVED E-MAIL BASE                        
         LR    RF,R0               TOTAL LENGTH                                 
         B     RPYCLS60                                                         
*                                                                               
RPYCLS38 LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),SRVLADDR                                               
         LR    RF,R0               TOTAL LENGTH                                 
         B     RPYCLS60                                                         
*                                                                               
RPYCLS40 SR    RE,RE                                                            
         IC    RE,SRVLLEN                                                       
         SHI   RE,SRVLSTLQ         MINUS OVERHEAD                               
         LR    RF,RE               LENGTH OF FAX NUMBER                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKTMP2(0),SRVLADDR                                               
*                                                                               
RPYCLS60 GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTADDR),    +        
               ('LD_CHARQ',WKTMP2),((RF),0)                                     
*                                                                               
         CLI   SRVLSUP,C'Y'        SUPPRESS COST?                               
         BNE   RPYCLS70                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SUPCOS),    +        
               ('LD_CHARQ',SRVLSUP),(L'SRVLSUP,0)                               
*                                                                               
RPYCLS70 DS    0H                                                               
*                                                                               
         B     RPYCLS30            NEXT VENDOR CONTACT LIST ELEM                
*                                                                               
RPYCLSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETESR#R NTR1  BASE=*,LABEL=*      GET ESR # AND REVISION                       
*                                                                               
         BRAS  RE,RDSETUPR         RETRIEVE & SAVE SETUP REC INFO               
*                                                                               
         BRAS  RE,VFNDESR#         CK FOR ESR #                                 
         BE    GETW#R40                                                         
         BRAS  RE,VNXTESR#         RESERVE AN ESR #                             
         B     GETW#R50                                                         
*                                                                               
GETW#R40 OC    QSRKEY,QSRKEY       PASSIVE KEY FOUND?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    QSRKEY+25,X'80'     DELETED?                                     
         BNZ   GETW#R50            NO NEED TO BUMP REV# IF DELETED              
         SR    RE,RE               WEB IO # FOUND, BUMP UP REV#                 
         IC    RE,SVESR_R#                                                      
         AHI   RE,1                                                             
         STC   RE,SVESR_R#                                                      
*                                                                               
GETW#R50 OC    SVESR#,SVESR#                                                    
         BNZ   *+6                                                              
         DC    H'0'                WEB IO # IS NOT SET!                         
*                                                                               
GETW#RX  J     EXIT                SET RETURN CODE                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WRIESR#R NTR1  BASE=*,LABEL=*      GET WEB IO NUMBER AND REVISION               
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         JNE   EXIT                                                             
*                                                                               
         MVC   WKSVKEY,KEY                                                      
*                                                                               
         GOTOR VMININIT            INIT MINIO BLOCK                             
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         LA    RE,MINMKEY          ESTABLISH WEB IO MASTER KEY                  
         USING ESRKEY,RE                                                        
         MVC   ESRKAGY,QAGENCY     SET AGENCY                                   
         MVC   ESRKMED,QMEDIA      SET MEDIA                                    
         MVI   ESRKRCD,ESRKRCDQ    SET RECORD CODE                              
         MVC   ESRKCLT,QCLIENT     SET CLIENT                                   
         MVC   ESRKPUB,BPUB        SET PUB CODE                                 
         MVC   ESRKSRYR,SVESR_YR   SET PERIOD YEAR                              
         MVC   ESRKSRSQ,SVESR_SQ   SET SEQUENCE NUMBER                          
         MVC   ESRKRV#,SVESR_R#    SET REVISION NUMBER                          
         MVI   ESRKELMK,X'FF'      SET FOR MASTER KEY                           
         MVC   ESRKELMK+1(L'ESRKELMK-1),ESRKELMK                                
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)                               
*                                                                               
         BRAS  RE,BLDX10EL         ADD HEADER ELEM                              
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO                                
         BE    WRIW#30                                                          
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)                               
*                                                                               
WRIW#30  BRAS  RE,BLDX20EL         ADD WEB IO STATUS ELEM                       
         BRAS  RE,BLDX28EL         ADD WEB IO INSERTION ELEM                    
         BRAS  RE,BLDX30EL         ADD ACTIVITY ELEM                            
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,VPSSVS           BUILD ALL POINTERS                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,R7                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX10EL NTR1  BASE=*,LABEL=*      ADD HEADER ELEM                              
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING ESRHDRD,R2                                                       
*                                                                               
         MVI   ESRHKCDE,ESRHKIDQ   HEADER ELEM CODE                             
         MVI   ESRHKLEN,ESRHDRLQ   HEADER ELEM LENGTH                           
         MVC   ESRHDATE,BTODAY                                                  
         MVI   ESRHSTAT,ESRSGENQ   GENERATED                                    
         MVC   ESRHSTRT,QPERSTR    PERIOD START                                 
         MVC   ESRHEND,QPEREND     PERIOD END                                   
         MVC   ESRHPRD,QPRODUCT    PRODUCT                                      
         MVC   ESRHEST,BEST        BINARY ESTIMATE                              
         MVC   ESRHBUY#,NUMPRCIN   NUMBER OF INSERTIONS PROCESSED               
*                                                                               
         CLI   SRESVTYP,SRTSTEWQ   STEWARD ORDER?                               
         JE    BLDX10_8                                                         
         CLI   SRESVTYP,SRTSTWEQ   STEWARD ORDER?                               
         JE    BLDX10_8                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JNE   *+8                                                              
BLDX10_8 MVI   ESRHSTEW,C'S'       STEWARD SPACE RESERVATION                    
*                                                                               
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         BE    *+12                                                             
         CLI   SRESVTYP,SRTSTWEQ   ESR BY ESTIMATE?                             
         BNE   *+8                                                              
         OI    ESRHOTYP,ESRTESTQ   ESR BY ESTIMATE                              
*                                                                               
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         BE    *+12                                                             
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         BNE   *+8                                                              
         OI    ESRHOTYP,ESRTEPRQ   ESR BY ESTIMATE PERIOD                       
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4                                                        
         MVC   ESRHTACC,H_TIMACC   ACCESS TIME OUT                              
         MVC   ESRHTANS,H_TIMACT   TIME TO REMIND                               
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD)                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R7                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX20EL NTR1  BASE=*,LABEL=*      ADD WEB IO STATUS ELEM                       
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING ESRSTATD,R2                                                      
*                                                                               
         MVI   ESRSKCDE,ESRSKIDQ   HEADER ELEM CODE                             
         MVI   ESRSKLEN,ESRSDRLQ   HEADER ELEM LENGTH                           
         MVI   ESRSKSQN,1          STARTS AT ONE                                
*                                                                               
         GOTOR DATCON,DMCB,(5,0),(25,ESRSDATE)                                  
         SR    RE,RE                                                            
         IC    RE,ESRSTIME                                                      
         AHI   RE,6                ADJUSTMENT FOR DDS TIME                      
         STC   RE,ESRSTIME                                                      
*                                                                               
         MVI   ESRSSTAT,ESRSGENQ   GENERATED                                    
         MVC   ESRSPID,SVSR2PID                                                 
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX28EL NTR1  BASE=*,LABEL=*      ADD WEB IO INSERTION ELEM                    
*                                                                               
         SR    R5,R5               COUNTER                                      
         L     R4,ASER#TAB         POINT TO SERIAL# TABLE                       
         USING SER#TABD,R4                                                      
         SR    R3,R3                                                            
         ICM   R3,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
*                                                                               
BX28_20  CLI   S#STATUS,S#NOTU_Q   SERIAL# NOT USED IN INS ORDER?               
         BNE   BX28_40                                                          
BX28_30  LA    R4,SER#TBLQ(R4)     POINT TO NEXT ENTRY IN TABLE                 
         BCT   R3,BX28_20                                                       
         B     BX28_X                                                           
*                                                                               
BX28_40  OC    S#PRCCNT,S#PRCCNT                                                
         BZ    BX28_30             USED, BUT NEVER PROCESSED                    
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING ESRBUYD,R2                                                       
*                                                                               
         MVI   ESRBKCDE,ESRBKIDQ   HEADER ELEM CODE                             
         MVI   ESRBKLEN,ESRBBUYL   HEADER ELEM LENGTH                           
         MVC   ESRBKSQN,S#PRCCNT                                                
         ZAP   ESRBSER#,S#SERIAL                                                
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BX28_30             CK FOR MORE TO BE ADDED                      
*                                                                               
BX28_X   J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDX30EL NTR1  BASE=*,LABEL=*      ADD ACTIVITY ELEM                            
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R2,WKTMP1                                                        
         USING ESRACTHD,R2                                                      
*                                                                               
         MVI   ESRAKCDE,ESRAKACQ   HEADER ELEM CODE                             
         MVI   ESRAKLEN,ESRACTLQ   HEADER ELEM LENGTH                           
         LHI   RE,1                                                             
         STCM  RE,3,ESRAKSQN       STARTS AT ONE                                
*                                                                               
         MVC   ESRAPID,SVSR2PID                                                 
         MVC   ESRADTE,BTODAY                                                   
         OI    ESRACH1,ESRAADD                                                  
*                                                                               
         GOTOR VADDELM,DMCB,WKTMP1                                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FIND SR# FOR A GIVEN DATE USUALLY THAT OF A RESERVATION                       
*                                                                               
* PERIOD DATES WILL BE UPDATED TO THAT OF SPACE RESERVATION                     
*                                                                               
* EXIT    CC NOT EQUAL - IO NOT FOUND                                           
*                                                                               
*         CC EQUAL     - IO FOUND (FOLLOWING DATA WILL BE RETURNED)             
*                                                                               
*         QSRKEY   = PASSIVE KEY                                                
*         SVESR#   = FOUND NEW SERIAL NUMBER                                    
*         SVESR_R# = CORRECT REVISION NUMBER                                    
*         ESRDISKA = DISK ADDRESS OF MASTER MINIO RECORD                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VFNDESR# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAREC,AREC                                                    
         XC    KEY,KEY             INIT KEY                                     
         XC    QSRKEY,QSRKEY       INIT IOKEY SAVEAREA                          
         MVI   PREVSRTY,0          INIT PREVIOUS RESERVATION TYPE               
         XC    PREVSRES,PREVSRES   INIT PREVIOUS RESERVATION EST                
         GOTOR VMININIT            INIT MINIO BLOCK                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS PERIOD PASSIVE              
         USING ESR1KEYD,R4                                                      
         MVC   ESR1AGY,QAGENCY     SET AGENCY                                   
         MVC   ESR1MED,QMEDIA      SET MEDIA                                    
         MVI   ESR1RCD,ESR1RCDQ    SET RECORD TYPE                              
         MVC   ESR1CLT,QCLIENT     SET CLIENT                                   
         MVC   ESR1PRD,QPRODUCT    SET PRODUCT                                  
         MVC   ESR1PUB,BPUB        SET PUB                                      
         MVC   ESR1END,QPERCEND    SET END DATE                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
         MVC   AREC,AWKAIO2        READ FIRST RECORD INTO IOA2                  
*                                                                               
FNDSR#LP DS    0H                                                               
         CLC   ESR1KEY(ESR1END-ESR1KEYD),KEYSAVE                                
         BNE   FNDSR#DN                                                         
*                                                                               
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    FNDSR#14                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    FNDSR#14                                                         
         SR    RE,RE                                                            
         ICM   RE,3,ESR1STRT                                                    
         SR    RF,RF                                                            
         ICM   RF,3,ESR1END                                                     
         SR    RF,RE               GET NUMBER OF DAYS IN DATE RANGE             
         CHI   RF,30                                                            
         JH    FNDSR#C6            SKIP - PASSIVE KEY IS BY PERIOD              
*                                                                               
* NOTE: PROBLEMATIC REVISION STATUS BIT IS MANUALLY SET USING PFM               
*                                                                               
         TM    ESR1CNTL,ESRDXRXQ   PROBLEMATIC REVISION?                        
         JNZ   FNDSR#C6                                                         
*                                                                               
FNDSR#14 CLC   QPERCEND,ESR1END    DATE MUST BE IN IO PERIOD                    
         BH    FNDSR#DN                                                         
         CLC   QPERCEND,ESR1STRT                                                
         BL    FNDSR#DN                                                         
*                                                                               
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JNE   FNDSR#16                                                         
         CLC   QPERCSTR,ESR1STRT   PERIOD START MATCH?                          
         JNE   FNDSR#C6                                                         
         CLC   QPERCEND,ESR1END    PERIOD END MATCH?                            
         JNE   FNDSR#C6                                                         
*                                                                               
FNDSR#16 OC    SVESR#,SVESR#       SKIP IF NO SR# GIVEN                         
         BZ    *+14                                                             
         CLC   ESR1SR#,SVESR#      FILTER ON SR#                                
         BNE   FNDSR#CN                                                         
*                                                                               
         MVC   WKTMPKEY,ESR1KEY    SAVE KEY                                     
         GOTOR GETPRT              READ IN FIRST RECORD IN MINIO SET            
         L     RF,AREC             POINT TO FOUND RECORD                        
         MVC   MINMKEY,0(RF)       GET MASTER KEY FOR MINIO SET                 
         MVI   MINDELSW,C'Y'       PROCESS DELETES                              
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
         TM    MINSTAT,MINDELQ     DELETED MINIO SET?                           
         BZ    FNDSR#L2                                                         
         CLI   SRESVTYP,SRTSTEWQ   STEWARD RESERVATION?                         
         BE    FNDSR#L1                                                         
*                                                                               
         CLI   SRESVTYP,SRT1ESTQ   SINGLE EST RESERVATION?                      
         JE    FNDSR#20                                                         
         CLI   SRESVTYP,SRTSTWEQ   SINGLE EST STEWARD RESERVATION?              
         JE    FNDSR#20                                                         
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    FNDSR#20                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    FNDSR#20                                                         
*                                                                               
         J     FNDSR#L1                                                         
*                                                                               
FNDSR#20 TM    PREVSRTY,ESRTESTQ   ESR BY ESTIMATE PREVIOUSLY?                  
         JNZ   FNDSR#24                                                         
         TM    PREVSRTY,ESRTEPRQ   ESR BY ESTIMATE PERIOD PREVIOUSLY?           
         JNZ   FNDSR#24                                                         
         J     FNDSR#CN                                                         
FNDSR#24 BRAS  RE,GET_BEST         GET BINARY EST                               
         CLC   PREVSRES,HALF       MATCH ON EST?                                
         BNE   FNDSR#CN                                                         
         CLI   WKTMPKEY+(ESR1RV#-ESR1KEY),0                                     
         BE    FNDSR#CN                                                         
         B     FNDSR#L6                                                         
*                                                                               
FNDSR#L1 TM    PREVSRTY,ESRTESTQ   RESERVATION BY EST PREVIOUSLY?               
         JNZ   FNDSR#CN                                                         
         TM    PREVSRTY,ESRTEPRQ   ESR BY ESTIMATE PERIOD PREVIOUSLY?           
         JNZ   FNDSR#CN                                                         
         OC    PREVSRES,PREVSRES   RESERVATION BY EST PREVIOUSLY?               
         JNZ   FNDSR#CN                                                         
         CLI   WKTMPKEY+(ESR1RV#-ESR1KEY),0                                     
         BE    FNDSR#CN                                                         
         B     FNDSR#L6                                                         
*                                                                               
FNDSR#L2 XC    WKTMP1,WKTMP1       ESTABLISH HEADER ELEMENT                     
         LA    R6,WKTMP1                                                        
         USING ESRHDRD,R6                                                       
         MVI   ESRHKCDE,ESRHKIDQ   SET HEADER ELEMENT ID                        
         GOTOR VGETELM,DMCB,WKTMP1 READ HEADER ELEMENT                          
         BNE   FNDSR#CN            SKIP - ELEMENT NOT FOUND                     
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
         MVC   PREVSRTY,ESRHOTYP                                                
         MVC   PREVSRES,ESRHEST                                                 
*                                                                               
         CLI   SRESVTYP,SRTSTEWQ   STEWARD RESERVATION?                         
         BE    FNDSR#L8                                                         
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    FNDSR#42                                                         
         CLI   SRESVTYP,SRTSTWEQ   STEWARDSHIP ESR BY ESTIMATE?                 
         JE    FNDSR#42                                                         
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    FNDSR#42                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    FNDSR#42                                                         
*                                                                               
         J     FNDSR#L8                                                         
*                                                                               
FNDSR#42 TM    ESRHOTYP,ESRTESTQ   ESR BY ESTIMATE PREVIOUSLY?                  
         JNZ   FNDSR#44                                                         
         TM    ESRHOTYP,ESRTEPRQ   ESR BY ESTIMATE PERIOD PREVIOUSLY?           
         JNZ   FNDSR#44                                                         
         J     FNDSR#CN                                                         
*                                                                               
FNDSR#44 CLI   PRQESTH+5,0         HAVE ESTIMATE?                               
         BNH   FNDSR#CN                                                         
         TM    PRQESTH+4,X'08'     VALID NUMERIC?                               
         BZ    FNDSR#CN                                                         
         BRAS  RE,GET_BEST         GET BINARY EST                               
         CLC   ESRHEST,HALF        MATCH ON ESTIMATE?                           
         BNE   FNDSR#CN                                                         
*                                                                               
         TM    ESRHOTYP,ESRTESTQ   ESR BY ESTIMATE PREVIOUSLY?                  
         JZ    FNDSR#46                                                         
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    FNDSR#50                                                         
         CLI   SRESVTYP,SRTSTWEQ   STEWARDSHIP ESR BY ESTIMATE?                 
         JE    FNDSR#50                                                         
         J     FNDSR#CN            KEEP LOOKING FOR NEXT MATCH                  
*                                                                               
FNDSR#46 TM    ESRHOTYP,ESRTEPRQ   ESR BY ESTIMATE PERIOD PREVIOUSLY?           
         JZ    FNDSR#50                                                         
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    FNDSR#50                                                         
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    FNDSR#50                                                         
         J     FNDSR#CN            KEEP LOOKING FOR NEXT MATCH                  
*                                                                               
FNDSR#50 B     FNDSR#L6                                                         
*                                                                               
FNDSR#L4 CLC   SRESVTYP,ESRHSTEW   MATCH ON STEWARDSHIP?                        
         BNE   FNDSR#CN                                                         
FNDSR#L6 MVC   QSRKEY,WKTMPKEY     SAVE KEY IF A MATCH                          
         B     FNDSR#CN                                                         
*                                                                               
FNDSR#L8 TM    ESRHOTYP,ESRTESTQ   ESR BY ESTIMATE?                             
         JNZ   FNDSR#CN                                                         
         TM    ESRHOTYP,ESRTEPRQ   ESR BY ESTIMATE PERIOD?                      
         JNZ   FNDSR#CN                                                         
         OC    ESRHEST,ESRHEST     HAVE ESTIMATE?                               
         BNZ   FNDSR#CN                                                         
         B     FNDSR#L4                                                         
*                                                                               
FNDSR#CN DS    0H                                                               
         MVC   KEY,WKTMPKEY        RESTORE CURRENT KEY                          
         GOTOR HIGH                RESTORE DIRECTORY POINTER                    
FNDSR#C6 OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR SEQ                 GET NEXT POINTER                             
         B     FNDSR#LP                                                         
*                                                                               
FNDSR#DN DS    0H                                                               
         MVC   AREC,WKSVAREC       RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,WKSVKEY         RESTORE PREVIOUS KEY                         
         MVI   MINDELSW,0          TURN OFF PROCESS DELETES                     
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         OC    QSRKEY,QSRKEY       SKIP IF KEY FOUND                            
         BNZ   *+8                                                              
         J     SETCCNEQ                                                         
*                                                                               
         LA    R4,QSRKEY           POINT TO FOUND RECORD                        
         MVC   ESRDISKA,ESR1DISK   SAVE MASTER RECORD DISK ADDR                 
         MVC   SVESR#,ESR1SR#      SET SR#                                      
         MVC   SVESR_R#,ESR1RV#    SET REVISION #                               
*                                                                               
         J     SETCCEQ             SET RETURN CODE                              
*                                                                               
GET_BEST PACK  DUB,PRQEST                                                       
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FIND AND RESERVE NEXT AVAILABLE INSERTION ORDER SERIAL #                      
*                                                                               
* IF THERE ARE NO INSORDS  ON FILE, START NUMBERING AT ONE                      
*                                                                               
* ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS SERIAL #S IN                  
* COMPLEMENT. READS LOWEST NUMBER (REALLY HIGHEST) FOR UPDATE                   
* AND THEN ADDS POINTER FOR NEXT NUMBER. THIS RESERVES NEXT                     
* NUMBER FOR THIS CALL TO SUBROUTINE.                                           
*                                                                               
* IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS REPEATED                    
*                                                                               
* SCHEMA RECORD DESCRIBES RANGE WHERE NUMBER IS UNIQUE                          
*                                                                               
* EXIT    SVESR#  =  WILL RETURN NEW SERIAL # (SVESR_YR & SVESR_SQ)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VNXTESR# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKSVKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS INV IO# PASSIVE             
         USING ESR#SR#D,R4                                                      
*                                                                               
NXTSERLP DS    0H                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   ESR#AGY,QAGENCY     SET AGENCY                                   
         MVC   ESR#MED,QMEDIA      SET MEDIA                                    
         MVI   ESR#RCD,ESR#RCDQ    SET RECORD TYPE                              
*                                                                               
* CK SCHEMA HERE TO SEE IF CLIENT/PUB INCLUDED IN KEY                           
* FOR NOW ASSUME CLIENT IS, BY PASS PUB FOR NOW                                 
*                                                                               
         MVC   ESR#CLT,QCLIENT     SET CLIENT                                   
         MVI   ESR#PUB,X'FF'                                                    
         MVC   ESR#PUB+1(L'ESR#PUB-1),ESR#PUB                                   
*                                                                               
         MVC   ESR#SRYR,QPER       SET YEAR OF INSORD PERIOD                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR  HIGH               READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   ESR#KEY(ESR#SRSQ-ESR#SR#D),KEYSAVE                               
         BE    NXTSER1                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   ESR#SRSQ,=X'000001' START SEQUENCE AT 1                          
         XC    ESR#SRSQ,=X'FFFFFF' 2'S COMPLEMENT                               
         B     NXTSER2                                                          
*                                                                               
NXTSER1  DS    0H                  READ RECORD AND RESERVE NEXT IO#             
         OI    DMINBTS,X'88'       READ FOR UPDATE AND DELETED                  
*                                                                               
         GOTOR READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'88' RESET                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,ESR#SRSQ       GET CURRENT SEQ NUMBER                       
         SHI   RF,1                DECREMENT BY ONE                             
         STCM  RF,7,ESR#SRSQ       RESET SEQUENCE NUMBER                        
*                                                                               
NXTSER2  DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NXTSERDN                                                         
         GOTOR ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSERDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSERCN DS    0H                                                               
         B     NXTSERLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSERDN DS    0H                  RETURN DATA                                  
         MVC   SVESR_YR,ESR#SRYR                                                
         MVC   SVESR_SQ,ESR#SRSQ                                                
         XC    SVESR_SQ,=X'FFFFFF'                                              
*                                                                               
         MVC   KEY,WKSVKEY         RESTORE CURRENT KEY                          
         GOTOR HIGH                RESTORE PRTDIR SEQUENCE                      
*                                                                               
VNXTSERX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CREATE PASSIVE POINTERS                                                       
*                                                                               
* NTRY    R7 ==> MINIO SET GETTING PASSIVES                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPSSVS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         MVC   WKSVKEY,KEY                                                      
*                                                                               
         IC    R0,DMINBTS          SAVE SETTING                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    VPSS10                                                           
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
* FIND DISK ADDRESS OF MASTER RECORD                                            
*                                                                               
VPSS10   XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS MASTER KEY                      
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKEY,MINMKEY      COPY MINIO MASTER KEY                        
         MVI   ESRKELMK,X'FF'      SET TO MAX ELEMENT KEY                       
         MVC   ESRKELMK+1(L'ESRKELMK-1),ESRKELMK                                
*                                                                               
         GOTOR HIGH                READ MASTER KEY                              
*                                                                               
         CLC   ESRKEY,KEYSAVE      CHECK IF KEY FOUND                           
         BE    *+6                                                              
         DC    H'0'                MUST FIND KEY                                
*                                                                               
         MVC   ESRDISKA,ESRDDISK   SAVE DISK ADDR OF MASTER REC                 
*                                                                               
* READ HEADER ELEMENT - USED FOR MAJOR KEY FIELDS                               
*                                                                               
         XC    WKTMP1,WKTMP1                                                    
         LA    R6,WKTMP1           ESTABLISH WORK ELM AS HEADER                 
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKCDE,ESRHKIDQ   SET FOR HEADER ELEMENT                       
*                                                                               
         GOTOR VGETELM,DMCB,WKTMP1 READ HEADER ELEMENT                          
         BZ    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
* ADD FIRST PASSIVE KEY                                                         
*                                                                               
                                                                                
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS 1ST PASSIVE                     
         USING ESR1KEY,R4                                                       
*                                                                               
         MVC   ESR1AGY,ESRKAGY-ESRKEY+MINMKEY                                   
         MVC   ESR1MED,ESRKMED-ESRKEY+MINMKEY                                   
         MVI   ESR1RCD,ESR1RCDQ                                                 
         MVC   ESR1CLT,ESRKCLT-ESRKEY+MINMKEY                                   
         MVC   ESR1PRD,ESRHPRD                                                  
         MVC   ESR1PUB,ESRKPUB-ESRKEY+MINMKEY                                   
         GOTOR DATCON,DMCB,(3,ESRHEND),(2,ESR1END)                              
         GOTOR DATCON,DMCB,(3,ESRHSTRT),(2,ESR1STRT)                            
         MVC   ESR1SR#,ESRKSR#-ESRKEY+MINMKEY     SET IO#                       
         MVC   ESR1RV#,ESRKRV#-ESRKEY+MINMKEY     SET IO#                       
*                                                                               
         GOTOR HIGH                READ PASSIVE                                 
                                                                                
         CLC   ESR1KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   PSVPS1NF                                                         
                                                                                
         TM    ESR1CNTL,ESRDDELQ   RESTORE IF DELETED                           
         BO    *+14                                                             
         CLC   ESR1DISK,ESRDISKA   DONE IF DISK ADDR SAME                       
         BE    PSVPS1X                                                          
                                                                                
         MVC   ESR1DISK,ESRDISKA   ELSE SET NEW DISK ADDR                       
         NI    ESR1CNTL,X'FF'-ESRDDELQ                                          
                                                                                
         GOTOR WRITE               RE-WRITE THE PASSIVE                         
                                                                                
         B     PSVPS1X                                                          
                                                                                
PSVPS1NF DS    0H                  PASSIVE NOT ON FILE                          
                                                                                
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVC   ESR1DISK,ESRDISKA   SET DISK ADDRESS OF MASTER REC               
                                                                                
         GOTOR ADD                 ADD PASSIVE TO FILE                          
                                                                                
PSVPS1X  DS    0H                                                               
         GOTOR VMINIO,ESRPARMS,('MINCLS',MINBLKD)                               
*                                                                               
         STC   R0,DMINBTS          RESTORE SETTING                              
*                                                                               
VPSSVSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VMININIT NTR1  BASE=*,LABEL=*      INITIALIZE MINIO BLOCK                       
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
         MVC   MINCOMF,VCOMFACS                                                 
         MVC   MINRECUP,VRECUP                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'PRTFIL '  FILE NAME                                    
         MVC   MINDIR,=C'PRTDIR '  DIR NAME                                     
         MVI   MINFKLEN,25         LENGTH OF KEY                                
*                                                                               
* FULL RECORD RETAINS 75% OF IT'S ELEMENTS ON SPLITING                          
*                                                                               
         LA    R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT                                                     
*                                                                               
         MVI   MINNCTL,L'ESRDCNTL  2 CONTROL BYTES                              
         LHI   R1,2976                                                          
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH                            
*                                                                               
         MVI   MINEKLEN,L'ESRKELMK                                              
         MVI   MINEKDSP,ESRKELMK-ESRKEY                                         
*                                                                               
         MVC   MINBUFF,AWKAIO3     A(FIRST MINIO BUFFER)                        
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
VMININIX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO ADD AN ELEMENT TO A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(ELEMENT TO BE ADDED)                                          
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT ADDED                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VADDELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE ADDED                 
*                                                                               
         OC    0(2,R6),0(R6)                                                    
         BNZ   *+6                                                              
         DC    H'0'                CONTRUCTED ELEM IS BAD                       
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINADD',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
VADDELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO ADD AN ELEMENT TO A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(ELEMENT TO BE DELETED)                                        
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT DELETED                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VDELELM  NTR1  BASE=*,LABEL=*                                                   
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
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SET REST OF ELEMENT KEY                      
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINDEL',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
DELELSX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET AN ELEMENT IN A RECORD                                         
*                                                                               
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                                   
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGETELM  NTR1  BASE=*,LABEL=*                                                   
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
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)                                                         
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
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VGETELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO REPLACE ELEMENT IN A MINIO SET                                     
* DELETES AND ADDS NEW ELEMENT (IT'S OKAY IF NO PRIOR ELEM IS FOUND)            
*                                                                               
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE REPLACED)                                
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT REPLACED                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VWRTELM  NTR1  BASE=*,LABEL=*                                                   
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
         GOTOR VMINIO,ESRPARMS,('MINWRT',MINBLKD)                               
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VWRTELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET NEXT ELEMENT IN A RECORD                                       
*                                                                               
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                              
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VNXTELM  NTR1  BASE=*,LABEL=*                                                   
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
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VNXTELM1 DS    0H                                                               
*                                                                               
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VNXTELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO GET PREVIOUS ELEMENT IN THE RECORD                                 
*                                                                               
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                              
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE              
*                  FOR ONLY THAT AMOUNT OF KEY                                  
*                                                                               
*        R7==>  MINIO BLOCK                                                     
*                                                                               
* EXIT   CC    NEQ - ERROR OCCURRED                                             
*              EQ  - ELEMENT FOUND                                              
*                                                                               
*        MINELEM     A(ELEMENT FOUND)                                           
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VPRVELM  NTR1  BASE=*,LABEL=*                                                   
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
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R6)                                                         
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VPRVELM1 DS    0H                                                               
*                                                                               
         SR    RF,RF               GET KEY LENGTH                               
         IC    RF,MINEKLEN                                                      
         SHI   RF,2                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTOR VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VPRVELMX J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSR205D DSECT                                                                  
*                                                                               
RELO05   DS    F                                                                
*                                                                               
AWKAIO1  DS    A                                                                
AWKAIO2  DS    A                                                                
AWKAIO3  DS    A                                                                
AWKAIO4  DS    A                                                                
*                                                                               
VMINIO   DS    A                                                                
*                                                                               
         DS    0D                  ALIGNMENT                                    
ESRPARMS DS    6F                  PARAMETER LIST                               
*                                                                               
QSRKEY   DS    CL32                CURRENT MINIO MASTER KEY                     
QPER     DS    0XL6                PERIOD                                       
QPERSTR  DS    XL3                 PERIOD START DATE (BINARY)                   
QPEREND  DS    XL3                 PERIOD END DATE (BINARY)                     
QPERCSTR DS    XL2                 PERIOD START DATE (COMPRESSED)               
QPERCEND DS    XL2                 PERIOD END DATE (COMPRESSED)                 
*                                                                               
MNBLKCB  DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
         DS    0D                  ALIGNMENT                                    
MNRTAB   DS    CL256               MINIO RECORD TABLE                           
MNELEM   DS    CL256               MINIO ELEMENT AREA                           
*                                                                               
WKSVMODE DS    X                   SAVE CALLING MODE                            
WKSVKEY  DS    XL(L'KEY)                                                        
WKTMPKEY DS    XL(L'KEY)                                                        
WKSVAREC DS    XL(L'AREC)                                                       
*                                                                               
WKTMP1   DS    XL256                                                            
WKTMP2   DS    XL256                                                            
WKFULL1  DS    F                                                                
WKFULL2  DS    F                                                                
WKBYTE1  DS    X                                                                
WKBYTE2  DS    X                                                                
WKSVDMIN DS    X                   SAVE DATA MANAGER IN BITS                    
*                                                                               
ADRTYP   DS    CL1                 TYPE OF ADDR REC - PAY,TRAFFIC,ETC.          
CLTDATA  DS    0CL7                KEY INFO FOR PPGETADR MODULE                 
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCOD   DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
CONINFSW DS    X                   CONTRACT INFORMATION REPLY SWITCH            
C_CADDRQ EQU   X'80'               CONTRACT ADDRESS                             
C_CREPCQ EQU   X'40'               CONTRACT REP CODE                            
*                                                                               
PREVSRTY DS    XL(L'ESRHOTYP)      PREVIOUS RESERVATION TYPE                    
PREVSRES DS    XL(L'ESRHEST)       PREVIOUS RESERVATION EST                     
*                                                                               
SVPUBNEL DS    XL4                 SAVE ADDRESS OF 1ST PUB ELEM                 
SVADRNAM DS    CL(L'PGADNAME)      SAVE FIELDS FROM PPGETADR                    
SVADRFAX DS    CL(L'PGADFAX)                                                    
SVADREML DS    CL(L'PGADEADD)                                                   
SVCNALVL DS    XL(L'PUBRPOFF)      SAVE CONTRACT ADDRESS LEVEL                  
SVCNREPC DS    XL(L'PUBCNREP)      SAVE CONTRACT REP CODE                       
*                                                                               
WKRPNAM  DS    CL(L'PGADNAME)      REP/PUB NAME                                 
WKRPNAML DS    X                                                                
WKRPTYP  DS    X                   TYPE OF CONTACT                              
WKRPADR  DS    CL(L'PGADEADD)      FAX NUMBER OR E-MAIL ADDRESS                 
WKRPADRL DS    X                                                                
*                                                                               
CKCTFWK  DS    CL32                LOCAL WORKING STORAGE AREA                   
CKCTFIO  DS    600C                CONTROL FILE IO AREA                         
*                                                                               
WKAIO1   DS    XL4096                                                           
WKAIO2   DS    XL4096                                                           
WKAIO3   DS    XL4096                                                           
WKAIO4   DS    XL4096                                                           
*                                                                               
PPSR205X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PPSR205   09/02/14'                                      
         END                                                                    
