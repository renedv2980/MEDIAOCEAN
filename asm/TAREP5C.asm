*          DATA SET TAREP5C    AT LEVEL 002 AS OF 01/28/16                      
*PHASE T7035CA                                                                  
         TITLE 'T7035C - VITA AGENCY/CLIENT/PRODUCT MQ UPLOAD'                  
T7035C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7035C,R6,R5                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING RECD,R7             R7=A(LOCAL W/S)                              
                                                                                
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         CLI   MODE,VALKEY         IF CALLED WITH VALKEY                        
         BNE   *+8                                                              
         BAS   RE,VK               VALIDATE OPTIONS                             
                                                                                
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   XIT                                                              
         BAS   RE,UL               UPLOAD DATA                                  
                                                                                
***********************************************************************         
*        CONDITION CODE AND EXIT ROUTINE                              *         
***********************************************************************         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1                                                                   
         MVI   OPTIONS,0                                                        
         MVI   MQPRENUM,C'*'                                                    
                                                                                
         USING SSOOFF,R3                                                        
         CLI   OFFLINE,C'Y'        SET DSPACE OFFLINE                           
         BNE   VK00                                                             
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   R3,15,4(R1)         =A(SSB)                                      
         CLC   0(2,R3),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   DSPACE,SSODSPAC-SSOOFF(R3)                                       
         B     VK05                                                             
         DROP  R3                                                               
                                                                                
         USING FACTSD,R3                                                        
VK00     GOTO1 GETFACT,DMCB,0      SET DSPACE ONLINE                            
         L     R3,DMCB                                                          
         MVI   DSPACE,C'T'                                                      
         CLI   FASYSID,1                                                        
         BE    VK05                                                             
         MVI   DSPACE,C'A'                                                      
         CLI   FASYSID,8                                                        
         BE    VK05                                                             
         MVI   DSPACE,C'C'                                                      
         CLI   FASYSID,11                                                       
         BE    VK05                                                             
         MVI   DSPACE,C'Q'                                                      
         CLI   FASYSID,15                                                       
         BE    VK05                                                             
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
VK05     CLI   SPLOPTH+5,0                                                      
         BE    XIT                                                              
                                                                                
         LA    R2,SPLOPTH                                                       
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
VK10     CLI   SCLEN2,0                                                         
         BNE   FLDINV                                                           
                                                                                
         CLI   SCLEN1,1                                                         
         BNE   VK20                                                             
         TM    SCVAL1,X'80'                                                     
         BZ    FLDINV                                                           
         MVC   MQPRENUM,SCDATA1                                                 
                                                                                
         CLI   DSPACE,C'A'                                                      
         BNE   VK10C                                                            
         CLC   SCBIN1,=F'2'                                                     
         BL    FLDINV                                                           
         CLC   SCBIN1,=F'5'                                                     
         BH    FLDINV                                                           
         B     VK40                                                             
                                                                                
VK10C    CLI   DSPACE,C'C'                                                      
         BNE   VK10T                                                            
         CLC   SCBIN1,=F'1'                                                     
         BNE   FLDINV                                                           
         B     VK40                                                             
                                                                                
VK10T    CLI   DSPACE,C'T'                                                      
         BNE   VK10Q                                                            
         CLC   SCBIN1,=F'2'                                                     
         BL    FLDINV                                                           
         CLC   SCBIN1,=F'6'                                                     
         BH    FLDINV                                                           
         B     VK40                                                             
                                                                                
VK10Q    CLI   DSPACE,C'Q'                                                      
         BNE   VK10X                                                            
         CLC   SCBIN1,=F'2'                                                     
         BL    FLDINV                                                           
         CLC   SCBIN1,=F'7'                                                     
         BH    FLDINV                                                           
         B     VK40                                                             
                                                                                
VK10X    B     FLDINV                                                           
                                                                                
VK20     CLI   SCLEN1,4                                                         
         BNE   FLDINV                                                           
         CLC   =C'FULL',SCDATA1                                                 
         BNE   VK30                                                             
         OI    OPTIONS,OPTFULL                                                  
         B     VK40                                                             
                                                                                
VK30     CLC   =C'FILE',SCDATA1                                                 
         BNE   FLDINV                                                           
         OI    OPTIONS,OPTFILE                                                  
                                                                                
VK40     LA    R3,SCANNEXT                                                      
         BCT   R0,VK10                                                          
         B     XIT                                                              
         DROP  R3                                                               
                                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SUMMARIZE NOTICES SENT TO VITA                    *         
***********************************************************************         
                                                                                
UL       NTR1                                                                   
         GOTO1 INITIAL,DMCB,0      INITIALIZE PROGRAM                           
                                                                                
         BAS   RE,OPENFILE         IF USING FILE OPTION, OPEN IT NOW            
                                                                                
         LA    R3,KEY                                                           
         BAS   RE,UACP             UPLOAD AGENY/CLIENT/PRODUCT INFO             
         BAS   RE,UUNI             UPLOAD UNION INFO                            
         BAS   RE,UNMD             UPLOAD NEW MEDIA INFO                        
         BAS   RE,UINT             UPLOAD INTERNET INFO                         
         BAS   RE,UGUA             UPLOAD GUARANTEE INFO                        
         BAS   RE,UAGT             UPLOAD AGENT INFO                            
         BAS   RE,UGUAN            UPLOAD GUARANTEE INFO                        
                                                                                
         BAS   RE,CLOSFILE         IF USING FILE OPTION, CLOSE FILE             
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD AGENCY/CLIENT/PRODUCT INFO TO VITA         *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UACP     NTR1                                                                   
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    UACP00                                                           
         TM    OPTIONS,OPTFULL     ONLY SEND IF OPTION FOR FULL                 
         BZ    XIT                 BLAST WAS INCLUDED                           
                                                                                
UACP00   XC    AKEY(RECDLNQ),AKEY  INITIALIZE WORKING STORAGE                   
                                                                                
         USING TLAYD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ ALL AGENCY RECORDS                      
         MVI   TLAYCD,TLAYCDQ                                                   
         GOTO1 HIGH                                                             
         B     UACP20                                                           
UACP10   GOTO1 SEQ                                                              
UACP20   CLC   TLAYAGY,=C'999999'  WHEN WE HIT 999999, DONE                     
         BE    XIT                                                              
                                                                                
         GOTO1 GETREC              GET AGENCY RECORD                            
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   UACP10                                                           
                                                                                
         CLI   MQPRENUM,C'*'       IF UPLOADING TO PRIMARY QUEUE                
         BNE   *+12                                                             
         TM    TAAYSTA3,TAAYSLCK   SKIP LOCKED AGENCIES                         
         BO    UACP10                                                           
                                                                                
         CLI   TAAYLEN,TAAYLNQ                                                  
         JL    UACP25                                                           
         TM    TAAYSTA7,TAAYSPPL   SKIP PAYROLL PLUS AGENCIES                   
         JO    UACP10                                                           
                                                                                
UACP25   GOTO1 OPENMQ,DMCB,ACPOPEN,L'ACPOPEN                                    
                                                                                
         MVC   ALINEAGY,TLAYAGY    COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'ALINEAGY,ALINEAGY)                              
         DROP  R3                                                               
                                                                                
         MVC   ALINENAM,SPACES     INITIALIZE AGENCY ADDRESS FIELDS             
         MVC   ALINEAD1,SPACES                                                  
         MVC   ALINEAD2,SPACES                                                  
         MVC   ALINEAD3,SPACES                                                  
         MVC   ALINEAD4,SPACES                                                  
                                                                                
         MVC   ALINEPHO,TAAYTEL    COPY AGENCY PHONE NUMBER                     
         OC    ALINEPHO,SPACES                                                  
         MVC   ALINEOFF,TAAYTPOF   AND OFFICE CODE INTO XML                     
         OC    ALINEOFF,SPACES                                                  
                                                                                
         MVI   ALINELCK,C'N'                                                    
         TM    TAAYSTA3,TAAYSLCK   COPY LOCKED STATUS INTO XML                  
         BZ    *+8                                                              
         MVI   ALINELCK,C'Y'                                                    
                                                                                
******** MVI   ALINEFIL,C'N'                                                    
******** TM    TAAYSTA6,TAAYSFIL                                                
******** BZ    *+8                                                              
         MVI   ALINEFIL,C'Y'       COPY FILLABLE PDF STATUS INTO XML            
                                                                                
         MVI   ALINE13W,C'Y'                                                    
         TM    TAAYSTAT,TAAYS13W   COPY CALCULATE 13 WEEKS AS 3 MONTHS          
         BZ    *+8                 STATUS INTO XML                              
         MVI   ALINE13W,C'N'                                                    
                                                                                
******** MVI   ALINETPY,C'N'                                                    
******** TM    TAAYSTA6,TAAYVIBE   COPY ENABLED FOR VITA TIMESHEETS             
******** BZ    *+8                 AND PAY STATUS INTO XML                      
         MVI   ALINETPY,C'Y'                                                    
                                                                                
         MVI   ALINEORQ,C'N'       COPY PRD REQUIRED STATUS INTO XML            
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   ALINEORQ,C'Y'                                                    
                                                                                
         MVI   ALINEPRQ,C'N'                                                    
         TM    TAAYSTAT,TAAYSAPO   COPY PO REQUIRED STATUS INTO XML             
         BZ    *+8                                                              
         MVI   ALINEPRQ,C'Y'                                                    
         TM    TAAYSTA4,TAAYPOV                                                 
         BZ    *+8                                                              
         MVI   ALINEPRQ,C'Y'                                                    
                                                                                
         MVI   ALINEPVL,C'N'                                                    
         TM    TAAYSTA4,TAAYSCPO   COPY PO VALIDATED STATUS INTO XML            
         BZ    *+8                                                              
         MVI   ALINEPVL,C'Y'                                                    
         TM    TAAYSTA4,TAAYPOV                                                 
         BZ    *+8                                                              
         MVI   ALINEPVL,C'Y'                                                    
                                                                                
         MVI   ALINEJRQ,C'N'                                                    
         TM    TAAYSTAT,TAAYSEST   COPY JOB REQUIRED STATUS INTO XML            
         BZ    *+8                                                              
         MVI   ALINEJRQ,C'Y'                                                    
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   ALINEJRQ,C'Y'                                                    
         TM    TAAYSTA7,TAAYSBBD                                                
         BZ    *+8                                                              
         MVI   ALINEJRQ,C'Y'                                                    
         OC    TAAYBUNT,TAAYBUNT                                                
         BZ    *+8                                                              
         MVI   ALINEJRQ,C'Y'                                                    
                                                                                
         MVI   ALINEBAG,C'N'       COPY betaAgency STATUS INTO XML              
         TM    TAAYSTA6,TAAYVIBE                                                
         BZ    *+8                                                              
         MVI   ALINEBAG,C'Y'                                                    
                                                                                
         MVI   ALINEJVL,C'N'                                                    
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   ALINEJVL,C'Y'                                                    
         TM    TAAYSTA7,TAAYSBBD                                                
         BZ    *+8                                                              
         MVI   ALINEJVL,C'Y'                                                    
         OC    TAAYBUNT,TAAYBUNT   COPY JOB VALIDATED STATUS INTO XML           
         BZ    *+8                                                              
         MVI   ALINEJVL,C'Y'                                                    
         TM    TAAYSTAT,TAAYSEST                                                
         BZ    UACP30                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   UACP30                                                           
         MVI   ALINEJVL,C'Y'                                                    
         DROP  R4                                                               
                                                                                
UACP30   MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTDJB))                                     
         BNE   UACP35                                                           
         MVI   ALINEJRQ,C'Y'                                                    
         MVI   ALINEJVL,C'Y'                                                    
                                                                                
         USING TABRD,R4                                                         
UACP35   MVC   ALINEEMP,=C'TP '    COPY EMPLOYER INTO XML                       
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP40                                                           
         OC    TABROEOR,TABROEOR                                                
         BZ    *+10                                                             
         MVC   ALINEEMP,TABROEOR                                                
                                                                                
         TM    TABRSTAT,TABRSINT                                                
         BZ    UACP40                                                           
         MVI   ALINEORQ,C'Y'       COPY PRD REQUIRED STATUS                     
         MVI   ALINEJRQ,C'Y'       JOB REQUIRED STATUS                          
         MVI   ALINEJVL,C'Y'       AND JOB VALIDATED STATUS INTO XML            
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
UACP40   L     R4,AIO              COPY AGENCY NAME INTO XML                    
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP50                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ALINENAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'ALINENAM,ALINENAM)                              
         DROP  R4                                                               
                                                                                
         USING TAADD,R4                                                         
UACP50   L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP60                                                           
         ZIC   RE,TAADLEN                                                       
         SHI   RE,3                                                             
         LA    RF,TAADADD                                                       
         DROP  R4                                                               
                                                                                
         MVC   ALINEAD1,0(RF)      COPY 1ST ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'ALINEAD1                                                    
         LTR   RE,RE                                                            
         BZ    UACP60                                                           
         LA    RF,L'ALINEAD1(RF)                                                
         MVC   ALINEAD2,0(RF)      COPY 2ND ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'ALINEAD2                                                    
         LTR   RE,RE                                                            
         BZ    UACP60                                                           
         LA    RF,L'ALINEAD2(RF)                                                
         MVC   ALINEAD3,0(RF)      COPY 3RD ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'ALINEAD3                                                    
         LTR   RE,RE                                                            
         BZ    UACP60                                                           
         LA    RF,L'ALINEAD3(RF)                                                
         MVC   ALINEAD4,0(RF)      COPY 4TH ADDRESS LINE INTO XML               
                                                                                
UACP60   GOTO1 ELIMCHAR,DMCB,(L'ALINEAD1,ALINEAD1)                              
         GOTO1 ELIMCHAR,DMCB,(L'ALINEAD2,ALINEAD2)                              
         GOTO1 ELIMCHAR,DMCB,(L'ALINEAD3,ALINEAD3)                              
         GOTO1 ELIMCHAR,DMCB,(L'ALINEAD4,ALINEAD4)                              
                                                                                
         GOTO1 PUTMQ,DMCB,ALINE,ALINELNQ  PUT AGENCY XML TO MQ/FILE             
                                                                                
         MVC   AKEY(L'TLAYKEY),KEY SAVE AGENCY KEY                              
                                                                                
         USING TLCLD,R3                                                         
         XC    KEY,KEY             READ ALL CLIENT RECORDS FOR                  
         MVI   TLCLCD,TLCLCDQ      THIS AGENCY                                  
         MVC   TLCLAGY,AKEY+TLAYAGY-TLAYD                                       
         GOTO1 HIGH                                                             
         B     UACP80                                                           
UACP70   GOTO1 SEQ                                                              
UACP80   CLC   KEY(TLCLCLI-TLCLD),KEYSAVE                                       
         BNE   UACP160                                                          
                                                                                
         MVC   CLINECLI,TLCLCLI    COPY CLIENT CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'CLINECLI,CLINECLI)                              
         DROP  R3                                                               
                                                                                
         MVC   CLINENAM,SPACES     INITIALIZE CLIENT FIELDS                     
         MVI   CLINELCK,C'N'                                                    
                                                                                
         GOTO1 GETREC              GET CLIENT RECORD                            
                                                                                
******** USING TACID,R4                                                         
******** L     R4,AIO              TEMPORARY CODE TO BLOCK OUT                  
******** MVI   ELCODE,TACIELQ      LOCKED CLIENTS                               
******** BAS   RE,GETEL                                                         
******** BNE   *+12                                                             
******** TM    TACISTAT,TACISLCK                                                
******** BO    UACP70                                                           
******** DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY CLIENT NAME INTO XML                    
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP90                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLINENAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'CLINENAM,CLINENAM)                              
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
UACP90   L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP100                                                          
         TM    TACISTAT,TACISLCK   COPY CLIENT LOCKED STATUS INTO XML           
         BZ    UACP100                                                          
         MVI   CLINELCK,C'Y'                                                    
         DROP  R4                                                               
                                                                                
UACP100  GOTO1 PUTMQ,DMCB,CLINE,CLINELNQ   PUT CLIENT XML TO MQ/FILE            
                                                                                
         MVC   CKEY(L'TLCLKEY),KEY SAVE CLIENT KEY                              
                                                                                
         USING TLPRD,R3                                                         
         XC    KEY,KEY             READ ALL PRODUCT RECORDS FOR                 
         MVI   TLPRCD,TLPRCDQ      THIS AGENCY/CLIENT                           
         MVC   TLPRAGY,AKEY+TLAYAGY-TLAYD                                       
         MVC   TLPRCLI,CKEY+TLCLCLI-TLCLD                                       
         GOTO1 HIGH                                                             
         B     UACP120                                                          
UACP110  GOTO1 SEQ                                                              
UACP120  CLC   KEY(TLPRPRD-TLPRD),KEYSAVE                                       
         BNE   UACP150                                                          
                                                                                
         MVC   PLINEPRD,TLPRPRD    COPY PRODUCT CODE INTO XML                   
         GOTO1 ELIMCHAR,DMCB,(L'PLINEPRD,PLINEPRD)                              
         DROP  R3                                                               
                                                                                
         MVC   PLINENAM,SPACES     INITIALIZE PRODUCT FIELDS                    
         MVI   PLINELCK,C'N'                                                    
                                                                                
         GOTO1 GETREC              GET PRODUCT RECORD                           
                                                                                
******** USING TAPID,R4                                                         
******** L     R4,AIO              TEMPORARY CODE TO BLOCK OUT                  
******** MVI   ELCODE,TAPIELQ      LOCKED PRODUCTS                              
******** BAS   RE,GETEL                                                         
******** BNE   *+12                                                             
******** TM    TAPISTAT,TAPISLCK                                                
******** BO    UACP110                                                          
******** DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY PRODUCT NAME INTO XML                   
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP130                                                          
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PLINENAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'PLINENAM,PLINENAM)                              
         DROP  R4                                                               
                                                                                
         USING TAPID,R4                                                         
UACP130  L     R4,AIO                                                           
         MVI   ELCODE,TAPIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP140                                                          
         TM    TAPISTAT,TAPISLCK   COPY PRODUCT LOCKED STATUS INTO XML          
         BZ    UACP140                                                          
         MVI   PLINELCK,C'Y'                                                    
         DROP  R4                                                               
                                                                                
UACP140  GOTO1 PUTMQ,DMCB,PLINE,PLINELNQ  PUT PRODUCT XML TO MQ/FILE            
         B     UACP110                                                          
                                                                                
UACP150  GOTO1 PUTMQ,DMCB,CCLOSE,L'CCLOSE CLOSE CLIENT XML                      
         MVC   KEY,CKEY                   AND RESTORE CLIENT KEY                
         GOTO1 HIGH                                                             
         B     UACP70                                                           
                                                                                
         USING TLATD,R3                                                         
UACP160  XC    KEY,KEY             READ ALL BILL-TO RECORDS FOR                 
         MVI   TLATCD,TLATCDQ      THIS AGENCY                                  
         MVC   TLATAGY,AKEY+TLAYAGY-TLAYD                                       
         GOTO1 HIGH                                                             
         B     UACP180                                                          
UACP170  GOTO1 SEQ                                                              
UACP180  CLC   KEY(TLATATT-TLATD),KEYSAVE                                       
         BNE   UACP210                                                          
                                                                                
         MVC   BLINECOD,TLATATT    COPY BILL-TO CODE INTO XML                   
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC              GET BILL-TO RECORD                           
                                                                                
         MVC   BLINENAM,SPACES     INITIALIZE BILL-TO FIELDS                    
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY BILL-TO NAME INTO XML                   
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UACP190                                                          
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BLINENAM(0),TANANAME                                             
         B     UACP200                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
UACP190  MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTATT))                                     
         BNE   UACP200                                                          
         L     R4,TGELEM                                                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BLINENAM(0),TAFNNAME                                             
         DROP  R4                                                               
                                                                                
UACP200  GOTO1 ELIMCHAR,DMCB,(L'BLINENAM,BLINENAM)                              
                                                                                
         GOTO1 PUTMQ,DMCB,BLINE,BLINELNQ  PUT BILL-TO XML TO MQ/FILE            
         GOTO1 PUTMQ,DMCB,BCLOSE,L'BCLOSE CLOSE BILL-TO XML                     
         B     UACP170                                                          
                                                                                
UACP210  GOTO1 PUTMQ,DMCB,ACLOSE,L'ACLOSE CLOSE AGENCY XML                      
         GOTO1 CLOSEMQ,DMCB,ACPCLOSE,L'ACPCLOSE                                 
         MVC   KEY,AKEY                   AND RESTORE AGENCY KEY                
         GOTO1 HIGH                                                             
         B     UACP10                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD UNION INFO TO VITA                         *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UUNI     NTR1                                                                   
         TM    OPTIONS,OPTFULL     ONLY SEND IF OPTION FOR FULL                 
         BZ    XIT                 BLAST WAS INCLUDED                           
                                                                                
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                                                              
         XC    AKEY(RECDLNQ),AKEY  INITIALIZE WORKING STORAGE                   
                                                                                
         GOTO1 OPENMQ,DMCB,UNIOPEN,L'UNIOPEN                                    
                                                                                
         USING TLLOD,R3                                                         
         XC    KEY,KEY             READ ALL LOCAL RECORDS                       
         MVI   TLLOCD,TLLOCDQ                                                   
         MVC   TLLOUN,=C'AFM'                                                   
         GOTO1 HIGH                                                             
         B     UUNI20                                                           
UUNI10   GOTO1 SEQ                                                              
UUNI20   CLI   TLLOCD,TLLOCDQ                                                   
         BNE   UUNI40                                                           
         CLC   TLLOUN,=C'SAG'      ONLY PROCESS SAG                             
         BE    UUNI30                                                           
         CLC   TLLOUN,=C'NON'      NON                                          
         BE    UUNI30                                                           
         CLC   TLLOUN,=C'AFT'      AFTRA                                        
         BE    UUNI30                                                           
         CLC   TLLOUN,=C'AFM'      AFM LOCALS                                   
         BNE   UUNI10                                                           
                                                                                
UUNI30   MVC   ULINEUNI,TLLOUN     PUT UNION/LOCAL XML TO MQ/FILE               
         MVC   ULINELCL,TLLOLCL                                                 
         GOTO1 ELIMCHAR,DMCB,(L'ULINELCL,ULINELCL)                              
         GOTO1 PUTMQ,DMCB,ULINE,ULINELNQ                                        
         B     UUNI10                                                           
         DROP  R3                                                               
                                                                                
UUNI40   GOTO1 CLOSEMQ,DMCB,UNICLOSE,L'UNICLOSE                                 
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD NEW MEDIA INFO TO VITA                     *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UNMD     NTR1                                                                   
         TM    OPTIONS,OPTFULL     IF OPTION FOR FULL BLAST WAS                 
         BZ    XIT                 INCLUDED                                     
                                                                                
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                                                              
         XC    AKEY(RECDLNQ),AKEY  INITIALIZE WORKING STORAGE                   
                                                                                
         GOTO1 OPENMQ,DMCB,NMDOPEN,L'NMDOPEN                                    
                                                                                
         USING TLMDD,R3                                                         
         XC    KEY,KEY             READ ALL NEW MEDIA RECORDS                   
         MVI   TLMDCD,TLMDCDQ                                                   
         MVI   TLMDTYPE,NEWMEDIA                                                
         GOTO1 HIGH                                                             
         B     UNMD20                                                           
UNMD10   GOTO1 SEQ                                                              
UNMD20   CLC   KEY(TLMDCODE-TLMDD),KEYSAVE                                      
         BNE   UNMD40                                                           
                                                                                
         MVC   NLINECOD,TLMDCODE   PUT CODE INTO XML                            
         OC    NLINECOD,SPACES                                                  
         GOTO1 ELIMCHAR,DMCB,(L'NLINECOD,NLINECOD)                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         MVC   NLINENAM,SPACES     INITIALIZE NAME FIELD                        
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              PUT NAME INTO XML                            
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UNMD30                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NLINENAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'NLINENAM,NLINENAM)                              
         DROP  R4                                                               
                                                                                
UNMD30   GOTO1 PUTMQ,DMCB,NLINE,NLINELNQ  PUT NEW MEDIA XML TO MQ/FILE          
         B     UNMD10                                                           
                                                                                
UNMD40   GOTO1 CLOSEMQ,DMCB,NMDCLOSE,L'NMDCLOSE                                 
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD INTERNET INFO TO VITA                      *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UINT     NTR1                                                                   
         TM    OPTIONS,OPTFULL     IF OPTION FOR FULL BLAST WAS                 
         BZ    XIT                 INCLUDED                                     
                                                                                
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                                                              
         XC    AKEY(RECDLNQ),AKEY  INITIALIZE WORKING STORAGE                   
                                                                                
         GOTO1 OPENMQ,DMCB,INTOPEN,L'INTOPEN                                    
                                                                                
         USING TLMDD,R3                                                         
         XC    KEY,KEY             READ ALL INTERNET RECORDS                    
         MVI   TLMDCD,TLMDCDQ                                                   
         MVI   TLMDTYPE,INTERNET                                                
         GOTO1 HIGH                                                             
         B     UINT20                                                           
UINT10   GOTO1 SEQ                                                              
UINT20   CLC   KEY(TLMDCODE-TLMDD),KEYSAVE                                      
         BNE   UINT40                                                           
                                                                                
         MVC   ILINECOD,TLMDCODE   PUT CODE INTO XML                            
         OC    ILINECOD,SPACES                                                  
         GOTO1 ELIMCHAR,DMCB,(L'ILINECOD,ILINECOD)                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         MVC   ILINENAM,SPACES     INITIALIZE NAME FIELD                        
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              PUT NAME INTO XML                            
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UINT30                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ILINENAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'ILINENAM,ILINENAM)                              
         DROP  R4                                                               
                                                                                
UINT30   GOTO1 PUTMQ,DMCB,ILINE,ILINELNQ  PUT INTERNET XML TO MQ/FILE           
         B     UINT10                                                           
                                                                                
UINT40   GOTO1 CLOSEMQ,DMCB,INTCLOSE,L'INTCLOSE                                 
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD GUARANTEE INFO TO VITA                     *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UGUA     NTR1                                                                   
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                 INITIALIZE WORKING STORAGE                   
         XC    GUATAB(RECDLNQ),GUATAB                                           
                                                                                
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,-30                                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDATE)                                  
                                                                                
         XC    TGSSN,TGSSN                                                      
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ                                                   
         GOTO1 HIGH                                                             
         B     UGUA20                                                           
UGUA10   GOTO1 SEQ                                                              
UGUA20   CLI   TLGUCD,TLGUCDQ                                                   
         BNE   UGUA110                                                          
         GOTO1 GETREC                                                           
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UGUA10                                                           
                                                                                
         MVC   TGAGY,TAGUAGY                                                    
         OC    TGAGY,TGAGY                                                      
         BNZ   *+10                                                             
         MVC   TGAGY,WILDCARD                                                   
                                                                                
         MVC   TGCLI,TAGUCLI                                                    
         OC    TGCLI,TGCLI                                                      
         BNZ   *+10                                                             
         MVC   TGCLI,WILDCARD                                                   
                                                                                
         OC    TAGUSTRT,TAGUSTRT   IF LARGE OVERSCALE GUARANTEE                 
         BZ    UGUA30                                                           
         CLC   TGDATE,TAGUEND      TARGET DATE MUST BE EQUAL TO                 
         BNL   UGUA10              OR LATER THAN GUARANTEE START                
         MVI   TGBYTE,C'L'         DATE                                         
         B     UGUA50                                                           
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
UGUA30   L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ      IF PER CYCLE GUARANTEE                       
         BAS   RE,GETEL            TAGRET DATE MUST BE EQUAL TO                 
         B     *+8                 OR LATER THAN EXISTING CYCLE                 
UGUA40   BAS   RE,NEXTEL           START DATE                                   
         BNE   UGUA10                                                           
         CLC   TGDATE,TAGCEND                                                   
         BNL   UGUA40                                                           
         MVI   TGBYTE,C'P'                                                      
         DROP  R4                                                               
                                                                                
UGUA50   CLC   TLGUSSN,TGSSN       IF FIRST TIME SEEING THIS SS#                
         BE    UGUA60              OUTPUT NEW PID                               
         BAS   RE,OUTACS                                                        
         GOTO1 OPENMQ,DMCB,GUAOPEN,L'GUAOPEN                                    
         GOTO1 SSNPACK,DMCB,TLGUSSN,GLINEPID                                    
         GOTO1 PUTMQ,DMCB,GLINE,GLINELNQ                                        
         MVC   TGSSN,TLGUSSN                                                    
         MVI   GUATAB,X'FF'        AND FLUSH TABLE                              
         DROP  R3                                                               
                                                                                
UGUA60   BAS   RE,ADD2TAB          ADD PRIMARY AGY/CLI TO TABLE                 
                                                                                
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ      READ ALL SUBSIDIARY AGENCY/                  
         BAS   RE,GETEL            CLIENT ELEEMNTS                              
         B     *+8                                                              
UGUA70   BAS   RE,NEXTEL                                                        
         BNE   UGUA10                                                           
                                                                                
         MVC   TGAGY,TAVAAGY       SET SUBSIDIARY AGENCY                        
                                                                                
         CLI   TAVALEN,TAVALNQ     IF SUBSIDIARY CLEINT NOT                     
         BH    UGUA80              PROVIDED                                     
         MVC   TGCLI,WILDCARD      ONLY ADD SUBSIDIARY AGENCY                   
         BAS   RE,ADD2TAB          TO TABLE                                     
         B     UGUA70                                                           
                                                                                
UGUA80   XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         BNZ   UGUA90                                                           
         LHI   RF,TAVALNQ                                                       
UGUA90   D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R0,RF               R0=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R2,TAVACLI          R2=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
UGUA100  MVC   TGCLI,0(R2)                                                      
         BAS   RE,ADD2TAB          ADD EACH SUBSIDIARY CLEINT TO                
         LA    R2,L'TAVACLI(R2)    TABLE                                        
         BCT   R0,UGUA100          AND BUMP TO NEXT CLIENT IN ELEMENT           
         B     UGUA70                                                           
         DROP  R4                                                               
                                                                                
UGUA110  BAS   RE,OUTACS                                                        
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD NEW AGENT INFO TO VITA                     *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UAGT     NTR1                                                                   
         TM    OPTIONS,OPTFULL     IF OPTION FOR FULL BLAST WAS                 
         BZ    XIT                 INCLUDED                                     
                                                                                
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                                                              
         XC    AKEY(RECDLNQ),AKEY  INITIALIZE WORKING STORAGE                   
                                                                                
         USING TLAND,R3                                                         
         XC    KEY,KEY             READ ALL AGENT RECORDS                       
         MVI   TLANCD,TLANCDQ                                                   
         GOTO1 HIGH                                                             
         B     UAGT20                                                           
UAGT10   GOTO1 SEQ                                                              
UAGT20   CLC   KEY(TLANAGT-TLAND),KEYSAVE                                       
         BNE   XIT                                                              
         GOTO1 OPENMQ,DMCB,AGTOPEN,L'AGTOPEN                                    
                                                                                
         MVC   AGLINCOD,TLANAGT    PUT CODE INTO XML                            
         OC    AGLINCOD,SPACES                                                  
         GOTO1 ELIMCHAR,DMCB,(L'AGLINCOD,AGLINCOD)                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         MVC   AGLINNAM,SPACES     INITIALIZE NAME FIELD                        
         MVC   AGLINSNM,SPACES                                                  
         MVC   AGLINAD1,SPACES                                                  
         MVC   AGLINAD2,SPACES                                                  
         MVC   AGLINAD3,SPACES                                                  
         MVC   AGLINAD4,SPACES                                                  
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              PUT NAME INTO XML                            
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UAGT25                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   AGLINNAM(0),TANANAME                                             
         GOTO1 ELIMCHAR,DMCB,(L'AGLINNAM,AGLINNAM)                              
         DROP  R4                                                               
                                                                                
         USING TASND,R4                                                         
UAGT25   L     R4,AIO              PUT SHORT NAME INTO XML                      
         MVI   ELCODE,TASNELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UAGT26                                                           
         ZIC   RE,TASNLEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   AGLINSNM(0),TASNAME                                              
         GOTO1 ELIMCHAR,DMCB,(L'AGLINSNM,AGLINSNM)                              
         DROP  R4                                                               
                                                                                
         USING TAADD,R4                                                         
UAGT26   L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UAGT30                                                           
         ZIC   RE,TAADLEN                                                       
         SHI   RE,3                                                             
         LA    RF,TAADADD                                                       
         DROP  R4                                                               
                                                                                
         MVC   AGLINAD1,0(RF)      COPY 1ST ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'AGLINAD1                                                    
         LTR   RE,RE                                                            
         BZ    UAGT28                                                           
         LA    RF,L'AGLINAD1(RF)                                                
         MVC   AGLINAD2,0(RF)      COPY 2ND ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'AGLINAD2                                                    
         LTR   RE,RE                                                            
         BZ    UAGT28                                                           
         LA    RF,L'AGLINAD2(RF)                                                
         MVC   AGLINAD3,0(RF)      COPY 3RD ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'AGLINAD3                                                    
         LTR   RE,RE                                                            
         BZ    UAGT28                                                           
         LA    RF,L'AGLINAD3(RF)                                                
         MVC   AGLINAD4,0(RF)      COPY 4TH ADDRESS LINE INTO XML               
                                                                                
UAGT28   GOTO1 ELIMCHAR,DMCB,(L'AGLINAD1,AGLINAD1)                              
         GOTO1 ELIMCHAR,DMCB,(L'AGLINAD2,AGLINAD2)                              
         GOTO1 ELIMCHAR,DMCB,(L'AGLINAD3,AGLINAD3)                              
         GOTO1 ELIMCHAR,DMCB,(L'AGLINAD4,AGLINAD4)                              
                                                                                
UAGT30   L     R4,AIO                                                           
         USING TAAND,R4                                                         
         MVI   ELCODE,TAANELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   UAGT35                                                           
         MVI   AGOVRIDE,C'N'                                                    
         TM    TAANSTAT,TAANSOVR                                                
         BZ    *+8                                                              
         MVI   AGOVRIDE,C'Y'                                                    
         DROP  R4                                                               
                                                                                
UAGT35   GOTO1 PUTMQ,DMCB,AGLINE,AGLINLNQ                                       
         GOTO1 CLOSEMQ,DMCB,AGTCLOSE,L'AGTCLOSE                                 
         B     UAGT10                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO UPLOAD GUARANTEE INFO TO VITA                     *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
UGUAN    NTR1                                                                   
         CLI   MQPRENUM,C'*'       IF UPLOADING TO SECONDARY QUEUE              
         BE    XIT                                                              
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ ALL GUARANTEE KEYS/RECORDS              
         MVI   TLGUCD,TLGUCDQ                                                   
         GOTO1 HIGH                                                             
         B     UGUAN20                                                          
UGUAN10  GOTO1 SEQ                                                              
UGUAN20  CLI   TLGUCD,TLGUCDQ                                                   
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE DETAILS ELEMENT)              
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TAGUSTRT,TAGUSTRT   IF LARGE OVERSCALE GUARANTEE                 
         BZ    UGUAN30                                                          
         CLC   =X'B30801',TAGUEND  AUG01/13 MUST BE EQUAL TO                    
         BH    UGUAN10             OR LATER THAN GUARANTEE END                  
         B     UGUAN50                                                          
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
UGUAN30  L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ      IF PER CYCLE GUARANTEE                       
         BAS   RE,GETEL            AUG01/13 MUST BE EQUAL TO                    
         B     *+8                 OR LATER THAN EXISTING CYCLE                 
UGUAN40  BAS   RE,NEXTEL           START DATE                                   
         BNE   UGUAN10                                                          
         CLC   =X'B30801',TAGCEND                                               
         BH    UGUAN40                                                          
         DROP  R4                                                               
                                                                                
UGUAN50  GOTO1 OPENMQ,DMCB,GU2AOPEN,L'GU2AOPEN                                  
                                                                                
         USING TLGUD,R4                                                         
         L     R4,AIO              PUT PID INTO XML                             
         MVC   TGSSN,TLGUSSN                                                    
         GOTO1 SSNPACK,DMCB,TLGUSSN,G2PID                                       
         GOTO1 PUTMQ,DMCB,G2PIDLIN,G2PIDLNQ                                     
                                                                                
         MVC   G2CODE,TLGUGUA      PUT GUARANTEE CODE INTO XML                  
         XC    G2CODE,=4X'FF'                                                   
         GOTO1 PUTMQ,DMCB,G2CODLIN,G2CODLNQ                                     
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ      R4=A(GUARANTEE DETAILS ELEMENT)              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   G2TYPE,=CL8'large'                                               
         OC    TAGUSTRT,TAGUSTRT   OUTPUT TYPE                                  
         BNZ   *+10                                                             
         MVC   G2TYPE,=CL8'percycle'                                            
         GOTO1 PUTMQ,DMCB,G2TYPLIN,G2TYPLNQ                                     
                                                                                
         BAS   RE,OUTCRP           OUTPUT CORP FID/PID                          
         GOTO1 PUTMQ,DMCB,G2CIDLIN,G2CIDLNQ                                     
                                                                                
         OC    TAGUSTRT,TAGUSTRT   IF LARGE OVERSCALE, OUTPUT                   
         BZ    UGUAN60             PERIOD AND AMOUNT                            
         GOTO1 DATCON,DMCB,(1,TAGUSTRT),(20,G2PSTART)                           
         GOTO1 DATCON,DMCB,(1,TAGUEND),(20,G2PEND)                              
         EDIT  TAGUAMT,G2PAMNT,2,MINUS=Y,FLOAT=-,ALIGN=RIGHT                    
         GOTO1 PUTMQ,DMCB,G2PERLIN,G2PERLNQ                                     
         B     UGUAN90                                                          
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
UGUAN60  L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ      IF PER CYCLE GUARANTEE, OUTPUT               
         BAS   RE,GETEL            PERIOD AND AMOUNT FOR EACH                   
         B     *+8                 CYCLE                                        
UGUAN70  BAS   RE,NEXTEL                                                        
         BNE   UGUAN80                                                          
         GOTO1 DATCON,DMCB,(1,TAGCSTRT),(20,G2PSTART)                           
         GOTO1 DATCON,DMCB,(1,TAGCEND),(20,G2PEND)                              
         EDIT  TAGCAMT,G2PAMNT,2,MINUS=Y,FLOAT=-,ALIGN=RIGHT                    
         GOTO1 PUTMQ,DMCB,G2PERLIN,G2PERLNQ                                     
         J     UGUAN70                                                          
         DROP  R4                                                               
                                                                                
         USING TAGUD,R4                                                         
UGUAN80  L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
UGUAN90  GOTO1 PUTMQ,DMCB,G2AGYOP1,L'G2AGYOP1                                   
                                                                                
         MVC   G2AGENCY,TAGUAGY                                                 
         OC    G2AGENCY,G2AGENCY                                                
         BNZ   *+10                                                             
         MVC   G2AGENCY,WILDCARD                                                
                                                                                
         MVC   G2CLIENT,TAGUCLI                                                 
         OC    G2CLIENT,G2CLIENT                                                
         BNZ   *+10                                                             
         MVC   G2CLIENT,WILDCARD                                                
         DROP  R4                                                               
                                                                                
         GOTO1 PUTMQ,DMCB,G2AGYOP2,G2ACLLNQ                                     
                                                                                
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ      READ ALL SUBSIDIARY AGENCY/                  
         BAS   RE,GETEL            CLIENT ELEEMNTS                              
         B     *+8                                                              
UGUAN100 BAS   RE,NEXTEL                                                        
         BNE   UGUAN140                                                         
         MVC   G2AGENCY,TAVAAGY                                                 
                                                                                
         CLI   TAVALEN,TAVALNQ     IF SUBSIDIARY CLIENT NOT DEFINED             
         BH    UGUAN110            PROVIDED                                     
         MVC   G2CLIENT,WILDCARD   SET AS WILDCARD IN XML                       
         GOTO1 PUTMQ,DMCB,G2AGYOP2,G2ACLLNQ                                     
         B     UGUAN100                                                         
                                                                                
UGUAN110 XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         BNZ   UGUAN120                                                         
         LHI   RF,TAVALNQ                                                       
UGUAN120 D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R0,RF               R0=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R2,TAVACLI          R2=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
UGUAN130 MVC   G2CLIENT,0(R2)                                                   
         GOTO1 PUTMQ,DMCB,G2AGYOP2,G2ACLLNQ                                     
         LA    R2,L'TAVACLI(R2)                                                 
         BCT   R0,UGUAN130         AND BUMP TO NEXT CLIENT IN ELEMENT           
         B     UGUAN100                                                         
         DROP  R4                                                               
                                                                                
UGUAN140 GOTO1 PUTMQ,DMCB,G2AGCLO1,L'G2AGCLO1                                   
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   G2ISLOCK,C'N'                                                    
         TM    TAGUSTAT,TAGUSLCK                                                
         BZ    *+8                                                              
         MVI   G2ISLOCK,C'Y'                                                    
         GOTO1 PUTMQ,DMCB,G2LCKLIN,G2LCKLNQ                                     
         DROP  R4                                                               
                                                                                
         GOTO1 CLOSEMQ,DMCB,GUA2CLOSE,L'GUA2CLOSE                               
         B     UGUAN10             READ NEXT GUARANTEE RECORD                   
                                                                                
***********************************************************************         
*        ROUTINE TO OUTPUT CORPORATION PID                            *         
*        ENTRY: TGSSN = GUARANTEE'S SSN                               *         
*               R4    = A(GUARANTEE DETAILS ELEMENT)                  *         
***********************************************************************         
                                                                                
OUTCRP   NTR1                                                                   
         MVC   G2CORPID,SPACES                                                  
                                                                                
         USING TAGUD,R2                                                         
         LR    R2,R4                                                            
         CLI   TAGUCRP,0                                                        
         BE    XIT                                                              
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',0)                                    
         BNE   OCX                                                              
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
OC10     BAS   RE,NEXTEL                                                        
         BNE   OCX                                                              
         CLC   TATICRPN,TAGUCRP                                                 
         BNE   OC10                                                             
         GOTO1 SSNPACK,DMCB,TATIID,G2CORPID                                     
         DROP  R2,R4                                                            
                                                                                
OCX      MVC   AIO,AIO1                                                         
                                                                                
         L     RE,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLGUKEY),0(RE)                                             
         GOTO1 HIGH                                                             
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS AGENCY/CLIENT TO TABLE                          *         
***********************************************************************         
                                                                                
ADD2TAB  NTR1                                                                   
         XR    R0,R0                                                            
                                                                                
         LA    R2,GUATAB                                                        
                                                                                
A2T10    CLI   0(R2),X'FF'                                                      
         BNE   A2T20                                                            
         LTR   R0,R0                                                            
         BNZ   XIT                                                              
         MVC   0(L'TGAGY,R2),TGAGY                                              
         MVC   6(L'TGCLI,R2),TGCLI                                              
         MVI   12(R2),X'FF'                                                     
         B     XIT                                                              
                                                                                
A2T20    CLC   TGAGY,0(R2)                                                      
         BNE   A2T30                                                            
                                                                                
         CLC   TGCLI,6(R2)                                                      
         BE    XIT                                                              
         CLC   WILDCARD,6(R2)                                                   
         BE    XIT                                                              
                                                                                
         CLC   TGCLI,WILDCARD                                                   
         BNE   A2T30                                                            
         MVC   0(12,R2),SPACES                                                  
         LTR   R0,R0                                                            
         BNZ   A2T30                                                            
         MVC   0(L'TGAGY,R2),TGAGY                                              
         MVC   6(L'TGCLI,R2),WILDCARD                                           
         AHI   R0,1                                                             
                                                                                
A2T30    LA    R2,12(R2)                                                        
         B     A2T10                                                            
                                                                                
***********************************************************************         
*        ROUTINE OUTPUTS AGENCY/CLIENTS                               *         
***********************************************************************         
                                                                                
OUTACS   NTR1                                                                   
         OC    TGSSN,TGSSN                                                      
         BZ    XIT                                                              
                                                                                
         XC    TGAGY,TGAGY                                                      
                                                                                
         LA    R2,GUATAB                                                        
OACS10   CLI   0(R2),X'FF'                                                      
         BE    OACS30                                                           
         CLC   0(12,R2),SPACES                                                  
         BE    OACS20                                                           
                                                                                
         CLC   TGAGY,0(R2)                                                      
         JE    OACS16                                                           
         OC    TGAGY,TGAGY                                                      
         JZ    OACS15                                                           
         GOTO1 PUTMQ,DMCB,GALCLOSE,L'GALCLOSE                                   
                                                                                
OACS15   MVC   GALINEAY,0(R2)                                                   
         GOTO1 PUTMQ,DMCB,GALINE,GALINLNQ                                       
         MVC   TGAGY,0(R2)                                                      
                                                                                
OACS16   MVC   GCLINECL,6(R2)                                                   
         GOTO1 PUTMQ,DMCB,GCLINE,GCLINLNQ                                       
OACS20   LA    R2,12(R2)                                                        
         B     OACS10                                                           
                                                                                
OACS30   GOTO1 PUTMQ,DMCB,GALCLOSE,L'GALCLOSE                                   
         GOTO1 PUTMQ,DMCB,GPICLOSE,L'GPICLOSE                                   
         GOTO1 CLOSEMQ,DMCB,GUACLOSE,L'GUACLOSE                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ELIMINATE XML-UNFRIENDLY CHARACTERS                          *         
***********************************************************************         
                                                                                
ELIMCHAR NTR1                                                                   
         L     R2,0(R1)            R2=A(FIELD)                                  
         ZIC   R3,0(R1)            R3=L'FIELD                                   
                                                                                
ECHAR10  CLI   0(R2),C'"'          REPLACE DOUBLE QUOTES                        
         BNE   ECHAR20             WITH SINGLE QUOTES                           
         MVI   0(R2),X'7D'                                                      
                                                                                
ECHAR20  LA    R2,1(R2)                                                         
         BCT   R3,ECHAR10                                                       
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO OPEN FILE                                         *         
***********************************************************************         
                                                                                
OPENFILE NTR1                                                                   
         TM    OPTIONS,OPTFILE     IF USING FILE                                
         BZ    XIT                                                              
         LA    R2,WEBFILE          OPEN FILE FOR OUTPUT                         
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO OPEN MQ                                           *         
***********************************************************************         
                                                                                
OPENMQ   NTR1                                                                   
         ZIC   R3,7(R1)                                                         
         L     R2,0(R1)                                                         
                                                                                
         TM    OPTIONS,OPTFILE     IF USING FILE, SKIP AHEAD                    
         BO    OMQ10                                                            
                                                                                
         MVC   WORK(16),=C'TALVITA**********'   SET PREAMBLE                    
         CLI   DSPACE,C'T'                                                      
         BNE   *+10                                                             
         MVC   WORK(3),=C'TST'                                                  
         CLI   DSPACE,C'Q'                                                      
         BNE   *+10                                                             
         MVC   WORK(3),=C'FQA'                                                  
         CLI   DSPACE,C'C'                                                      
         BNE   *+10                                                             
         MVC   WORK(3),=C'CSC'                                                  
                                                                                
OMQ00    MVC   WORK+7(1),MQPRENUM                                               
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,WORK),(X'C0',0),0                    
         MVC   TCAUTREA,TCOPFAIL                                                
         CLI   DMCB+8,0                                                         
         BNE   ERREND                                                           
                                                                                
OMQ10    GOTO1 PUTMQ,DMCB,MSGOPEN,L'MSGOPEN                                     
         GOTO1 PUTMQ,DMCB,(R2),(R3)                                             
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PUT TO MQ/FILE                                    *         
***********************************************************************         
                                                                                
PUTMQ    NTR1                                                                   
         L     R2,0(R1)            R2=A(MESSAGE TO OUTPUT)                      
         L     R3,4(R1)            R3=L'OUTPUT                                  
                                                                                
         TM    OPTIONS,OPTFILE     IF USING FILE                                
         BZ    PMQ10                                                            
         LA    R0,BLOCK                                                         
         LHI   R1,480                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,BLOCK                                                         
         LR    R1,R3                                                            
         MVCL  R0,R2                                                            
                                                                                
         LA    R2,BLOCK                                                         
         LA    R3,WEBFILE                                                       
         PUT   (R3),(R2)                                                        
         B     XIT                                                              
*                                  IF USING MQ, PUT TO MQ                       
PMQ10    GOTO1 VMQRPT,DMCB,(0,=C'PUT'),(R2),(R3),0                              
         MVC   TCAUTREA,TCPTFAIL                                                
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         B     ERREND                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO CLOSE FILE                                                  
***********************************************************************         
                                                                                
CLOSFILE NTR1                                                                   
         TM    OPTIONS,OPTFILE     IF USING FILE                                
         BZ    XIT                 CLOSE THE FILE                               
         LA    R2,WEBFILE                                                       
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO CLOSE MQ                                          *         
***********************************************************************         
                                                                                
CLOSEMQ  NTR1                                                                   
         ZIC   RE,7(R1)                                                         
         L     RF,0(R1)                                                         
         GOTO1 PUTMQ,DMCB,(RF),(RE)                                             
                                                                                
         TM    OPTIONS,OPTFILE     IF USING FILE, EXIT                          
         BO    XIT                                                              
*                                  IF USING MQ, CLOSE MQ                        
CMQ10    GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         MVC   TCAUTREA,TCCLFAIL                                                
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         B     ERREND                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO SEND OUT FAILURE EMAIL AND DUMP                             
***********************************************************************         
                                                                                
ERREND   GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(TCAUTLEN),TCANOTE)                  
         DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS, ETC.                                                
***********************************************************************         
                                                                                
WEBFILE  DCB   DDNAME=WEBFIL,DSORG=PS,RECFM=FB,LRECL=500,BLKSIZE=0,    X        
               MACRF=PM                                                         
                                                                                
MSGOPEN  DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
                                                                                
WILDCARD DC    CL6'*'                                                           
                                                                                
***********************************************************************         
                                                                                
ACPOPEN  DC    C'<acp>'                                                         
                                                                                
ALINE    DC    C'<agency code="'                                                
ALINEAGY DS    CL6                                                              
         DC    C'" name="'                                                      
ALINENAM DS    CL36                                                             
         DC    C'" add1="'                                                      
ALINEAD1 DS    CL30                                                             
         DC    C'" add2="'                                                      
ALINEAD2 DS    CL30                                                             
         DC    C'" add3="'                                                      
ALINEAD3 DS    CL30                                                             
         DC    C'" add4="'                                                      
ALINEAD4 DS    CL30                                                             
         DC    C'" phone="'                                                     
ALINEPHO DS    CL12                                                             
         DC    C'" office="'                                                    
ALINEOFF DS    CL1                                                              
         DC    C'" employer="'                                                  
ALINEEMP DS    CL3                                                              
         DC    C'" locked="'                                                    
ALINELCK DS    CL1                                                              
         DC    C'" fillablePDF="'                                               
ALINEFIL DS    CL1                                                              
         DC    C'" calculate13WeeksAs3Months="'                                 
ALINE13W DS    CL1                                                              
         DC    C'" productRequired="'                                           
ALINEORQ DS    CL1                                                              
         DC    C'" poRequired="'                                                
ALINEPRQ DS    CL1                                                              
         DC    C'" poValidated="'                                               
ALINEPVL DS    CL1                                                              
         DC    C'" jobRequired="'                                               
ALINEJRQ DS    CL1                                                              
         DC    C'" jobValidated="'                                              
ALINEJVL DS    CL1                                                              
         DC    C'" timesheetPay="'                                              
ALINETPY DS    CL1                                                              
         DC    C'" betaAgency="'                                                
ALINEBAG DS    CL1                                                              
         DC    C'">'                                                            
ALINELNQ EQU   *-ALINE                                                          
ACLOSE   DC    C'</agency>'                                                     
                                                                                
CLINE    DC    C'<client code="'                                                
CLINECLI DS    CL6                                                              
         DC    C'" name="'                                                      
CLINENAM DS    CL36                                                             
         DC    C'" locked="'                                                    
CLINELCK DS    CL1                                                              
         DC    C'">'                                                            
CLINELNQ EQU   *-CLINE                                                          
CCLOSE   DC    C'</client>'                                                     
                                                                                
PLINE    DC    C'<product code="'                                               
PLINEPRD DS    CL6                                                              
         DC    C'" name="'                                                      
PLINENAM DS    CL36                                                             
         DC    C'" locked="'                                                    
PLINELCK DS    CL1                                                              
         DC    C'" />'                                                          
PLINELNQ EQU   *-PLINE                                                          
                                                                                
BLINE    DC    C'<billTo code="'                                                
BLINECOD DS    CL2                                                              
         DC    C'" name="'                                                      
BLINENAM DS    CL36                                                             
         DC    C'">'                                                            
BLINELNQ EQU   *-BLINE                                                          
BCLOSE   DC    C'</billTo>'                                                     
                                                                                
ACPCLOSE DC    C'</acp>'                                                        
                                                                                
***********************************************************************         
                                                                                
UNIOPEN  DC    C'<unions>'                                                      
                                                                                
ULINE    DC    C'<unionlocal code="'                                            
ULINEUNI DS    CL3                                                              
         DC    C' '                                                             
ULINELCL DS    CL3                                                              
         DC    C'" />'                                                          
ULINELNQ EQU   *-ULINE                                                          
                                                                                
UNICLOSE DC    C'</unions>'                                                     
                                                                                
***********************************************************************         
                                                                                
NMDOPEN  DC    C'<newmedia_list>'                                               
                                                                                
NLINE    DC    C'<newmedia code="'                                              
NLINECOD DS    CL6                                                              
         DC    C'" name="'                                                      
NLINENAM DS    CL36                                                             
         DC    C'"/>'                                                           
NLINELNQ EQU   *-NLINE                                                          
                                                                                
NMDCLOSE DC    C'</newmedia_list>'                                              
                                                                                
***********************************************************************         
                                                                                
INTOPEN  DC    C'<internet_list>'                                               
                                                                                
ILINE    DC    C'<internet code="'                                              
ILINECOD DS    CL6                                                              
         DC    C'" name="'                                                      
ILINENAM DS    CL36                                                             
         DC    C'"/>'                                                           
ILINELNQ EQU   *-ILINE                                                          
                                                                                
INTCLOSE DC    C'</internet_list>'                                              
                                                                                
***********************************************************************         
                                                                                
GUAOPEN  DC    C'<performerGuarantees>'                                         
                                                                                
GLINE    DC    C'<pid code="'                                                   
GLINEPID DS    CL6                                                              
         DC    C'">'                                                            
GLINELNQ EQU   *-GLINE                                                          
                                                                                
GALINE   DC    C'<agency code="'                                                
GALINEAY DS    CL6                                                              
         DC    C'" >'                                                           
GALINLNQ EQU   *-GALINE                                                         
                                                                                
GCLINE   DC    C'<client code="'                                                
GCLINECL DS    CL6                                                              
         DC    C'" />'                                                          
GCLINLNQ EQU   *-GCLINE                                                         
                                                                                
GALCLOSE DC    C'</agency>'                                                     
                                                                                
GPICLOSE DC    C'</pid>'                                                        
                                                                                
GUACLOSE DC    C'</performerGuarantees>'                                        
                                                                                
***********************************************************************         
                                                                                
AGTOPEN  DC    C'<agent>'                                                       
                                                                                
AGLINE   DC    C'<agent code="'                                                 
AGLINCOD DS    CL4                                                              
         DC    C'" name="'                                                      
AGLINNAM DS    CL36                                                             
         DC    C'" shortname="'                                                 
AGLINSNM DS    CL16                                                             
         DC    C'" add1="'                                                      
AGLINAD1 DS    CL30                                                             
         DC    C'" add2="'                                                      
AGLINAD2 DS    CL30                                                             
         DC    C'" add3="'                                                      
AGLINAD3 DS    CL30                                                             
         DC    C'" add4="'                                                      
AGLINAD4 DS    CL30                                                             
*                                                                               
         DC    C'" isallowableoverride="'                                       
AGOVRIDE DS    CL1                                                              
         DC    C'"/>'                                                           
AGLINLNQ EQU   *-AGLINE                                                         
                                                                                
AGTCLOSE DC    C'</agent>'                                                      
                                                                                
***********************************************************************         
                                                                                
GU2AOPEN DC    C'<guarantee>'                                                   
                                                                                
G2PIDLIN DC    C'<pid>'                                                         
G2PID    DS    CL6                                                              
G2PIDCLO DC    C'</pid>'                                                        
G2PIDLNQ EQU   *-G2PIDLIN                                                       
                                                                                
G2CODLIN DC    C'<code>'                                                        
G2CODE   DS    CL4                                                              
G2CODCLO DC    C'</code>'                                                       
G2CODLNQ EQU   *-G2CODLIN                                                       
                                                                                
G2TYPLIN DC    C'<type>'                                                        
G2TYPE   DS    CL8                                                              
G2TYPCLO DC    C'</type>'                                                       
G2TYPLNQ EQU   *-G2TYPLIN                                                       
                                                                                
G2CIDLIN DC    C'<corporationpid>'                                              
G2CORPID DS    CL6                                                              
G2CIDCLO DC    C'</corporationpid>'                                             
G2CIDLNQ EQU   *-G2CIDLIN                                                       
*                                                                               
G2PERLIN DC    C'<period>'                                                      
         DC    2C' '                                                            
         DC    C'<startdate>'                                                   
G2PSTART DS    CL8                                                              
         DC    C'</startdate>'                                                  
         DC    2C' '                                                            
         DC    C'<enddate>'                                                     
G2PEND   DS    CL8                                                              
         DC    C'</enddate>'                                                    
         DC    2C' '                                                            
         DC    C'<amount>'                                                      
G2PAMNT  DS    CL15                                                             
         DC    C'</amount>'                                                     
         DC    2C' '                                                            
         DC    C'</period>'                                                     
G2PERLNQ EQU   *-G2PERLIN                                                       
*                                                                               
G2AGYOP1  DC   C'<agencyclients>'                                               
*                                                                               
G2AGYOP2 DC    C'<agencyclient>'                                                
         DC    2C' '                                                            
         DC    C'<agency>'                                                      
G2AGENCY DS    CL6                                                              
         DC    C'</agency>'                                                     
         DC    2C' '                                                            
         DC    C'<client>'                                                      
G2CLIENT DS    CL6                                                              
         DC    C'</client>'                                                     
         DC    2C' '                                                            
         DC    C'</agencyclient>'                                               
G2ACLLNQ EQU   *-G2AGYOP2                                                       
*                                                                               
G2AGCLO1 DC    C'</agencyclients>'                                              
*                                                                               
G2LCKLIN DC    C'<islocked>'                                                    
G2ISLOCK DS    CL1                                                              
G2LCKCLO DC    C'</islocked>'                                                   
G2LCKLNQ EQU   *-G2LCKLIN                                                       
*                                                                               
GUA2CLOSE DC    C'</guarantee>'                                                 
                                                                                
***********************************************************************         
                                                                                
TCANOTE  DC    C'AUTONOTE*US-TALENT_TEAM@MEDIAOCEAN.COM:'                       
TCAUTREA DS    CL15                                                             
TCAUTLEN EQU   *-TCANOTE                                                        
TCOPFAIL DC    CL(L'TCAUTREA)'MQ OPEN FAILED'                                   
TCPTFAIL DC    CL(L'TCAUTREA)'MQ PUT ERROR'                                     
TCCLFAIL DC    CL(L'TCAUTREA)'MQ CLOSE FAILED'                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER LOCAL WORKING STORAGE                                   
***********************************************************************         
                                                                                
RECD     DSECT                                                                  
OPTIONS  DS    X                   PROGRAM STATUS                               
OPTFULL  EQU   X'80'               FULL BLAST                                   
OPTFILE  EQU   X'40'               OUTPUT TO FILE                               
DSPACE   DS    C                   DSPACE                                       
MQPRENUM DS    C                   MQ PREAMBLE NUMBER                           
                                                                                
AKEY     DS    CL(L'KEY)           AGENCY KEY                                   
CKEY     DS    CL(L'KEY)           CLIENT KEY                                   
                                                                                
         ORG   AKEY                                                             
GUATAB   DS    XL1201                                                           
RECDLNQ  EQU   255                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDTWADCOND                                                                    
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDMASTD                                                                       
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP5C   01/28/16'                                      
         END                                                                    
