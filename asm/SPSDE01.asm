*          DATA SET SPSDE01    AT LEVEL 038 AS OF 10/10/11                      
*PHASE T23101B                                                                  
T23101   TITLE 'SPSDE01 - SPOT SUPERDESK - DOWNLOAD HEADERS'                    
T23101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSDE01*,R7                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTT-TWAD)       A(TV MARKET LIST)                      
         ST    R1,AMKTLSTT                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTR-TWAD)       A(R MARKET LIST)                       
         ST    R1,AMKTLSTR                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(MKTLSTR-TWAD)       A(X MARKET LIST)                       
         ST    R1,AMKTLSTX                                                      
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(CLTLST-TWAD)        A(CLIENT LIST)                         
         ST    R1,ACLTLST                                                       
         LR    R1,RA                     A(TWA)                                 
         AH    R1,=Y(CLTLSTX-TWAD)       A(END OF CLIENT LIST)                  
         ST    R1,ACLTLSTX                                                      
*                                                                               
         CLC   VERSION,=X'02000015'      VERSION 2.0.0.21 & HIGH ONLY           
         BNL   *+8                                                              
         MVI   SVSDPRF+3,C'N'            OR NO ATTACHMENTS BY OFFICE            
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVC   ERROR,=Y(INVALID)                                                
         MVI   SENTHDR,C'N'              FLAG TO CHECK IF HEADER SENT           
         MVI   OVFLAG1,0                                                        
*                                                                               
         CLI   SVRCVEL+1,X'01'           INITIAL MOMENT (USER LOGIN)            
         BE    RCV01H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'03'           ACTIVE MARKET DOWNLOAD                 
         BE    RCV03H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'07'           AUTHORIZATION MARKET DETAILS           
         BE    RCV07H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'0A'           APPROVAL HISTORY                       
         BE    RCV0AH                                                           
*                                                                               
         CLI   SVRCVEL+1,X'0F'           COMPLETE MARKET DOWNLOAD               
         BE    RCV0FH                                                           
*                                                                               
         CLI   SVRCVEL+1,X'23'           CLIENT OFFICE REQUEST                  
         BE    SND23H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'24'           CLIENT SDUDEFS REQUEST                 
         BE    SND24H                                                           
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 01 HEADER - INITIAL MOMENT (LOGIN)                                    
* VALIDATE THE USER CODE LOGGIN IN AS                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV01H   DS    0H                                                               
*                                                                               
* VERSION CHECK                                                                 
         CLC   VERSION,=X'02000010'      VERSION 2.0.0.16 & HIGH ONLY           
         BNL   R01H05                     ALLOWED!                              
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGXTR(4),=X'02000010'                                         
         GOTO1 AADDDATA,DMCB,AFABLK,FALAYRF,FALAYRF                             
         B     EXIT                                                             
*                                                                               
* DO SOME INITIALIZING OF TABLES                                                
R01H05   NI    FLAG2,X'FF'-ALLMKTT-ALLMKTR-ALLMKTX                              
         L     RE,AMKTLSTT               T MARKET LIST TABLE                    
         LR    R0,RE                                                            
         LHI   R1,L'MKTLSTT                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AMKTLSTR               R MARKET LIST TABLE                    
         LR    R0,RE                                                            
         LHI   R1,L'MKTLSTR                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AMKTLSTX               X MARKET LIST TABLE                    
         LR    R0,RE                                                            
         LHI   R1,L'MKTLSTX                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,ACLTLST                CLIENT LIST TABLE                      
         ST    RE,CLTPNTR                                                       
         LR    R0,RE                                                            
         LHI   R1,L'CLTLST                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   QMED,C'T'                 VALIDATE THE MEDIA TO GET THE          
         GOTO1 VALIMED                   AGENCY CODE                            
*                                                                               
         MVC   ERROR,=Y(NOTAUTH)         NOT AUTHORIZED                         
         CLI   SVAPROF+7,C'C'            TEST CANADA                            
         BE    SNDERMSG                  SEND ERROR                             
*                                                                               
         MVI   SVBYRFLT,0                INITIALIZE BUYER'S CLT FILTER          
         MVC   SVSPVCD,SVUSRCD           INITIALIZE SUPERVISOR CODE             
         MVC   SVUSER,SVUSER2                                                   
*                                                                               
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE PROFILE          
         BNE   *+8                                                              
         BAS   RE,BLDOFC                 BINSRCH OF MED/CLT/OFFICE              
*                                                                               
R01H08   CLI   SVUSRTP,C'B'              LOGGING ON AS A BUYER?                 
         BNE   R01H10                    NO, READ SUPERVISOR RECORD             
         MVC   ERROR,=Y(INVBYR)          INVALID BUYER                          
         GOTO1 VALIBYR                                                          
         BNE   SNDERMSG                  SEND ERROR                             
         MVC   SVBYRCD,SVUSRCD                                                  
         MVC   SVUCD,SVSPVCD             NOW READ SUPERVISOR RECORD             
         MVC   WORK(L'SVUSRNM),SVUSRNM                                          
*                                                                               
R01H10   MVC   ERROR,=Y(SPVNTFND)        SUPERVISOR NOT FOUND                   
         GOTO1 VALISPV                                                          
         BNE   SNDERMSG                  SEND ERROR                             
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         MVI   SVSPIDFL,0          GET SUPV FLAG FROM RECORD                    
         MVI   SVSPDAYS,0                                                       
         USING SPVRECD,R6                                                       
         L     R6,AIO3             FOR PLAN=Y SUPVS                             
         LA    R6,SPVEL                                                         
         USING SPIDELD,R6                                                       
         MVI   ELCODE,SPIDELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   SVSPIDFL,SPIDFLAG   SUPV IS A PLANNER                            
         MVC   SVSPDAYS,SPIDDAYS   SAVE DAYS RESTRICTION                        
         DROP  R6                                                               
*                                                                               
         CLI   DDS,C'Y'                  DDS TERMINAL?                          
         BE    SND01H                    YES - ALL ACCESS                       
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#TPASS   EXTRACT CONNECT PWD         
         OC    FULL(2),FULL              ON SECURITY?                           
         BZ    SND01H                    NO                                     
*                                                                               
         CLI   SVUSRTP,C'B'              LOGGING ON AS A BUYER                  
         BNE   R01H30                    NO, SO DON'T CHECK BUYER REC           
         CLI   SVSDPRF+14,C'N'           SKIP PID CHECKS?                       
         BE    *+14                                                             
         CLC   SVBPIDNO,FULL             THIS USER?                             
         BNE   R01H20                    NO                                     
         MVC   SVUSRNM,WORK              REPLACE SPV NAME W/ BYR NAME           
         B     SND01H                                                           
*                                                                               
R01H20   MVI   SVUSRTP,C'O'              CHECK IF SPV ACTING AS A BUYER         
R01H30   MVC   ERROR,=Y(NOTAUTH)         NOT AUTHORIZED                         
         CLI   SVSDPRF+14,C'N'           SKIP PID CHECKS?                       
         BE    *+14                                                             
         CLC   SVSPIDNO,FULL             MATCHES SUPERVISOR PID?                
         BNE   SNDERMSG                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 01 HEADER - DOWNLOAD BUYERS, MARKETS, CLIENTS, BASED ON USER             
* ON ENTRY:  SVUSRTP = S, IF LOGGED ON AS A SUPERVISOR                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND01H   DS    0H                                                               
*                                                                               
         CLI   SVUSRTP,C'B'              LOGGING ON AS A BUYER                  
         BNE   *+10                                                             
         MVC   SVUSRNM,WORK              REPLACE SPV NAME W/ BYR NAME           
*                                                                               
         MVI   BRKFLAG,0                                                        
         BAS   RE,SNDMED                 SEND VALID MEDIAS FOR THIS AGY         
*                                                                               
         LA    R2,MEDTAB                 TABLE OF VALID MEDIA TYPES             
         CLI   SVUSRTP,C'B'              BUYER?                                 
         BNE   S01H20                    NO, MUST BE A SUPERVISOR               
*                                                                               
* IF USER IS A BUYER JUST SEND BUYER'S CLIENTS AND MARKETS                      
S01H10   CLI   0(R2),X'FF'               CHECK IF DONE WITH ALL MEDIAS          
         BE    S01H90                                                           
         BAS   RE,SNDMKT                 SEND BYR'S MARKETS                     
         BAS   RE,SNDCLT                 SEND BYR'S CLIENTS                     
         AHI   R2,MEDTABL                                                       
         B     S01H10                                                           
*                                                                               
* IF USER IS A SPV, SEND SPV CLIENTS AND THEN EACH BUYER'S MARKETS              
S01H20   OI    OVFLAG1,SNSPVCLT          SEND ALL SPV'S CLIENTS                 
S01H30   CLI   0(R2),X'FF'               CHECK IF DONE WITH ALL MEDIAS          
         BE    S01H40                                                           
         BAS   RE,SNDCLT                 SEND SPV'S CLIENTS                     
         AHI   R2,MEDTABL                                                       
         B     S01H30                                                           
*                                                                               
S01H40   NI    OVFLAG1,X'FF'-SNSPVCLT                                           
         LA    R6,KEY                    SEND EACH BUYER'S MARKETS              
         XC    KEY,KEY                                                          
         USING BYRRECD,R6                                                       
         MVC   BYRPTYP2,=X'0DE3'         LOOK FOR PASSIVES TO GET BYRS          
         MVC   BYRPAGY2,SAVEKEY+2        SAVEKEY HAS SUPV REC - USE AGY         
         MVC   SAVEKEY,KEY               SET NEW SAVEKEY                        
         GOTO1 HIGH                                                             
*                                                                               
S01H50   CLC   BYRPTYP2(3),SAVEKEY       RECORD ID + AGENCY                     
         BNE   S01H90                                                           
         MVC   SAVEKEY,KEY               SAVE LAST PASSIVE READ                 
*                                                                               
         CLI   SVSDPRF+12,C'Y'           FILTER BUYERS WITH SUPV GROUP          
         BNE   *+14                                                             
         CLC   BYRPOFC2,SVUSER2          SAME GROUP CODE AS SUPV                
         BNE   S01H85                    NO - THEN SKIP                         
*                                                                               
         MVC   SVBYRCD,BYRPBYR2                                                 
         CLC   SVUSER(6),BYRPOFC2        ALREADY HAVE SPV RECORD?               
         BE    S01H60                                                           
         MVC   SVUSER(6),BYRPOFC2        NO, THEN GET IT                        
         GOTO1 VALISPV                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY    RESTORE READ SEQ                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
S01H60   DS    0H                                                               
         USING BYRRECD,R6                                                       
         L     R6,AIO2                   GET BUYER REC IN AIO2                  
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BYREL                                                         
         USING BYRNAMED,R6                                                      
         TM    BYROPT1,BYROPT1_ASS       ASSISTANT BUYER?                       
         BO    S01H80                    YES, IGNORE ASST BUYERS                
         MVC   SVBYRFLT,BYRFILT                                                 
*                                                                               
         LA    R2,MEDTAB                 TABLE OF VALID MEDIA TYPES             
S01H70   CLI   0(R2),X'FF'               DONE?                                  
         BE    S01H80                                                           
*                                                                               
         BAS   RE,SNDMKT                 SEND BYR'S MARKETS                     
         BAS   RE,SNDCLT                                                        
         AHI   R2,MEDTABL                                                       
         B     S01H70                                                           
*                                                                               
S01H80   XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
S01H85   GOTO1 SEQ                                                              
         NI    OVFLAG1,X'FF'-SNTBYRCD                                           
         LA    R6,KEY                                                           
         B     S01H50                                                           
*                                                                               
S01H90   DS    0H                                                               
         CLC   VERSION,=X'0101001F'      VERSION 1.1.0.31 & HIGH ONLY           
         BL    SND01HX                                                          
         CLI   SVSPDAYS,0          IF D=## IS SET ON SUPV                       
         BNE   S01H93               THEN  A DIFFERENT +-FILTER DAYS.            
         CLC   TWAAGY,=C'MC'             FOR THIS AGENCY SEND DOWN              
         BNE   S01H100                    A DIFFERENT +-FILTER DAYS.            
         CLI   SVUSRTP,C'S'              LOGGING ON AS A SUPV                   
         BNE   S01H94                                                           
         CLI   SVSPDAYS,0                                                       
         BE    S01H94                                                           
S01H93   MVC   BYTE,SVSPDAYS                                                    
         B     *+8                                                              
S01H94   MVI   BYTE,45                                                          
         LA    R1,MCFLTDY                                                       
         LA    R4,BYTE                                                          
         SR    R5,R5                     GIVES OUTPUT DATA LENGTH               
         BRAS  RE,SENDD                                                         
*                                                                               
S01H100  DS    0H                                                               
         CLC   VERSION,=X'02000001'      VERSION 2.0.0.1 & HIGH ONLY            
         BL    SND01HX                                                          
         MVI   BYTE,0                                                           
****     CLI   SVSDPRF+1,C'Y'      TBD OPTION ON? SEND ANYWAY!!                 
****     BNE   S01H105             THEN CAN'T SET AS PLANNER                    
         TM    SVSPIDFL,SPIDFPLN   SUPV IS A PLANNER                            
         BNO   S01H105                                                          
         MVI   BYTE,1                                                           
S01H105  LA    R4,BYTE                                                          
         LA    R1,MCSPLAN                                                       
         SR    R5,R5                     GIVES OUTPUT DATA LENGTH               
         BRAS  RE,SENDD                                                         
*                                                                               
S01H110  DS    0H                        SEND PROFILES                          
         CLC   VERSION,=X'02000015'      VERSION 2.0.0.21 & HIGH ONLY           
         BL    SND01HX                                                          
*                                                                               
         CLI   SVSDPRF+6,C' '      FILL IN DEFAULT PROFILE VALUES               
         BH    *+8                                                              
         MVI   SVSDPRF+6,C'Y'      YES PLANNERS CAN ADD COMMENTS                
         CLI   SVSDPRF+7,C' '                                                   
         BH    *+8                                                              
         MVI   SVSDPRF+7,C'Y'      YES PLANNER CHANGES ARE REVISIONS            
         CLI   SVSDPRF+8,C' '                                                   
         BH    *+8                                                              
         MVI   SVSDPRF+8,C'Y'      YES ADD ATTACHMENTS W/O REVISIONS            
         CLI   SVSDPRF+10,C' '                                                  
         BH    *+8                                                              
         MVI   SVSDPRF+10,C'N'     AGENCY DOES NOT USE WEBDAV                   
         CLI   SVSDPRF+11,C' '                                                  
         BH    *+8                                                              
         MVI   SVSDPRF+11,C'Y'     DUE DATE BEFORE ISSUE DATE                   
*                                                                               
         XC    WORK2(10),WORK2                                                  
         MVC   WORK2(1),SVSDPRF+3        ATTACHMENTS BY OFFICE                  
         LA    R5,1                      GIVES OUTPUT DATA LENGTH               
         CLC   VERSION,=X'02000028'      VERSION 2.0.0.40 & HIGH ONLY           
         BL    S01H115                                                          
         MVC   WORK2+1(1),SVSDPRF+6      PLANNERS CANNOT ADD COMMENTS           
         MVC   WORK2+2(1),SVSDPRF+7      PLANNER CHANGES ARE REVISIONS          
         MVC   WORK2+3(1),SVSDPRF+8      ATTACHMENTS W/O REVISIONS              
         LA    R5,4                      GIVES OUTPUT DATA LENGTH               
         CLC   VERSION,=X'03000019'      VERSION 3.0.0.25 & HIGH ONLY           
         BL    S01H115                                                          
         MVC   WORK2+4(1),SVSDPRF+10     ANGENCY USES WEBDAV                    
         MVC   WORK2+5(1),SVSDPRF+11     DUE DATE BEFORE ISSUE DATE             
         LA    R5,6                      GIVES OUTPUT DATA LENGTH               
S01H115  LA    R4,WORK2                                                         
         LA    R1,MCSDPRF                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
S01H120  DS    0H                                                               
         CLC   VERSION,=X'02000028'      VERSION 2.0.0.40 & HIGH ONLY           
         BL    S01H130                                                          
         CLI   SVSDPRF+5,C'Y'            USING SD UDEFS?                        
         BNE   *+8                                                              
         BAS   RE,SNDUDEF                SEND UDEFS                             
*                                                                               
S01H130  DS    0H                                                               
         CLC   VERSION,=X'03000011'      VERSION 3.0.0.17 & HIGH ONLY           
         BL    S01H140                                                          
         MVC   WORK2(2),QAGY             SEND ALPHA AGENCY TO PC                
         LA    R5,2                      GIVES OUTPUT DATA LENGTH               
         LA    R4,WORK2                                                         
         LA    R1,MCAGNCY                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
S01H140  DS    0H                                                               
         BAS   RE,SNDWEBD                SEND WEBDAV USERIDS AND PWORDS         
*                                                                               
SND01HX  B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SENDS A SUBSET OF SUPERDESK VALID MEDIAS AND AGENCY'S VALID MEDIAS            
* ON ENTRY:  MEDTAB- LIST OF VALID MEDIAS FOR NON-CANADIAN AGENCIES             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDMED   NTR1                                                                   
         MVI   LASTMED,0                 CLEAR LAST MEDIA SENT TO PC            
         LA    R1,X'0001'                LOCATE 01 HEADER                       
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,SVUSRNM                USER NAME                              
         LA    R5,L'SVUSRNM              LENGTH OF USER NAME                    
         LA    R1,SVUSRNM+L'SVUSRNM-1                                           
SMED02   CLI   0(R1),C' '                                                       
         BH    SMED05                                                           
         BCTR  R1,0                                                             
         BCTR  R5,0                                                             
         CR    R4,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
SMED05   LA    R1,MCUSRNM                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
* SEND MEDIA AND MEDIA NAME FROM AGENCY RECORD FOR MEDIAS SUPPORTING            
         GOTO1 VALIMED                   GET AGENCY RECORD IN AIO1              
         USING MEDTABD,R2                                                       
         LA    R2,MEDTAB                 TABLE OF VALID MEDIA TYPES             
*                                                                               
SMED10   CLI   0(R2),X'FF'               DONE?                                  
         BE    EXIT                                                             
         L     R6,AIO1                                                          
         USING AGYMEDEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     SMED30                                                           
*                                                                               
SMED20   BAS   RE,NEXTEL                                                        
SMED30   BNE   SMED40                    GET NEXT MEDIA                         
         CLC   AGYMEDCD,MEDCD            MATCH MEDIA CODE                       
         BNE   SMED20                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   0(L'AGYMEDCD,R4),AGYMEDCD MEDIA CODE                             
         LA    R4,L'AGYMEDCD-1(R4)                                              
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'AGYMEDEX,R4),AGYMEDEX MEDIA NAME                             
         LA    R4,L'AGYMEDEX-1(R4)                                              
         BRAS  RE,SETDLM                                                        
*                                                                               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R1,MCAGYMD                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
SMED40   AHI   R2,MEDTABL                                                       
         B     SMED10                                                           
         DROP  R2                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY: AIO3 CONTAINS THE ADDRESS OF THE SUPERVISOR RECORD                  
*           R2 POINTS TO ENTRY IN MEDTAB WITH MEDIA OF CLIENTS SENDING          
*             MEDCD HAS MEDIA, EX. T                                            
*             MEDCLT HAS THE ELEMENT CODE FOR THIS MEDIA                        
*             MEDB HAS BINARY CODE FOR MEDIA X'01'                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -MVI          
SNDCLT   NTR1                                                                   
         USING MEDTABD,R2                                                       
         TM    OVFLAG1,SNSPVCLT          SEND ALL SPV'S CLIENTS?                
         BO    SCLT10                    YES                                    
         CLI   SVUSRTP,C'B'              IF BUYER THEN CHECK FILTER             
         BNE   EXIT                      IF SUPV - SNSPVCLT SET                 
         CLI   SVBYRFLT,0                IS THERE A FILTER?                     
         BE    EXIT                      NO, BUYER SEES ALL CLIENTS             
*                                                                               
         USING SPVCLTD,R6                                                       
SCLT10   L     R6,AIO3                                                          
         MVC   ELCODE,MEDCLT             ELEMENT CODE FOR THIS MEDIA            
         BAS   RE,GETEL                                                         
         BE    SCLT20                                                           
*                                                                               
         L     R6,AIO3                                                          
         MVC   ELCODE,MEDCLT                                                    
         OI    ELCODE,X'10'              CHECK FOR CLIENT GROUPS                
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         OI    OVFLAG2,CLTGRP            SUPV HAS CLIENT GROUPS                 
*                                                                               
SCLT20   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         LA    R3,BLOCK+245              DON'T LET STRING GET TOO LONG          
*                                                                               
SCLT30   OI    OVFLAG1,SPVHSCLT          SUPERVISOR HAS CLIENTS                 
         MVC   QMED,MEDCD                MEDIA CODE                             
         TM    OVFLAG1,SNSPVCLT          SEND ALL SPV'S CLIENTS?                
         BO    SCLT40                    YES                                    
*        TM    OVFLAG2,CLTGRP            SUPV HAS CLIENT GROUPS?                
*        BO    SCLT40                    YES                                    
         CLC   SPVFILT,SVBYRFLT          NO, COMPARE W/ BYR'S FILTER            
         BNE   SCLT45                                                           
*                                                                               
SCLT40   BAS   RE,CHKBUFF                CHECK IF BUFFER FULL                   
*                                                                               
         TM    OVFLAG2,CLTGRP            SUPV HAS CLIENT GROUPS?                
         BNZ   SCLT50                    YES                                    
         USING SPVCLTD,R6                                                       
         MVC   0(L'SPVCLT,R4),SPVCLT     CLIENT CODE                            
         LA    R4,L'SPVCLT-1(R4)                                                
*                                                                               
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE                  
         BNE   SCLT42                                                           
         XC    WORK(5),WORK                                                     
         MVC   WORK(1),MEDB                                                     
         GOTO1 VCLPACK,DMCB,SPVCLT,WORK+1                                       
         BAS   RE,GETOFC                                                        
         LA    R4,1(R4)                                                         
*                                                                               
         CLC   VERSION,=X'03000026'      VERSION 3.0.0.38 & HIGH ONLY           
         BL    SCLT41                                                           
         MVC   0(2,R4),WORK+3      SEND 2 CHAR OFFICE                           
         MVI   2(R4),C'|'                                                       
         LA    R4,3(R4)                                                         
         B     SCLT43                                                           
*                                                                               
SCLT41   MVC   0(1,R4),WORK+3      SEND 1 CHAR OFFICE                           
         MVI   1(R4),C'|'                                                       
         LA    R4,2(R4)                                                         
         B     SCLT43                                                           
*                                                                               
SCLT42   BRAS  RE,SETDLM                                                        
SCLT43   OI    OVFLAG1,SNDCLTS           CLIENTS TO SEND                        
         MVC   QCLT,SPVCLT                                                      
         BAS   RE,ADDCLT                                                        
SCLT45   BAS   RE,NEXTEL                                                        
         BE    SCLT30                                                           
         B     SCLT80                                                           
*                                                                               
* READ CLIENT GROUP RECORD                                                      
         USING SPVCGRD,R6                READ                                   
SCLT50   XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING GRPKEY,R5                                                        
         MVI   GRPKTYP,GRPKTYPQ          X'0D'                                  
         MVI   GRPKSTYP,GRPKCTYQ         X'04'                                  
         MVC   GRPKAGMD,BAGYMD           A/M                                    
         NI    GRPKAGMD,X'F0'            DROP MEDIA CODE                        
         OC    GRPKAGMD,MEDB             USE CORRECT MEDIA CODE                 
*                                                                               
         MVC   GRPKID,SPVCGRID           ID                                     
         MVC   GRPKCODE,SPVCGRP          CODE                                   
         DROP  R5,R6                                                            
         ST    R6,GRPPNTR                SAVE PTR TO SUPV REC ELEM              
*                                                                               
         MVC   AIO,AIO4                  DONT CLOBBER SUPV REC                  
         GOTO1 HIGH                                                             
         B     SCLT54                                                           
SCLT52   GOTO1 SEQ                                                              
*                                                                               
SCLT54   CLC   KEY(GRPKMSQL),KEYSAVE     CHECK MATCHES GROUP                    
         BNE   SCLT70                                                           
****     BE    *+6                       IF DOESN'T EXIST - DON'T DIE           
****     DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SVELCODE,ELCODE           SAVE ELCODE OF SUPV RE                 
         MVI   ELCODE,GRPVALCQ           X'30'                                  
         USING GRPVALD,R6                                                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
SCLT60   BAS   RE,NEXTEL                                                        
         BNE   SCLT52              CHECK FOR NEXT GROUP RECORD                  
         BAS   RE,CHKBUFF                CHECK IF BUFFER FULL                   
         MVC   0(L'SPVCLT,R4),GRPVALUE   A CLIENT FROM CGROUP                   
         LA    R4,L'SPVCLT-1(R4)                                                
*                                                                               
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE                  
         BNE   SCLT62                                                           
         XC    WORK(5),WORK                                                     
         MVC   WORK(1),MEDB                                                     
         GOTO1 VCLPACK,DMCB,GRPVALUE,WORK+1                                     
         BAS   RE,GETOFC                                                        
*                                                                               
         CLC   VERSION,=X'03000026'      VERSION 3.0.0.38 & HIGH ONLY           
         BL    SCLT61                                                           
         LA    R4,1(R4)                                                         
         MVC   0(2,R4),WORK+3      2 CHAR OFFICE                                
         MVI   2(R4),C'|'                                                       
         LA    R4,3(R4)                                                         
         B     SCLT63                                                           
*                                                                               
SCLT61   LA    R4,1(R4)                                                         
         MVC   0(1,R4),WORK+3      1 CHAR OFFICE                                
*                                                                               
SCLT62   BRAS  RE,SETDLM                                                        
SCLT63   OI    OVFLAG1,SNDCLTS           CLIENTS TO SEND                        
         MVC   QCLT,GRPVALUE                                                    
         BAS   RE,ADDCLT                 ADD CLIENT TO TABLE                    
         B     SCLT60                    NEXT CLT FROM CGROUP                   
         DROP  R6                                                               
*                                                                               
SCLT70   DS    0H                                                               
         MVC   ELCODE,SVELCODE           RESTORE SUPV ELCODE                    
         L     R6,GRPPNTR                REST PTR TO CURRENT CGRP ELEM          
         MVC   AIO,AIO2                  RESTORE AIO TO SUPV REC                
         BAS   RE,NEXTEL                 GET NEXT SUPV ELEM                     
         BE    SCLT40                                                           
*                                                                               
SCLT80   NI    OVFLAG2,X'FF'-CLTGRP                                             
         TM    OVFLAG1,SNDCLTS           ANY CLIENTS TO SEND?                   
         BNO   EXIT                                                             
         LA    R1,MCDLCLT                DELIMITTED STRING                      
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE                  
         BNE   *+8                                                              
         LA    R1,MCDLCLTO               DELIMITTED STRING/OFFICE               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         NI    OVFLAG1,X'FF'-SNDCLTS                                            
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         DROP  R2                                                               
*                                                                               
* CHECK IF BUFFER IS FULL AND NEEDS TO BE SENT TO FALINK                        
CHKBUFF  NTR1                                                                   
         USING MEDTABD,R2                                                       
         CLC   QMED,LASTMED              CHECK IF THE SAME MEDIA                
         BE    CBUFF20                   YES, DON'T SEND AGAIN                  
         MVC   LASTMED,QMED                                                     
*                                                                               
         TM    OVFLAG1,SNDCLTS           ANY CLIENTS TO SEND?                   
         BNO   CBUFF10                                                          
         LA    R1,MCDLCLT                DELIMITTED STRING                      
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE                  
         BNE   *+8                                                              
         LA    R1,MCDLCLTO               DELIMITTED STRING/OFFICE               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
CBUFF10  LA    R1,MCMEDIA                SEND MEDIA FOR CLIENT                  
         ST    R4,FULL                                                          
         LA    R4,QMED                                                          
         SR    R5,R5                                                            
         BAS   RE,SENDD                                            *            
         L     R4,FULL                                                          
*                                                                               
CBUFF20  CR    R4,R3                                                            
         BNH   CBUFFX                                                           
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R1,MCDLCLT                DELIMITTED STRING                      
         CLI   SVSDPRF+3,C'Y'            ATTACHMENTS BY OFFICE                  
         BNE   *+8                                                              
         LA    R1,MCDLCLTO               DELIMITTED STRING/OFFICE               
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         LA    R3,BLOCK+245              DON'T LET STRING GET TOO LONG          
CBUFFX   XIT1  REGS=(R3,R4)                                                     
*                                                                               
* SAVE CLIENT IN TABLE IF USER IS RESPONSIBLE FOR IT                            
ADDCLT   NTR1                                                                   
         CLI   SVUSRTP,C'B'              LOGGING ON AS A BUYER?                 
         BE    *+14                                                             
         CLC   SVUSER2,SVUSER            SUPERVISOR THE CURRENT USER?           
         BNE   ADDCLTX                                                          
*                                                                               
         L     R5,CLTPNTR                                                       
         USING CLTLSTD,R5                                                       
         LA    R5,CLTLSTL(R5)                                                   
         L     R0,ACLTLSTX               TEST PAST END OF BUFFER                
         MVC   ERROR,=Y(TABLERR)         TABLE OVERFLOW ERROR                   
         CR    R5,R0                                                            
         BH    SNDERMSG                  SEND ERROR                             
         L     R5,CLTPNTR                                                       
*                                                                               
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLTCODE,BCLT                                                     
         MVC   CLTFLAG,MEDB                                                     
         LA    R5,CLTLSTL(R5)                                                   
         ST    R5,CLTPNTR                                                       
*                                                                               
ADDCLTX  B     EXIT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        BUILD BINSRCH TABLE OF MEDIA/CLIENT/OFFICE TO SUPPORT                  
*        ATTACHMENTS BY OFFICE                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BLDOFC   NTR1                                                                   
         XC    OFCPAR3,OFCPAR3     MAKE SURE RECORD COUNT IS ZERO               
         LA    R1,OFCTAB                                                        
         ST    R1,OFCPAR2          AND TABLE IS SET                             
*                                                                               
         USING CLTPHDR,R5                READ CLIENT OFFICE POINTERS            
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   CPKEYTYP(2),=X'0D80'                                             
         MVC   CPKEYAM,BAGYMD                                                   
         NI    CPKEYAM,X'F0'             DROP MEDIA CODE                        
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         B     BO10                                                             
BO10SEQ  GOTO1 SEQ                                                              
BO10     CLC   KEY(2),SAVEKEY      END OF TYPE                                  
         BNE   BOX                                                              
         MVC   BYTE,CPKEYAM                                                     
         NI    BYTE,X'F0'          AGENCY                                       
         CLC   BYTE,SAVEKEY+2      SAME AGENCY                                  
         BNE   BOX                                                              
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CPKEYAM                                                  
         NI    WORK,X'0F'                     KEEP MEDIA HEX                    
         MVC   WORK+1(2),CPKEYCLT             CLIENT                            
         MVC   WORK+3(1),CPKEYOFF             OFFICE                            
*                                                                               
***      MVC   OFF1CHAR,CPKEYOFF                                                
***      BAS   RE,CHK1CHAR         WILL RETURN SPACE IF NOT VALID               
***      MVC   WORK+3(1),OFF1CHAR                                               
*                                                                               
         GOTO1 VBINSRCH,OFCPAR1,(1,WORK)  INSERT IF NOT FOUND                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BO10SEQ                                                          
BOX      XIT1                                                                   
         DROP  R5                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        CHECK 1 CHAR OFFICE AGAINST TABLE OF VALID 1 CHAR OFFICES              
*        ATTACHMENTS BY OFFICE                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
CHK1CHAR NTR1                                                                   
         LA    R2,DOFFTAB          DEFAULT 1 CHAR OFFICES                       
C1C10    CLI   0(R2),X'FF'         EOT                                          
         BE    C1C20                                                            
         CLC   OFF1CHAR,0(R2)                                                   
         BE    C1CX                IN TABLE = OK TO SEND TO PC                  
         LA    R2,1(R2)                                                         
         B     C1C10                                                            
                                                                                
C1C20    MVI   OFF1CHAR,C' '       NOT IN TABLE = SEND SPACE                    
*                                                                               
C1CX     XIT1                                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*        FIND CLIENT'S OFFICE IN BINSRCH TABLE TO SUPPORT                       
*        ATTACHMENTS BY OFFICE                                                  
*        KEY ALREADY BUILT IN WORK                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
GETOFC   NTR1                                                                   
*                                                                               
         GOTO1 VBINSRCH,OFCPAR1,WORK                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         MVC   WORK(4),0(RE)                                                    
         CLC   VERSION,=X'03000026'      VERSION 3.0.0.38 & HIGH ONLY           
         BL    GOX                 ONLY 1 CHAR OFFICES                          
*                                                                               
         USING OFFICED,OFCBLK                                                   
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,WORK+3                                                    
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),ACOMFACS                            
         MVC   WORK+3(2),OFCOFC2                                                
         CLI   WORK+3,C'|'                                                      
         BNE   GOX                                                              
         MVC   WORK+3(2),=C'  '    SPACES ARE INVALID - BETTER THAN |           
*                                                                               
GOX      XIT1                                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND MARKETS ASSOCIATED WITH A BUYER                                          
* ON ENTRY: AIO2 CONTAINS THE ADDRESS OF A BUYER RECORD                         
*           R2 POINTS TO ENTRY IN MEDTAB WITH MEDIA OF CLIENTS SENDING          
*             MEDCD HAS CHARACTER MEDIA CODE, EX. T                             
*             MEDB HAS BINARY CODE FOR MEDIA, EX X'01' FOR MEDIA T              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDMKT   NTR1                                                                   
         USING MEDTABD,R2                                                       
         USING BYRMKTD,R6                                                       
         L     R6,AIO2                                                          
         MVI   DEBUG,0                                                          
*                                                                               
         MVC   ELCODE,MEDMKT             ELEMENT CODE FOR THIS MEDIA            
         BAS   RE,GETEL                                                         
SMKT10   BE    SMKT15                                                           
         TM    OVFLAG2,MKTGRP            ALREADY DID MKT GROUP ELEMENTS         
         BO    SMKT100                                                          
         L     R6,AIO2                                                          
         MVC   ELCODE,MEDMGRP            NO, SO PROCESS NOW                     
         BAS   RE,GETEL                                                         
         BNE   SMKT100                                                          
         OI    OVFLAG2,MKTGRP                                                   
*                                                                               
SMKT15   ST    R6,FULL                   SAVE REGISTER 6                        
         MVC   QMED,MEDCD                MEDIA CODE                             
*                                                                               
         CLI   SVUSRTP,C'S'              SUPERVISOR?                            
         BNE   SMKT20                                                           
         TM    OVFLAG1,SNTBYRCD          ALREADY SENT BUYER CODE?               
         BO    SMKT20                                                           
*                                                                               
         L     R6,AIO2                                                          
         USING BYRRECD,R6                                                       
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         SR    R0,R0                                                            
         CLC   SVUSER2,SVUSER            SUPERVISOR THE CURRENT USER?           
         BE    *+8                                                              
         LA    R0,1                      FLAG TO SAY NOT THIS SPV'S BYR         
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   0(L'BYRKBYR,R4),BYRKBYR   BUYER CODE                             
         LA    R4,L'BYRKBYR-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'BYRKOFC,R4),BYRKOFC   BUYING GROUP                           
         LA    R4,L'BYRKOFC-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         LA    R6,BYREL                                                         
         USING BYRNAMED,R6                                                      
         MVC   0(L'BYRFNAME,R4),BYRFNAME BUYER NAME                             
         LA    R4,L'BYRFNAME-1(R4)                                              
         CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         MVI   1(R4),C' '                                                       
         MVC   2(L'BYRLNAME,R4),BYRLNAME BUYER NAME                             
         LA    R4,L'BYRLNAME+1(R4)                                              
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'BYROFF,R4),BYROFF     BUYER OFFICE                           
         LA    R4,L'BYROFF-1(R4)                                                
         BRAS  RE,SETDLM                                                        
*                                                                               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         LA    R1,MCBYRCD                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         OI    OVFLAG1,SNTBYRCD+NEWMKTLS ALREADY SENT BUYER CODE                
*                                                                               
SMKT20   CLC   QMED,LASTMED              CHECK IF THE SAME MEDIA                
         BE    SMKT40                    YES, DON'T SEND AGAIN                  
         MVC   LASTMED,QMED                                                     
*                                                                               
         TM    OVFLAG1,SNDMKTS           ANY MARKETS TO SEND?                   
         BNO   SMKT30                                                           
         LA    R1,MCDLMKT                DELIMITTED STRING                      
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
SMKT30   LA    R1,MCMEDIA                SEND MEDIA FOR MARKET                  
         LA    R4,QMED                                                          
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
         OI    OVFLAG1,NEWMKTLS                                                 
*                                                                               
SMKT40   TM    OVFLAG1,NEWMKTLS                                                 
         BNO   SMKT50                                                           
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         LA    R3,BLOCK+245              MAXIMUM LENGTH FOR STRING              
*                                                                               
SMKT50   CR    R4,R3                                                            
         BNH   SMKT60                                                           
         LA    R1,MCDLMKT                DELIMITTED STRING OF MARKETS           
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         USING BYRMKGD,R6                MARKET GROUP ELEMENT                   
SMKT60   L     R6,FULL                   RESTORE R6                             
         TM    OVFLAG2,MKTGRP            PROCESSING MKT GROUP ELEMENTS          
         BNO   SMKT80                                                           
*                                                                               
         USING MKGRECD,R5                READ MARKET GROUP RECORDS              
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         NI    MKGPAGMD,X'F0'            DROP MEDIA CODE                        
         OC    MKGPAGMD,MEDB             USE CORRECT MEDIA CODE                 
         MVC   MKGPMID(3),BYRMKGID       GROUP ID & GROUP                       
         GOTO1 HIGH                                                             
*                                                                               
SMKT70   LA    R5,KEY                                                           
         CLC   MKGKEY(MKGPMKT-MKGPTYP),KEYSAVE                                  
         BNE   SMKT90                                                           
         MVC   BMKT,MKGPMKT              SEND MARKETS IN MARKET GROUP           
         BAS   RE,DWNMKT                                                        
*                                                                               
         CR    R4,R3                                                            
         BNH   SMKT74                                                           
         LA    R1,MCDLMKT                DELIMITTED STRING OF MARKETS           
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
***      LA    R1,SENDD                                                         
***      MVI   0(R1),0                                                          
***      LA    R1,SENDH                                                         
***      MVI   0(R1),0                                                          
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
SMKT74   GOTO1 SEQ                                                              
         B     SMKT70                                                           
         DROP  R5,R6                                                            
*                                                                               
         USING BYRMKTD,R6                MARKET ELEMENT                         
SMKT80   MVC   BMKT,BYRMKT                                                      
         BAS   RE,DWNMKT                                                        
         DROP  R6                                                               
*                                                                               
SMKT90   BAS   RE,NEXTEL                                                        
         B     SMKT10                                                           
*                                                                               
SMKT100  TM    OVFLAG1,SNDMKTS           ANY MARKETS TO SEND?                   
         BNO   SNDMKTX                                                          
         LA    R1,MCDLMKT                DELIMITTED STRING                      
         LA    R5,BLOCK                                                         
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
SNDMKTX  NI    OVFLAG1,X'FF'-SNDMKTS                                            
         NI    OVFLAG2,X'FF'-MKTGRP                                             
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* DOWNLOAD MARKET AND SAVE IT IN A TABLE OF VALID MARKET IF USER IS             
* RESPONSIBLE FOR IT                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DWNMKT   NTR1                                                                   
         SR    R0,R0                     MARKET                                 
         CLC   BMKT,=X'FFFF'             ALL MARKETS?                           
         BE    *+8                       THEN JUST SEND MKT=0 FOR ALL           
         ICM   R0,3,BMKT                                                        
         BRAS  RE,SETNUM                                                        
         OI    OVFLAG1,SNDMKTS           MARKETS TO SEND                        
         NI    OVFLAG1,X'FF'-NEWMKTLS    SENT NEW LIST                          
*                                                                               
* SAVE MARKET IN TABLE IF USER IS RESPONSIBLE FOR IT                            
         CLI   SVUSRTP,C'B'              LOGGING ON AS A BUYER?                 
         BE    *+14                                                             
         CLC   SVUSER2,SVUSER            SUPERVISOR THE CURRENT USER?           
         BNE   DWNMKTX                                                          
         BAS   RE,MKTBIT                 ADD MARKET TO TABLE                    
*                                                                               
DWNMKTX  XIT1  REGS=(R4)                                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TURN ON BIT FOR A MARKET IN A TABLE -                                         
* ON ENTRY: R2 CONTAINS ADDRESS OF ENTRY IN MEDIA TABLE                         
*           BMKT CONTAINS THE MARKET                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
MKTBIT   NTR1                                                                   
         SR    R1,R1               PUT MARKET IN R1                             
         ICM   R1,3,BMKT                                                        
*                                                                               
         CLC   BMKT,=X'FFFF'       IF MARKET =FFFF                              
         BNE   MKTBIT03            TURN ON FLAG FOR ALL MARKTES                 
         OC    FLAG2,MEDMKTBT                                                   
         B     EXIT                                                             
*                                                                               
MKTBIT03 L     R4,AMKTLSTT         POINT R2 TO THE CORRECT TABLE                
         CLI   MEDCD,C'T'                                                       
         BE    MKTBIT05                                                         
         L     R4,AMKTLSTR                                                      
         CLI   MEDCD,C'R'                                                       
         BE    MKTBIT05                                                         
         L     R4,AMKTLSTX                                                      
*                                                                               
MKTBIT05 CHI   R1,9999             IF MKT > 9999 SOMETHING IS WRONG!            
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'8'            DIVISOR                                      
         AR    R4,R1               ADD TO QUOTIENT                              
         STC   R0,FULL             REMAINDER IN 1ST BYTE OF FULL                
*                                                                               
         USING BITTABD,R3                                                       
         LA    R3,BITTAB                                                        
MKTBIT10 CLC   FULL(1),BITREM      MATCH REMAINDER IN TABLE TO                  
         BE    MKTBIT20            FIND OUT WHICH BIT TO TURN ON                
         LA    R3,BITLNQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   MKTBIT10                                                         
         DC    H'00'               END OF TABLE                                 
*                                                                               
MKTBIT20 OC    0(1,R4),BITBIT      TURN ON BIT DEPENDING ON REMAINDER           
         B     EXIT                                                             
*                                                                               
*        REMAINDER/BIT TURNED ON                                                
BITTAB   DC    X'00',X'80'                                                      
         DC    X'01',X'40'                                                      
         DC    X'02',X'20'                                                      
         DC    X'03',X'10'                                                      
         DC    X'04',X'08'                                                      
         DC    X'05',X'04'                                                      
         DC    X'06',X'02'                                                      
         DC    X'07',X'01'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 03 HEADER - ACTIVE MARKET DOWNLOAD                                    
* DOWNLOAD ALL MARKET THAT ARE ACTIVE (NOT CANCELLED OR DELETED)                
* REGARDLESS OF USER                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV03H   DS    0H                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVI   AUTKMKT+1,1                                                      
*                                                                               
R03H10   GOTO1 HIGH                                                             
         CLC   AUTKEY(AUTKMKT-AUTKEY),KEYSAVE                                   
         BNE   RCV03HX                                                          
*                                                                               
         TM    AUTKSTAT,AUTKSCAN+AUTKSDL MARKET CANCELLED/DELETED?              
         BO    R03H20                                                           
*                                                                               
         LA    R1,X'0003'                SEND 03 HEADER                         
         BAS   RE,SENDH                                                         
         LA    R1,MCMKT                                                         
         SR    R5,R5                                                            
         LA    R4,AUTKMKT                MARKET                                 
         BRAS  RE,SENDD                                                         
*                                                                               
R03H20   MVC   AUTKSTA,=X'FFFF'          SKIP STATION RECORDS                   
         B     R03H10                                                           
RCV03HX  B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 07 HEADER - AUTHORIZATION MARKET DETAILS                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV07H   DS    0H                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AUTKMKT,BMKT                                                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUTKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'           READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,MINFELQ          X'01' - INFORMATION ELEMENT              
         BRAS  RE,GETEL2               MUST BE THERE, REQUIRED                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MINFEL,R6                                                        
         OC    SVRPDUE,SVRPDUE                                                  
         BZ    *+10                                                             
         MVC   MINFRDDT,SVRPDUE        DATE BUY REPORTS DUE TO PLANING          
         OC    SVRPSNT,SVRPSNT                                                  
         BZ    *+10                                                             
         MVC   MINFRSDT,SVRPSNT        DATE REPORTS SENT TO FIELD               
         GOTO1 PUTREC                                                           
*                                                                               
RCV07HX  B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 0A HEADER - MARKET APPROVAL HISTORY                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV0AH   DS    0H                                                               
         USING AUTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AUTKMKT,BMKT                                                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   AUTKEY(L'AUTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                   GET MARKET RECORD                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R6,KEY                                                           
         BAS   RE,SND0AH                 SEND APPROVAL HISTORY                  
*                                                                               
RCV0AHX  B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 0A HEADER - MARKET APPROVAL HISTORY                                      
* ON ENTRY: AIO CONTAINS ADDRESS OF AUTH MARKET RECORD                          
* DOWNLOAD COMPLETED, APROVED, REJECT, AND CANCELLED STATUS ELEMENTS            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* WHAT DO I SEND IF THERE IS NO DATA?                                           
SND0AH   NTR1                                                                   
         L     R6,AIO                                                           
         ST    R6,FULL             SAVE MKT REC AIO                             
         USING AUTRECD,R6                                                       
         MVC   BMKT,AUTKMKT                                                     
         MVC   SVAUKEY,KEY                                                      
*                                                                               
* FIND FIRST STATUS ELEMENT                                                     
         LA    R6,AUTEL                  FIRST ELEMENT                          
         USING MSTAELD,R6                                                       
         MVI   ELCDLO,MSTAREJQ           LOWEST STATUS ELEMENT CODE             
         MVI   ELCDHI,MSTADELQ           HIGHEST STATUS ELEMENT CODE            
S0AH10   BRAS  RE,NEXTEL3                                                       
         BNE   S0AH40                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
*****>   CLC   SVUSER,MSTABYGR           CHANGE TO ALWAYS RE-READ               
*****>   BE    S0AH30                                                           
         MVC   SVUSER,MSTABYGR                                                  
         OI    FLAG1,NAMEONLY                                                   
         CLI   MSTAEL,MSTACMPQ           IF ACTION COMPLETE CHECK BUYER         
         BNE   S0AH15                     FIRST                                 
         GOTO1 VALIBYR                                                          
         BE    S0AH20                                                           
S0AH15   GOTO1 VALISPV                   OTHERWISE ONLY CHECK SPV               
         BE    S0AH20                                                           
         XC    SVUSRNM,SVUSRNM                                                  
         MVC   SVUSRNM(2),MSTABYGR       USE CODE IF NO RECORD                  
         MVC   SVUSRNM+3(4),MSTACODE                                            
*                                                                               
S0AH20   NI    FLAG1,X'FF'-NAMEONLY                                             
S0AH30   MVC   0(L'SVUSRNM,R4),SVUSRNM                                          
         LA    R4,L'SVUSRNM-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         CLI   MSTAEL,MSTACMSQ           SUPERVISOR COMPLETE?                   
         BNE   *+12                      NOPE                                   
         IC    R0,=X'08'                 SEND BUYER COMPLETE                    
         B     *+8                                                              
         IC    R0,MSTAEL                                                        
         CLI   MSTAEL,MSTADELQ                                                  
         BNE   *+8                                                              
         IC    R0,=X'0C'                 SEND DELETED                           
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   WORK2(L'MSTADATE),MSTADATE                                       
         BRAS  RE,SETDATE                                                       
*                                                                               
         LA    R1,X'000A'                SEND 0A HEADER                         
         BAS   RE,SENDH                                                         
         LA    R1,MCDLSTR                DELIMITTED STRING                      
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,MSTACMT                                                       
         ZIC   R5,MSTALEN                LENGTH OF ELEMENT                      
         SH    R5,=Y(MSTALENQ)           SUBTRACT LEN OF EL W/O COMMENT         
         LA    R1,MCAPCOM                APPROVAL COMMENT                       
         BAS   RE,SENDD                                                         
         B     S0AH10                    GET NEXT ELEMENT                       
*                                                                               
* CHECK FOR MARKET STATUS HISTORY TO SEND TOO                                   
*                                                                               
         USING AUTRECD,R6                                                       
S0AH40   L     R6,FULL                   SAVED AIO                              
         LA    R6,AUTEL                  FIRST ELEMENT                          
         USING MSHIELD,R6                                                       
         MVI   ELCDLO,MSHIELQ            X'38'                                  
         MVI   ELCDHI,MSHIELQ                                                   
S0AH50   BRAS  RE,NEXTEL3                                                       
         BNE   S0AH100                                                          
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   SVUSER,MSHIBYGR                                                  
         OI    FLAG1,NAMEONLY                                                   
         CLI   MSHISTAT,MSTACMPQ         COMPLETED BY BUYER                     
         BNE   S0AH55                                                           
         GOTO1 VALIBYR                                                          
         BE    S0AH60                                                           
S0AH55   GOTO1 VALISPV                   OTHERWISE ALL SPV ACTIONS              
         BE    S0AH60                                                           
         XC    SVUSRNM,SVUSRNM                                                  
         MVC   SVUSRNM(2),MSHIBYGR       USE CODE IF NO RECORD                  
         MVI   SVUSRNM+2,C'-'                                                   
         MVC   SVUSRNM+3(4),MSHICODE                                            
*                                                                               
S0AH60   NI    FLAG1,X'FF'-NAMEONLY                                             
         MVC   0(L'SVUSRNM,R4),SVUSRNM                                          
         LA    R4,L'SVUSRNM-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         CLI   MSHISTAT,MSTACMSQ         SUPERVISOR COMPLETE?                   
         BNE   *+12                      NOPE                                   
         IC    R0,=X'08'                 SEND BUYER COMPLETE                    
         B     *+8                                                              
         IC    R0,MSHISTAT                                                      
         CLI   MSHISTAT,MSTADELQ                                                
         BNE   *+8                                                              
         IC    R0,=X'0C'                 SEND DELETED                           
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   WORK2(L'MSHIDATE),MSHIDATE                                       
         XC    WORK2(3),=X'FFFFFF'                                              
         BRAS  RE,SETDATE                                                       
*                                                                               
         IC    R0,MSHIMREV         SEND MKT REV #                               
         BRAS  RE,SETNUM                                                        
*                                                                               
         LA    R1,X'000A'                SEND 0A HEADER                         
         BAS   RE,SENDH                                                         
         LA    R1,MCDLSTR                DELIMITTED STRING                      
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,MSHICMT                                                       
         ZIC   R5,MSHILEN                LENGTH OF ELEMENT                      
         SH    R5,=Y(MSHILENQ)           SUBTRACT LEN OF EL W/O COMMENT         
         LA    R1,MCAPCOM                APPROVAL COMMENT                       
         BAS   RE,SENDD                                                         
         B     S0AH50                    GET NEXT ELEMENT                       
*                                                                               
S0AH100  TM    FLAG2,ALLMKTS             SENDING FOR ALL MKTS?                  
         BNO   SND0AHX                   NO, THEN DONE                          
         MVI   XSP,C'Y'                                                         
         XC    KEY,KEY                   RE-READ MARKET RECORD                  
         MVC   KEY(L'SVAUKEY),SVAUKEY                                           
         GOTO1 HIGH                                                             
SND0AHX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 0F HEADER - MARKET LIST                                               
* DOWNLOAD ALL MARKETS ON THE FILE FOR AN AGENCY                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV0FH   DS    0H                                                               
         USING MEDTABD,R2                                                       
         LA    R2,MEDTAB                 TABLE OF VALID MEDIA TYPES             
*                                                                               
R0FH10   CLI   0(R2),X'FF'                                                      
         BE    RCV0FHX                                                          
         MVC   QMED,MEDCD                                                       
         BAS   RE,SND0FH                                                        
         NI    OVFLAG1,X'FF'-FMEDSENT                                           
         AHI   R2,MEDTABL                                                       
         B     R0FH10                                                           
RCV0FHX  B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 0F HEADER: MEDIA (ONCE FOR EACH MEDIA TYPE)                              
*                 MARKET AND MARKET NAME (FOR EACH MARKET)                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND0FH   NTR1                                                                   
*                                                                               
         USING MKTREC,R6                                                        
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,=C'0001'                                                 
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
S0FH05   MVC   KEY+MKTKAGY-MKTRECD(2),QAGY                                      
S0FH07   GOTO1 HIGHSTA                                                          
         B     S0FH20                                                           
*                                                                               
S0FH10   MVC   KEY(L'MKTKEY),MKTKEY                                             
         GOTO1 SEQSTA                                                           
S0FH20   CLC   KEY(MKTKMKT-MKTREC),MKTKEY   CHECK IF MEDIA CHANGED              
         BNE   EXIT                                                             
         CLC   KEY+(MKTKAGY-MKTRECD)(L'MKTKAGY),MKTKAGY                         
         BE    S0FH25                                                           
         MVC   KEY(MKTKAGY-MKTRECD),MKTKEY                                      
         BH    S0FH05                           FILL IN AGENCY CODE             
         MVC   KEY+MKTKAGY-MKTRECD(2),=X'FFFF'  GET NEXT MARKET NUMBER          
         B     S0FH07                                                           
*                                                                               
S0FH25   TM    OVFLAG1,FHDRSENT          NEED TO SEND HEADER?                   
         BO    S0FH30                                                           
         LA    R1,X'000F'                SEND 0F HEADER                         
         BAS   RE,SENDH                                                         
         OI    OVFLAG1,FHDRSENT                                                 
*                                                                               
S0FH30   TM    OVFLAG1,FMEDSENT          NEED TO SEND MEDIA                     
         BO    S0FH40                                                           
*                                                                               
         LA    R1,MCMEDIA                SEND MEDIA                             
         LA    R4,QMED                                                          
         SR    R5,R5                     CLEAR OVERRIDE LENGTH                  
         BAS   RE,SENDD                                                         
         OI    OVFLAG1,FMEDSENT                                                 
*                                                                               
S0FH40   XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   0(L'MKTKMKT,R4),MKTKMKT   MARKET CODE                            
         LA    R4,L'MKTKMKT-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'MKTNAME,R4),MKTNAME   MARKET NAME                            
         LA    R4,L'MKTNAME-1(R4)                                               
         BRAS  RE,SETDLM                                                        
*                                                                               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R1,MCDLSTR                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         B     S0FH10                                                           
         EJECT                                                                  
         DROP  R6                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND CLIENT OFFICE                                                  *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND23H   LA    R1,X'0023'                SEND 23 HEADER                         
         BAS   RE,SENDH                                                         
*                                                                               
         L     R6,AIO              CLIENT REC SHOULD BE HERE!                   
         USING CLTHDR,R6                                                        
         LA    R4,COFFICE                                                       
*                                                                               
         USING OFFICED,OFCBLK                                                   
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),ACOMFACS                            
*                                                                               
***      MVC   OFF1CHAR,COFFICE                                                 
***      BAS   RE,CHK1CHAR         WILL RETURN SPACE IF NOT VALID               
***      LA    R4,OFF1CHAR                                                      
*                                                                               
         LA    R1,MCCLTOF                SEND OFFICE                            
         LA    R4,OFCOFC2          IF 1 CHAR SPACE WILL BE STRIPPED             
         LA    R5,2                      BY FALINK ANYWAY                       
*                                                                               
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND CLIENT UDEFS                                                   *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND24H   LA    R1,X'0024'                SEND 24 HEADER                         
         BAS   RE,SENDH                                                         
*                                                                               
         USING SDURECD,R6                READ SDUDEFS                           
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SDUKTYP(2),=X'0D08'                                              
         MVC   SDUKAM,BAGYMD                                                    
         MVC   SDUKCLT,BCLT                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(5),SAVEKEY                                                   
         BE    S24H10                                                           
*                                                                               
         XC    BLOCK(250),BLOCK                                                 
         LA    R4,BLOCK                                                         
         MVC   0(1,R4),QMED        MEDIA|                                       
         BRAS  RE,SETDLM                                                        
         MVC   0(L'QCLT,R4),QCLT   CLIENT|                                      
         LA    R4,L'QCLT-1(R4)                                                  
         BRAS  RE,SETDLM                                                        
         MVI   0(R4),C'0'          NUMBER 0 MEANS NO RECORD                     
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         LA    R1,MCUDEFS          UDEFS                                        
         BAS   RE,SENDD                                                         
         B     S24HX               AND DONE                                     
*                                                                               
S24H10   MVC   AIO,AIO4                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         BAS   RE,FMTUDEF          FORMATS AND SENDS DATA                       
S24HX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND UDEF INFO - INITIAL MOMENT                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDUDEF  NTR1                                                                   
*                                                                               
         USING SDURECD,R6                READ SDUDEFS                           
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SDUKTYP(2),=X'0D08'                                              
         MVC   SDUKAM,BAGYMD                                                    
         NI    SDUKAM,X'F0'              DROP MEDIA CODE                        
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(2),SAVEKEY      END OF TYPE                                  
         BNE   SU99                THERE ARE NO UDEF RECORDS ON FILE            
         B     SU10                                                             
SU10SEQ  GOTO1 SEQ                                                              
SU10     CLC   KEY(2),SAVEKEY      END OF TYPE                                  
         BNE   SUX                                                              
         LA    R6,KEY                                                           
         MVC   BYTE,SDUKAM                                                      
         NI    BYTE,X'F0'          AGENCY                                       
         CLC   BYTE,SAVEKEY+2      SAME AGENCY                                  
         BNE   SUX                                                              
*                                                                               
         MVC   BYTE,SDUKAM                                                      
         NI    BYTE,X'0F'          KEEP MEDIA                                   
         MVI   QMED,C'T'                                                        
         CLI   BYTE,X'01'                                                       
         BE    SU12                                                             
         MVI   QMED,C'R'                                                        
         CLI   BYTE,X'02'                                                       
         BE    SU12                                                             
         MVI   QMED,C'X'                                                        
*                                                                               
SU12     OC    SDUKCLT,SDUKCLT                                                  
         BNZ   SU20                                                             
         MVC   QCLT,=C'ALL'                                                     
         B     SU30                                                             
*                                                                               
SU20     BAS   RE,CHKCLT           CHECK TO SEND CLIENT                         
         BNE   SU10SEQ                                                          
         MVC   BCLT,SDUKCLT                                                     
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,BCLT),QCLT                               
*                                                                               
SU30     MVC   AIO,AIO4                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO4                                                          
         BAS   RE,FMTUDEF          SEND RECORD DATA                             
         B     SU10SEQ             AND GET NEXT                                 
*                                                                               
SU99     XC    BLOCK(250),BLOCK    NO DATA                                      
         LA    R4,BLOCK                                                         
         MVI   0(R4),C'T'                                                       
         BRAS  RE,SETDLM                                                        
         MVC   0(3,R4),=C'ALL'                                                  
         LA    R4,3(R4)                                                         
         BRAS  RE,SETDLM                                                        
         MVI   0(R4),C'0'          NUMBER 0 MEANS NO RECORD/DATA                
         LA    R4,1(R4)                                                         
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA LENGTH                     
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         LA    R1,MCUDEFS          UDEFS                                        
         BAS   RE,SENDD                                                         
*                                                                               
SUX      MVC   AIO,AIO1                                                         
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND UDEF INFO - QMED AND QCLT ARE SET- R6 POINTS TO SDUDEF RECORD            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
FMTUDEF  NTR1                                                                   
         USING SDURECD,R6                                                       
*                                                                               
         MVI   ELCODE,SDUDELQ      X'20'                                        
         BAS   RE,GETEL                                                         
         BE    FU20                                                             
*                                                                               
         XC    BLOCK(250),BLOCK    NO DATA                                      
         LA    R4,BLOCK                                                         
         MVC   0(1,R4),QMED        MEDIA|                                       
         BRAS  RE,SETDLM                                                        
         MVC   0(L'QCLT,R4),QCLT   CLIENT|                                      
         LA    R4,L'QCLT-1(R4)                                                  
         BRAS  RE,SETDLM                                                        
         MVI   0(R4),C'0'          NUMBER 0 MEANS NO RECORD/DATA                
         LA    R4,1(R4)                                                         
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA LENGTH                     
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         LA    R1,MCUDEFS          UDEFS                                        
         BAS   RE,SENDD                                                         
         B     FUX                 AND DONE                                     
*                                                                               
FU10NXT  BAS   RE,NEXTEL                                                        
         BNE   FUX                                                              
*                                                                               
         USING SDUDELD,R6                                                       
FU20     XC    BLOCK(250),BLOCK                                                 
         LA    R4,BLOCK                                                         
         MVC   0(1,R4),QMED        MEDIA|                                       
         BRAS  RE,SETDLM                                                        
         MVC   0(L'QCLT,R4),QCLT   CLIENT|                                      
         LA    R4,L'QCLT-1(R4)                                                  
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   BYTE,SDUDNUM                                                     
         OI    BYTE,X'F0'          MAKE CHAR NUMBER 1-5|                        
         MVC   0(1,R4),BYTE                                                     
         LA    R4,1(R4)                                                         
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(L'SDUDDESC,R4),SDUDDESC    DESCRIPTION|                        
         LA    R4,L'SDUDDESC-1(R4)                                              
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVI   0(R4),C'Y'                                                       
         TM    SDUDFLAG,SDUDFREQ   REQUIRED FIELD = Y/N |                       
         BO    *+8                                                              
         MVI   0(R4),C'N'                                                       
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(1,R4),SDUDEDIT    EDIT RULE|                                   
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         CLI   SDUDEDIT,SDUDEYN    ZERO LEN FOR Y/N                             
         BE    FU30                                                             
         CLI   SDUDEDIT,SDUDEDT    ZERO LEN FOR DATE                            
         BE    FU30                                                             
         ZIC   R0,SDUDMLEN         MAX LENGTH                                   
FU30     BRAS  RE,SETNUM                                                        
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
*                                                                               
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         LA    R1,MCUDEFS          UDEFS                                        
         BAS   RE,SENDD                                                         
         B     FU10NXT                                                          
*                                                                               
FUX      XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND WEBDAV USERIDS AND PASSWORDS BY OFFICE - INITIAL MOMENT                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDWEBD  NTR1                                                                   
*                                                                               
         MVI   AGY2CHAR,C'N'       SET TO NOT TURNED ON FOR 2 CHARS             
         USING OFFICED,OFCBLK                                                   
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,QAGY                                                      
         MVI   OFCOFC,C'?'                                                      
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),ACOMFACS                            
*                                                                               
         TM    OFCINDS,OFCINOLA    NOT 2 CHARS                                  
         BO    SW06                                                             
         MVI   AGY2CHAR,C'Y'       IF BIT OFF THEN ARE ON 2 CHARS               
         CLC   VERSION,=X'03000026'      VERSION 3.0.0.38 & HIGH ONLY           
         BNL   SW06                                                             
         MVC   ERROR,=Y(OFCVRSN)   NEED TO UPGRADE FOR 2 CHAR OFC               
         B     SNDERMSG                  SEND ERROR                             
*                                                                               
SW06     CLI   SVSDPRF+10,C'Y'           AGENCY USES WEBDAV                     
         BNE   SWX                                                              
         CLC   VERSION,=X'03000026'      VERSION 3.0.0.38 & HIGH ONLY           
         BNL   SW08                                                             
         MVC   ERROR,=Y(WEBVRSN)         PROFILE ON BUT NOT RIGHT VRSN          
         B     SNDERMSG                  SEND ERROR                             
*                                                                               
         USING MOFRECD,R6                READ MOFFICE RECS                      
SW08     LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   MOFKTYP,MOFKTYPQ    C'O'                                         
         MVI   MOFKSUB,MOFKS1Q     BY 1 CHAR OFFICE                             
         MVC   MOFKAGY,QAGY                                                     
*                                                                               
         MVC   SVKEY,KEY                                                        
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY                  
         B     SW10                                                             
SW10SEQ  GOTO1 VDATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEY,KEY                  
SW10     CLC   KEY(24),SVKEY        END OF OFFICE RECS FOR AGY                  
         BNE   SWX                                                              
         LA    R6,KEY                                                           
         CLI   MOFKSYS,X'02'        ONLY WANT SPOT                              
         BNE   SW10SEQ                                                          
         CLI   MOFKC2OF,C'|'        CAN'T USE OUR DELIMETER                     
         BE    SW10SEQ                                                          
         CLI   MOFKC2OF+1,C'|'      CAN'T USE OUR DELIMETER                     
         BE    SW10SEQ                                                          
*                                                                               
         MVC   OFF2CHAR,MOFKC2OF                                                
         MVC   OFF1CHAR,MOFK1OF                                                 
*                                                                               
         CLI   AGY2CHAR,C'Y'       IF AGENCY IS ON 2 CHARS - SEND               
         BE    SW14                                                             
         BAS   RE,CHK1CHAR         IF STILL ON 1 CHAR                           
         CLI   OFF1CHAR,C' '       IF RETURED A SPACE THEN SKIP                 
         BE    SW10SEQ             BECAUSE IT'S NOT A VALID 1 CHAR              
         MVC   OFF2CHAR(1),OFF1CHAR    IF VALID STILL SEND OLD 1 CHAR           
         MVI   OFF2CHAR+1,C' '         VALUE - TO MATCH CLIENT RECS             
*                                                                               
SW14     GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',MOFKDA,AIO4,    X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,42(R6)           FIRST ELEM                                   
SW20     CLI   0(R6),0                                                          
         BE    SW10SEQ                                                          
         CLI   0(R6),OLUIPELQ      X'0C' WEBDAV ELEM                            
         BE    SW30                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SW20                                                             
*                                                                               
         USING OLUIPD,R6                                                        
SW30     CLC   OLUIPUID,SPACES                                                  
         BNE   SW40                                                             
         CLC   OLUIPASS,SPACES                                                  
         BE    SW10SEQ                                                          
SW40     XC    BLOCK(250),BLOCK                                                 
         LA    R4,BLOCK                                                         
         MVC   0(2,R4),OFF2CHAR    OFFICE                                       
         LA    R4,2(R4)                                                         
         BRAS  RE,SETDLM                                                        
         MVC   0(L'OLUIPUID,R4),OLUIPUID                                        
         LA    R4,L'OLUIPUID-1(R4)                                              
         BRAS  RE,SETDLM                                                        
         MVC   0(L'OLUIPASS,R4),OLUIPASS                                        
         LA    R4,L'OLUIPASS-1(R4)                                              
         BRAS  RE,SETDLM                                                        
*                                                                               
         BCTR  R4,0                      GET RID OF LAST DELIMITTER             
         LA    R1,MCWEBDAV                                                      
         LA    R5,BLOCK                                                         
         SR    R5,R4                     GIVES OUTPUT DATA LENGTH               
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         B     SW10SEQ                                                          
*                                                                               
SWX      MVC   AIO,AIO1                                                         
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND UDEF INFO - CHECK IF CLIENT SENT - R6 POINTS TO SDUDEF RECORD            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
CHKCLT   NTR1                                                                   
         USING SDURECD,R6                                                       
         USING CLTLSTD,R2                                                       
CHKCLT10 L     R2,ACLTLST                                                       
CHKCLT20 OC    CLTCODE,CLTCODE                                                  
         BZ    CHKCLNEX                                                         
         L     R3,ACLTLSTX                                                      
         CR    R2,R3                                                            
         BNL   CHKCLNEX                                                         
*                                                                               
         CLC   CLTCODE,SDUKCLT                                                  
         BNE   CHKCLT30                                                         
*                                                                               
         MVC   BYTE,CLTFLAG                                                     
         NI    BYTE,X'FF'-X'F0'                                                 
         MVC   BYTE2,SDUKAM                                                     
         NI    BYTE2,X'FF'-X'F0'                                                
         CLC   BYTE,BYTE2                                                       
         BNE   CHKCLT30                                                         
         B     CHKCLEQX                                                         
*                                                                               
CHKCLT30 LA    R2,CLTLSTL(R2)                                                   
         B     CHKCLT20                                                         
*                                                                               
CHKCLEQX SR    RC,RC                                                            
CHKCLNEX LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                    *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDH    LR    R0,RE                                                            
         CLI   SENTHDR,C'Y'                                                     
         BE    SENDHX                                                           
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         MVI   SENTHDR,C'Y'                                                     
SENDHX   LR    RE,R0                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                    *         
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT        *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BACK UP TO LAST NONBLANK AND INSERT BAR DELIMITER                   *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SETDLM   CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLM                                                        
         MVI   1(R4),C'|'                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND A DATE IN MMDDYY FORM                                          *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SETDATE  DS    0H                                                               
         LR    R5,RE                                                            
         OC    WORK2(3),WORK2                                                   
         BZ    SDATE10                                                          
         GOTO1 VDATCON,DMCB,(3,WORK2),(X'20',WORK)                              
         MVC   0(4,R4),WORK+2                                                   
         MVC   4(2,R4),WORK                                                     
         AHI   R4,5                                                             
SDATE10  BRAS  RE,SETDLM                                                        
         LR    RE,R5                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT SEMICOLON DELIMITER         *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SETNUM   DS    0H                                                               
         EDIT  (R0),(10,(R4)),ZERO=NOBLANK,ALIGN=LEFT                           
         AR    R4,R0                                                            
         MVI   0(R4),C'|'                                                       
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
NEXTEL3  CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTEL3X                                                         
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL3                                                          
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL3                                                          
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTEL3X LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
SNDERMSG GOTO1 SENDMSG                                                          
EXIT     XIT1                                                                   
         GETEL R6,24,ELCODE                                                     
         GETEL2 R6,42,ELCODE                                                    
                                                                                
         LTORG                                                                  
*                                                                               
* VALID MEDIA TABLE, SEE MEDTABD                                                
MEDTAB   DC    C'R',AL1(MEDR),X'03',X'03',X'13',AL1(ALLMKTR)                    
         DC    C'T',AL1(MEDT),X'02',X'02',X'12',AL1(ALLMKTT)                    
         DC    C'X',AL1(MEDX),X'04',X'04',X'14',AL1(ALLMKTX)                    
         DC    X'FF'                                                            
*                                                                               
* DEFAULT ONE CHARACTER OFFICE TABLE                                            
* CHARACTERS OMITTED ARE: 0 - $ * , =                                           
* WE WON'T SEND 1 CHAR OFFICE TO PC UNLESS IT IS ONE OF THESE                   
*                                                                               
DOFFTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ' LETTERS                            
         DC    C'123456789'                  NUMBERS                            
         DC    C'`º\''./'                   SPECIAL CHARAC                     
         DC    X'5E'                         SEMICOLON                          
         DC    C'~!@#%^&&()_+{}|:"<>?'       (SHIFT) SPECIAL CHARACTERS         
         DC    X'FF'                         SEMICOLON                          
DOFFTQ   EQU   *-DOFFTAB                                                        
*                                                                               
OFCPAR1  DC    A(0)                ENTRY                                        
OFCPAR2  DC    A(OFCTAB)           A(TABLE)                                     
OFCPAR3  DC    F'0'                RECORD COUNT                                 
OFCPAR4  DC    A(4)                RECORD LENGTH                                
OFCPAR5  DC    A(3)                KEYDSPL/KEYLEN                               
OFCPAR6  DC    A((OFCTBX-OFCTAB)/L'OFCTAB)                                      
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFCTAB*'                                                    
OFCTAB   DS    6000XL4                                                          
OFCTBX   EQU   *                                                                
*                                                                               
       ++INCLUDE SPSDEWRK                                                       
*                                                                               
BITTABD  DSECT                                                                  
BITREM   DS    XL1                                                              
BITBIT   DS    XL1                                                              
BITLNQ   EQU   *-BITTABD                                                        
*                                                                               
MEDTABD  DSECT                                                                  
MEDCD    DS    C                         CHARACTER MEDIA CODE                   
MEDB     DS    X                         BINARY MEDIA CODE (FROM A/M)           
MEDCLT   DS    X                         EL CODE FOR CLT IN SPV RECORD          
MEDMKT   DS    X                         EL CODE FOR MKT IN BYR RECORD          
MEDMGRP  DS    X                         EL CODE FOR MGRP IN BYR REC            
MEDMKTBT DS    X                         BIT FOR FLAG2 FOR ALL MARKETS          
MEDTABL  EQU   *-MEDTABD                                                        
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVWORK                    480 BYTES IN OVWORK                    
AMKTLSTT DS    A                         ADDRESS OF TV MARKET LIST              
AMKTLSTR DS    A                         ADDRESS OF R MARKET LIST               
AMKTLSTX DS    A                         ADDRESS OF X MARKET LIST               
ACLTLST  DS    A                         ADDRESS OF CLIENT LIST                 
ACLTLSTX DS    A                         ADDRESS OF END OF CLIENT LIST          
CLTPNTR  DS    A                         POINTER FOR CLTLST                     
*                                                                               
GRPPNTR  DS    A                         POINTER FOR GROUP ELEMS                
*                                                                               
SAVEKEY  DS    XL13                                                             
SVKEY    DS    XL32                                                             
LASTMED  DS    C                         LAST MEDIA SENT TO PC                  
*                                                                               
SENTHDR  DS    C                         FLAG Y=ALREADY SENT HEADER             
*                                                                               
OVFLAG1  DS    X                                                                
FMEDSENT EQU   X'80'                     MEDIA SENT                             
FHDRSENT EQU   X'40'                     HEADER SENT                            
SNSPVCLT EQU   X'20'                     SEND SUPERVISOR'S CLIENTS              
SNTBYRCD EQU   X'10'                     ALREADY SENT BUYER CODE                
SPVHSCLT EQU   X'08'                     SUPERVISOR HAS CLIENTS                 
NEWMKTLS EQU   X'04'                     SEND NEW DELIMITTED MKT LIST           
SNDMKTS  EQU   X'02'                     THERE ARE MARKETS TO SEND              
SNDCLTS  EQU   X'01'                     THERE ARE CLIENTS TO SEND              
*                                                                               
OVFLAG2  DS    X                                                                
MKTGRP   EQU   X'80'                     PROCESSING MARKET GROUPS               
CLTGRP   EQU   X'40'                     PROCESSING CLIENT GROUPS               
*                                                                               
OFF1CHAR DS    CL1                                                              
OFF2CHAR DS    CL2                                                              
AGY2CHAR DS    CL1                 Y = AGENCY USES 2 CHAR OFFICES               
*                                                                               
SVELCODE DS    CL1                       SAVES ELCODE FOR >1 REC READS          
DEBUG    DS    XL1                                                              
OFCBLK   DS    XL(OFCLENQ)                                                      
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENOFC                                                       
       ++INCLUDE SPGENGRP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENCLTO                                                      
       ++INCLUDE SPGENSDU                                                       
GOLRECD  DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE GEGENOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPSDE01   10/10/11'                                      
         END                                                                    
