*          DATA SET RECNT2A    AT LEVEL 027 AS OF 10/08/15                      
*PHASE T8022AA                                                                  
         TITLE 'T8022A - MAKEGOOD OFFER EDIT'                                   
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT2A (T8022A) --- MAKEGOOD OFFER EDIT                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 08OCT15 KWA BYAPSS AM VALIDATION                                              
* 11DEC13 SKU ADDITIONAL FIX TO TAKEOVER OFFERS TO START AT DA    *             
* 13NOV13 SKU FIX MGX NOT DELETING RIS OFFER PASSIVE KEYS         *             
* 12NOV13 SKU FIX BAD BRANCH BUG                                  *             
* 18SEP13 BOB UPDATE MAKEGOOD PASSIVE POINTERS                    *             
* 28AUG13 SKU SAVE OFF FIRST AIR DATE FOR RIS OFFER LISTING       *             
* 14SEP09 SKU START TAKEOVER CONTRACTS AT OFFER CODE DA INSTEAD   *             
* 07MAY09 SKU START TAKEOVER CONTRACTS AT OFFER CODE BA           *             
* 26FEB08 SKU DON'T USE ADDREC IN RECWRITE TO CHECK FOR DUPE REC  *             
* 11APR07 HQ  FIX CLEANUP BUG                                     *             
* 26OCT06 HQ  ACCEPT M/D/Y INPUT FROM USER TO ACCOMODATE          *             
*             CONTRACT THAT SPAN ACROSS A WHOLE BROADCAST CALENDAR*             
*             YEAR                                                *             
* 13JUN06 SKU ALLOW ORBIT OFFERS                                  *             
* 04OCT04 HQ  MAKE SURE CC NOT EQUAL IS CORRECT                   *             
* 14NOV03 SKU DISALLOW MAKEGOOD TO TARGET CANCELLED BUYS          *             
* 07JUL02 HQ  DEMO CAT AND VALUE INPUT ON MKG SCREEN              *             
* 18APR03 SKU A011 PASSIVE ADD BUG FIX                            *             
* 22AUG02 HQ  CHANGE CALL TO TIMVAL,DAYVAL TO REPFACS(REDAYVAL..) *             
*         HQ  BUG FIX, ALLOW USE TO INPUT DUPLICATE MISS DATE     *             
* 07FEB02 SKU REMOVE 5 MINUTE RULE                                *             
* 15AUG01 SKU DISABLE DATE RANGE FOR PREEMPT                      *             
* 31JAN01 RHV NO MG AGAINST SPORTS BUYS                           *             
*         SKU SKIP SENDING MKGCAN TO AGENCY IF DARE INFO NOT FOUND*             
* 18JAN01 HWO IGNORE AGENCY OFFICE WHEN RETRIEVING X'51' DARE RECORD            
* 24AUG00 SKU CAN NO LONGER ADD MULTIPLE OFFERS TO A MG GROUP     *             
* 04AUG00 SKU ALLOW MGX OF OFFERS WITH MISSING TARGET BUYS        *             
* 01JUN00 SKU FIX PREEMPT BUG                                     *             
* 13JAN00 SKU NEW MAKEGOOD SUPPORT                                *             
* 08JUN99 SKU LIMIT BUY RECORD TO MAX 972 BYTES                   *             
* 16NOV98 BU  S/P TEAM + SWITCH REFERENCES, PASSIVE KEY           *             
* 09JUN98 SKU DIRECT CREDIT PROCESSING                            *             
* 03DEC97 SKU CANNOT HAVE ZERO MISSED SPOTS FOR PREEMPT           *             
* 12NOV97 SKU FIX MAKEGOOD OFFER APPLY BUG                        *             
* 21MAY97 SKU ALLOW B FOR BONUS IN LINE NUMBER                    *             
* 23APR97 SKU SPECIAL FOX VERSION TO RETRIEVE X'51' FROM PETRY    *             
* 27JAN97 SKU TAKEOVER                                            *             
* 09JAN97 SKU MAKEGOOD FOR MAKEGOOD                               *             
* 27DEC96 SKU ALLOW 5 MINS BEFORE NEXT MGX                        *             
* 01NOV96 SKU BOTH REP/STA CAN CHG MG REGARDLESS OF OWNERSHIP     *             
* 17OCT96 SKU SUPPORT BONUSES AND PREEMPTS                        *             
* 03OCT96 SKU SUPPORT LOW POWER STATION                           *             
* 12JUL96 SKU CHECK IF PROFILE 9 SPOTPAK INTERFACE SET            *             
* 02JUL96 SKU DARE MAKEGOOD BUG FIX                               *             
* 01MAY96 SKU IF STATION MGX AND IS DARE, SENT NOTICE TO AGENCY   *             
* 22MAR96 SKU SUPPORT EXPANDED X'21' MG SEND ELEMENT              *             
*             DISABLE ORBIT SUPPORT IF DARE                       *             
* 11MAR96 SKU DON'T CLEAR X'02' ELMTS IN MG HEADER IN GRPCOMNT    *             
* 08MAR96 SKU MGX BUG FIX                                         *             
* 13DEC95 SKU 2K CONTRACT SUPPORT                                 *             
* 16AUG95 SKU ADD MAKEGODD OFFERS SEND ACTIVITY ELEMENT           *             
* 01AUG95 SKU MAKEGOOD OFFERS CAN ONY BE USED IF CONTRACT         *             
*             HAS BEEN CONFIRMED AT LEAST ONCE                    *             
* 18APR95 BU  MULTI MISSED DATES - LEVEL 141 CREATED FROM         *             
*             LEVEL 191 ..                                        *             
* 06MAR95 BU  CHANGE DATE/TIME COMPONENT OF KEY TO                *             
*             TWO-CHARACTER CODE..                                *             
* 16FEB95 BU  PROHIBIT ANYTHING BUT COMMENT CHANGE FROM           *             
*             THE REP SIDE.                                       *             
*******************************************************************             
*                                                                               
* PROGRAMMER BEWARE: THERE ARE VARIABLES ORG'ED ON TO WORK2 !!!                 
*                                                                               
*******************************************************************             
T8022A   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 0,T8022A,R9,R8                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LA    R2,MGOOPGNH         TEST                                         
         LA    R2,CONCACTH                                                      
         LA    R3,504              INVALID ACTION IF GROUP APPLIED              
         CLC   =C'APPLIED',MGOSTAT                                              
         BE    ERROR                                                            
         CLC   =C'MGO',CONCACT                                                  
         BE    BUED0090                                                         
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         LA    R3,576              NO D/A, REC NOT FOUND                        
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    ERROR                                                            
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* FOR DARE MG, ONLY ACTION LEFT IS MGX IF MAKEGOOD OFFER HAS BEEN               
* CANCELLED TO AGENCY                                                           
*                                                                               
         LA    R5,IOAREA                                                        
         USING RMKGREC,R5                                                       
         CLC   =C'MGC',CONCACT     FOR ACTION CHANGE ONLY                       
         BNE   BUED0030                                                         
         LA    R3,582              MG IS CANCELLED, MUST DO MGX                 
         TM    RMKGSFG1,RMGF1MCN   DARE MAKEGOOD WAS CANCELLED                  
         BO    ERROR                                                            
*                                                                               
* FOR DARE MG, OFFERER CANNOT REVISE A MAKEGOOD                                 
* THAT HAS BEEN SENT/RESENT TO THE AGENCY BY THE REP                            
*                                                                               
BUED0030 DS    0H                                                               
         LA    R3,618              MG IS SENT, MUST RECALL                      
         TM    RMKGSFG1,RMGF1MSN+RMGF1MCR+RMGF1MAR                              
         BNZ   ERROR                                                            
*                                                                               
         CLI   TWAACCS,C'$'        ARE I A REP?                                 
         BE    BUED0050                                                         
*                                                                               
* YES, CHECK IF ORDER IS IN-PROGRESS                                            
*                                                                               
         LA    R3,628              WIP ON STATION SIDE                          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    ERROR               ON THE STATION SIDE                          
*                                                                               
*        TM    RMKGSCST,RMKGSCRQ   REP CREATED THIS OFFER?                      
*        BO    BUED0060            YES, PROCEED                                 
*        LA    R3,636              OFFEREE CANNOT CHANGE IF REJ/RECALL          
*        TM    RMKGSCST,RMKGSRCQ+RMKGSRJQ                                       
*        BNZ   ERROR                                                            
         B     BUED0060                                                         
*                                                                               
* CHECK IF ORDER IS IN-PROGRESS                                                 
*                                                                               
BUED0050 DS    0H                  I MUST BE A STATION                          
         LA    R3,629              WIP ON REP SIDE                              
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    ERROR               ON THE REP SIDE                              
*                                                                               
*        TM    RMKGSCST,RMKGSCRQ   STA CREATED THIS OFFER?                      
*        BZ    BUED0060            YES, PROCEED                                 
*        LA    R3,636              OFFEREE CANNOT CHANGE IF REJ/RECALL          
*        TM    RMKGSCST,RMKGSRCQ+RMKGSRJQ                                       
*        BNZ   ERROR                                                            
         DROP  R5                                                               
*                                                                               
* PROCESS FOR ACTION MGX                                                        
*                                                                               
BUED0060 DS    0H                                                               
         CLC   =C'MGX',CONCACT     FOR ACTION DELETE ONLY                       
         BNE   BUED0090                                                         
*                                                                               
         LA    R3,497              ONLY OFFERER CAN DELETE                      
         CLI   TWAACCS,C'$'        ARE I A REP?                                 
         BE    BUED0070                                                         
         CLI   MGOCREA,C'R'        CREATOR REP OR STATION?                      
         BNE   ERROR                                                            
         B     BUED0080                                                         
*                                                                               
BUED0070 DS    0H                                                               
         CLI   MGOCREA,C'S'        CREATOR REP OR STATION?                      
         BNE   ERROR                                                            
*                                                                               
BUED0080 DS    0H                                                               
*        LA    R3,646              MUST WAIT 5 MINUTE SINCE                     
*        BAS   RE,CHKTIME          PREVIOUS ACTION TO AGENCY, IF ANY            
*        BNE   ERROR                                                            
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VMOVEREC,DMCB,IOAREA,RMKGREC                                     
*                                  FIND OLD X'0A' ELEMENT                       
         GOTO1 =A(OLD0AELT),RR=Y                                                
*                                                                               
         GOTO1 =A(DELOFFER),RR=Y   YES. DROP OFFER, WIPE FROM ORIG BUY          
*                                                                               
         LA    R3,RMKGREC                                                       
         BAS   RE,PASSREWR         REWRITE PASSIVE KEYS                         
*                                                                               
         GOTO1 =A(MKGCAN),DMCB,(RC),RR=Y                                        
*                                                                               
         LA    R3,123                                                           
         LA    R2,CONCACTH                                                      
         B     INFEXIT                                                          
*        XC    DUB,DUB                                                          
*        MVC   DUB,=H'123'         INSERT RETURN MESSAGE                        
*        B     EXXMOD                                                           
*                                                                               
* PROCESS FOR ACTION MGC/MGO                                                    
*                                                                               
BUED0090 DS    0H                                                               
         LA    R0,3                                                             
*                                                                               
BUED0100 EQU   *                                                                
         LA    RF,MGOMSTRH                                                      
         MVI   8(RF),C' '          CLEAR 1ST CHAR FLAG (*)                      
         OI    6(RF),X'80'         SET TRANSMIT                                 
         XC    REDISPIT,REDISPIT   CLEAR REDISPLAY FLAG                         
         LA    RF,MGOMST2H-MGOMSTRH(RF)                                         
*                                  BUMP TO NEXT LINE                            
         BCT   R0,BUED0100         GO BACK FOR ALL LINES                        
*                                                                               
         L     RF,ASPULAR          2K VERSION HAS ADDRESS OF SPOOLAR            
         LA    RF,1000(RF)         SET A(1ST AREA)                              
*                                                                               
         ST    RF,MGSTOR#1         SAVE A(1ST AREA)                             
         LA    RF,1000(RF)         SET A(2ND AREA)                              
         ST    RF,MGSTOR#2         SAVE A(2ND AREA)                             
         LA    RF,1000(RF)         SET A(3RD AREA)                              
         ST    RF,MGSTOR#3         SAVE A(3RD AREA)                             
*                                                                               
         LA    RF,2000(RF)         SET A(ORIGINAL BUY AREA)                     
*                                     FOR INSERTING 'MISSING' DATA              
         ST    RF,AORIGBUY         SAVE A(ORIGINAL BUY)                         
         MVC   MGSTOR#0,MGSTOR#1   SET 1ST AVAILABLE AREA                       
         L     RF,MGSTOR#1         CLEAR THE AREAS                              
         XCEFL 0(RF),3000          CLEAR 3X1000 AREAS                           
         MVI   NEXTBUYL,0          CLEAR NEXT BUYLINE #                         
*                                                                               
         MVI   STAT2,0             CLEAR OUT STATUS BYTE                        
         LA    R2,CONCACTH                                                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
*                                  READ CONTRACT VIA D/A                        
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,BACERR                                                        
         TM    RCONCNTL,X'01'      COMPRESSED CONTRACT                          
         BO    ERROR                                                            
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'      BONUS??                                      
         BO    BUED0108            NO ORIGINAL BUY NEEDED                       
         MVC   KEY+28(4),TWAMKGDS  READ ORIGINAL BUY RECORD VIA                 
         DROP  RF                     DISK ADDRESS                              
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* CANNOT HAVE MAKEGOOD AGAINST SPORTS BUY                                       
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    BUED0101            NO - OK                                      
         LA    R3,880                                                           
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
* CANNOT TARGET CANCELLED BUY                                                   
*                                                                               
BUED0101 DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BNE   BUED0103            NO - OK                                      
         LA    R3,983                                                           
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
BUED0103 DS    0H                                                               
*                                                                               
* CANNOT HAVE MAKEGOOD AGAINST CREDIT                                           
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0105                                                         
         USING RBUYCMEL,R6                                                      
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   BUED0105                                                         
         LA    R3,650                                                           
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
BUED0105 DS    0H                                                               
         L     R2,AORIGBUY                                                      
         PRINT GEN                                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,(R2)                                        
         PRINT NOGEN                                                            
*                                                                               
BUED0108 DS    0H                                                               
         XC    OFFRDISP,OFFRDISP   SET TO DISP(1ST OFFER LINE)                  
*                                                                               
CHOYSERR EQU   464                 ERROR IN CHOICE FIELD                        
ALLCHOYS EQU   465                 OPTIONS CONFLICT                             
*                                                                               
         MVI   CHOYSCTR,0          CLEAR WORKFIELD                              
         LA    R3,1                MISSING INPUT                                
         LA    R2,MGOMOPTH         A(OPTION FIELD)                              
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    ERROR               NO  -                                        
*                                                                               
* ACTION IS MGO/ADD                                                             
*                                                                               
         LA    R3,ALLCHOYS                                                      
         CLC   =C'MGO',CONCACT     FOR ADDING NEW OFFERS                        
         BNE   BUED0110            SPECIFY IF ALL OR CHOICE                     
         CLI   MGOMOPT,C'A'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         CLI   MGOMOPT,C'C'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         B     ERROR                                                            
*                                                                               
* ACTION IS MGC/CHANGE                                                          
*                                                                               
BUED0110 DS    0H                                                               
         CLI   MGOCREA,C'S'        CREATOR REP OR STATION?                      
         BE    BUED0120                                                         
*                                                                               
* CREATOR WAS REP, CHECK IF USER STATION OR REP                                 
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    BUED0140                                                         
         B     BUED0130                                                         
*                                                                               
* CREATOR WAS STATION, CHECK IF USER STATION OR REP                             
*                                                                               
BUED0120 DS    0H                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   BUED0140                                                         
*                                                                               
* CURRENT USER IS THE CREATOR OF THIS MAKEGOOD OFFER                            
* SO CHECK FOR (A)LL OR (C)HOICE ONLY                                           
*                                                                               
BUED0130 DS    0H                                                               
         CLI   MGOMOPT,C'A'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         CLI   MGOMOPT,C'C'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         B     ERROR                                                            
*                                                                               
* CURRENT USER DID NOT CREATE THIS MAKEGOOD OFFER                               
* SO IF (A)LL, DO NOTHING, ELSE IF (C)HOICE, SPECIFY MKG LINE DESIRED           
*                                                                               
BUED0140 DS    0H                                                               
         CLI   MGOMOPT,C'A'                                                     
         BE    BUED0170                                                         
         CLI   MGOMOPT,C'1'                                                     
         BL    ERROR                                                            
         CLI   MGOMOPT,C'3'                                                     
         BH    ERROR                                                            
                                                                                
         LA    R3,CHOYSERR                                                      
         ZIC   RF,8(R2)                                                         
         SLL   RF,28               DROP ZONE BITS                               
         SRL   RF,28               MOVE IT BACK                                 
         STC   RF,CHOYSCTR         SAVE                                         
*                                                                               
         SR    RF,RF               CLEAR COUNTER                                
         LA    R6,MGOMDYSH         FIRST DAY FIELD                              
         LA    R0,3                                                             
BUED0150 EQU   *                                                                
         OC    8(L'MGOMDYS,R6),8(R6)                                            
         BZ    BUED0160            ANYTHING IN FIELD?                           
         CLC   =C'**',8(R6)        NO  - REQUEST TO DELETE LINE?                
         BE    BUED0160            YES - DON'T COUNT IT                         
         LA    RF,1(RF)            NO  - COUNT LINE                             
BUED0160 EQU   *                                                                
         LA    R6,MGOMDY2H-MGOMDYSH(R6)                                         
*                                  BUMP TO NEXT LINE                            
         BCT   R0,BUED0150         GO BACK AND CHECK NEXT LINE                  
         ZIC   RE,CHOYSCTR         COMPARE AGAINST CHOICE                       
         CR    RE,RF               CHOICE VS LINES ON SCREEN                    
         BH    ERROR               CHOICE EXCEEDS DISPLAYED LINES               
         GOTO1 =A(MRKCHOYS),DMCB,(RC),RR=Y                                      
         MVC   DUB(2),=H'137'      SELECTED                                     
*                                                                               
         XCEFL RMKGREC,1000        CLEAR THE MAKEGOOD RECORD OUT                
*                                                                               
BUED0170 EQU   *                                                                
         ZIC   RF,LINECNTR         INCREMENT LINE COUNTER                       
         LA    RF,1(RF)                                                         
         STC   RF,LINECNTR                                                      
*                                                                               
* VALIDATE PLEASE ADVISE OR ASSUME OK SELECTIONS                                
*                                                                               
         LA    R3,495              SELECTION ERROR                              
         LA    R2,MGOMPADH                                                      
         CLI   MGOMPAD,C'Y'                                                     
         BE    BUED0180                                                         
         CLI   MGOMAOK,C'Y'        ELSE ASSUME OK                               
         BNE   ERROR                                                            
         B     BUED0190                                                         
*                                                                               
BUED0180 DS    0H                                                               
         CLI   MGOMAOK,C'Y'                                                     
         BE    ERROR                                                            
*                                                                               
* VALIDATE EXPIRATION DATE                                                      
*                                                                               
BUED0190 DS    0H                                                               
         LA    R2,MGOEXPDH                                                      
         CLI   5(R2),0                                                          
         BE    BUED0200                                                         
         LA    R3,496                                                           
         GOTO1 DATVAL,DMCB,MGOEXPD,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
*                                                                               
BUED0200 DS    0H                                                               
*                                  IF FIRST LINE HAS '**', THIS                 
*                                     TEST WILL SKIP IT                         
         LA    RF,MGOMDYSH         A(1ST LINE OF MG OFFER)                      
         A     RF,OFFRDISP         + DISP TO NEXT SCREEN LINE                   
         CLC   =C'**',8(RF)        FIRST TWO CHARS = '**'?                      
         BE    ENED0440                                                         
*                                                                               
         CLC   CONADV(3),=C'GEN'   DON'T ALLOW BUYS WHEN ADV=GEN &              
         BNE   BUED0210                                                         
         CLC   CONCAT,=C'ZZ'       CATEGORY=ZZ (I.E. GENERAL AVAILS)            
         BNE   BUED0210                                                         
         LA    R3,192                                                           
         B     ERROR                                                            
*                                                                               
*              MUST BE NEW BUY                                                  
*                                                                               
BUED0210 EQU   *                                                                
         MVI   ECFORMAT,C' '       CLEAR EC FORMAT                              
         CLI   RCONKSTA+4,C' '     TV STATION?                                  
         BE    BUED0230            YES                                          
         CLI   RCONKSTA+4,C'L'     TV STATION?                                  
         BE    BUED0230            YES                                          
         SPACE                                                                  
BUED0220 EQU   *                                                                
*                                  YES                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0250            NO EPL ELEMENT                               
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN' NO BUYS ALLOWED ON CANCELLED                 
         BNE   BUED0250            CONTRACTS                                    
         LA    R3,190                                                           
         B     ERROR                                                            
         DROP  R6                                                               
         SPACE 1                                                                
BUED0230 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0240            NO SAR ELEMENT                               
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         OC    RSARRDT,RSARRDT     CONTRACT RESOLVED?                           
         BNZ   BUED0240            YES - FIELD CONTAINS DATE                    
         SPACE 1                                                                
         MVC   RSARRDT,TODAY       NO  - RESOLVE CONTRACT                       
****>>>  GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     BUED0240                                                         
         DROP  R6                                                               
         SPACE 1                                                                
BUED0240 EQU   *                                                                
         CLI   ECFORMAT,C' '       FIRST TIME?                                  
         BNE   BUED0250            NO  - EC FORMAT ALREADY FOUND                
         BAS   RE,GETSTAT          GET STATION FOR EC FORMAT                    
         MVC   ECFORMAT,RSTATRAF   SAVE EC FORMAT                               
*                                                                               
BUED0250 EQU   *                                                                
*                                                                               
*                                                                               
         MVI   RMKGLEN+1,77        ELEM LENGTH (34 + 43)                        
         MVC   RMKGCODE(2),=X'012B'     BUY DESC ELEM CODE + LEN                
*                                                                               
*                                  SET OFF PREVALID BITS FOR                    
*                                     ADD (IN CASE OF ERROR)                    
         CLI   LINECNTR,1          1ST PASS?                                    
         BNE   MISSEDED            NO  - SKIP NEXT STUFF                        
         LA    R2,MGODAYSH                                                      
         LA    R3,MGOLAST                                                       
         SR    RE,RE                                                            
*                                                                               
         NI    4(R2),X'DF'                                                      
         IC    RE,0(R2)            LEN                                          
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    *-14                                                             
*                                                                               
* IF PREEMPT, ONLY LINE ONE IS USED                                             
*                                                                               
         CLC   =C'PREEMPT',MGOMDYS                                              
         BNE   MISSEDED                                                         
         LA    R3,632                                                           
         LA    R2,MGOMDY2H                                                      
         CLI   MGOMDY2H+5,0                                                     
         BNE   ERROR                                                            
         LA    R2,MGOMDY3H                                                      
         CLI   MGOMDY3H+5,0                                                     
         BNE   ERROR                                                            
*                                                                               
***>>    FOUT  CONBNUMH,MYSPACES,8                                              
***>>    NI    CONBNUMH+4,X'DF'    TURN OFF VALID BITS IN CASE OF ERROR         
*                                                                               
*                                                                               
         TITLE 'REPPAK MISSED DATE/# SPOTS EDIT'                                
*        EDIT MISSED DATE/# SPOTS FIELD -                                       
*        THIS IS FOR VALIDATION ONLY.                                           
*        REDONE IN THE COMMENT EDIT SECTION, SO THAT                            
*        AN ELEMENT MAY BE INSERTED INTO THE RECORD.                            
*                                                                               
MISSEDED EQU   *                                                                
         CLC   =C'PREEMPT',MGOMDYS                                              
         BE    CMISEDED            DIFFERENT VALIDATION FOR PREEMPT             
*                                                                               
MSED0010 DS    0H                                                               
*                                                                               
*&&DO                                                                           
         LA    R3,MGMISDER                                                      
         LA    R2,MGOMMSDH         SET A(MISSED DATE/SPOTS FIELD HDR)           
         LA    RE,MGOMMSD          SET A(MISSED DATE/SPOTS FIELD)               
*                                                                               
         CLC   =C'BONUS',8(R2)                                                  
         BNE   MSED0020                                                         
         CLI   MGOMOPT,C'A'                                                     
         BE    DAED0020                                                         
         LA    R2,MGOMOPTH                                                      
         LA    R3,620              MUST OFFER ALL                               
         B     ERROR                                                            
*                                                                               
MSED0020 EQU   *                                                                
         LR    R4,RE               SAVE POINTER WITHIN STRING                   
         GOTO1 DATVAL,DMCB,(1,(R4)),DUB                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         LR    RE,R4               RESET POINTER WITHIN STRING                  
*                                  FORMAT IS JAN15/#                            
*                                                                               
*                                  GET NUMBER OF MISSED SPOTS                   
         A     RE,DMCB             ADD LENGTH OF DATE FIELD                     
         CLI   0(RE),C','          FIELD SEPARATOR?                             
         BNE   MSED0030            NO                                           
         LA    RE,1(RE)            YES - SKIP AND TEST NEXT FIELD               
         B     MSED0020            GO BACK AND TEST                             
MSED0030 EQU   *                                                                
         CLI   0(RE),C' '          LAST FIELD?                                  
         BE    DAED0020            YES - FINISHED                               
         CLI   0(RE),X'00'         LAST FIELD?                                  
         BE    DAED0020            YES - FINISHED                               
         CLI   0(RE),C'('          # SPOTS FOLLOWS?                             
         BNE   ERROR               NO  - UNRECOGNIZED CHARACTER                 
         LA    RE,1(RE)            YES - EDIT NUMBER OF SPOTS                   
         LR    R4,RE                                                            
         SR    R1,R1                                                            
*                                                                               
MSED0040 EQU   *                                                                
         CLI   0(RE),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'F9'                                                      
         BH    ERROR                                                            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         CLI   0(RE),C')'          # SPOTS TERMINATOR?                          
         BNE   MSED0040            NO  - GO BACK FOR NEXT                       
MSED0060 EQU   *                                                                
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
MSED0080 EQU   *                                                                
         PACK  DUB,0(0,R4)         MISSED NUMBER OF SPOTS                       
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CLI   0(RE),C' '          NO MORE DATA?                                
         BE    DAED0020            FINISHED                                     
         CLI   0(RE),X'00'         NO MORE DATA?                                
         BE    DAED0020            FINISHED                                     
         CLI   0(RE),C','          ANOTHER FIELD FOLLOWS?                       
         BNE   ERROR               NO  - UNRECOGNIZED CHARACTER                 
         LA    RE,1(RE)            YES - SKIP THE ','                           
         B     MSED0020            GO BACK AND VALIDATE NEXT SET                
*&&                                                                             
*                                                                               
*  NOTE:  IF NOT FIRST OFFER LINE, # MISSED SPOTS MUST BE ZERO!                 
*    ELSE WE WILL TAKE THE SPOT(S) OUT OF THE ORIGINAL LINE FOR                 
*    EACH OF THE OFFERS, WHICH IS INCORRECT!                                    
*                                                                               
         EJECT                                                                  
*                                                                               
* SPECIAL FOR PREEMPT MAKEGOOD OFFERS                                           
* ALLOW RANGE DATE FORMATS: IE JAN01-DEC31(1), JAN1-2W(2), ETC                  
*                                                                               
* DISABLE DATE RANGE FOR PREEMPT UNTIL RECNT3A CAN HANDLE IT (8/15/01)          
* SKUI                                                                          
*                                                                               
CMISEDED DS    0H                                                               
*                                                                               
         EJECT                                                                  
*              EDIT DAYS AND TIMES                                              
DAED0020 EQU   *                                                                
         LA    R2,MGOMDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  A(NTH LINE, 1ST FIELD)                       
*                                 DELETE ALL DAY-TIME ELEMENTS                  
         GOTO1 VDELELEM,DMCB,(2,RMKGREC)                                        
*                                                                               
         NI    RMKGRTS,X'FF'-X'10'                                              
         CLC   =C'PREEMPT',8(R2)   NO INPUT FOR PREEMPT                         
         BNE   DAED0030                                                         
         LA    R3,632                                                           
         OC    OFFRDISP,OFFRDISP                                                
         BNZ   ERROR                                                            
*                                                                               
         LA    R3,625              CANNOT DO BOTH BONUS AND PREEMPT             
         CLC   =C'BONUS',MGOMMSD                                                
         BE    ERROR                                                            
*                                                                               
         LA    R3,622              NO INPUT IF PREEMPT                          
         LA    R2,MGOMTMSH                                                      
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,MGOMDTSH                                                      
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,MGOMDTSH                                                      
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,MGOMSPTH                                                      
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
*                                                                               
         GOTO1 VDELELEM,DMCB,(3,RMKGREC)                                        
*                                                                               
         OI    RMKGRTS,X'10'       SET MKGD OFFER AS PREEMPT                    
*                                                                               
         LA    R2,MGOMDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         B     BUYED2                                                           
*                                                                               
DAED0030 EQU   *                                                                
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*                                  PREPARE TO EDIT STRING OF                    
*                                     DAY-TIME FIELDS IN PARALLEL               
         LA    R4,MGOMDYS-1                                                     
         A     R4,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R5,MGOMDYSH                                                      
         A     R5,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R6,MGOMTMS-1                                                     
         A     R6,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R7,MGOMTMSH                                                      
         A     R7,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
         GOTO1 CHKBLANK,DMCB,(RC)                                               
*                                  CHECK DAY/TIMES FOR BLANK, AND               
*                                     PRIME FIELDS IF NECESSARY.                
         CLI   DAYTMFLG,0          BOTH FIELDS ENTERED?                         
         BE    DAED0120            YES - EDIT BOTH IN SYNCH                     
         CLI   DAYTMFLG,2          SEED X'02'S SERIALLY? (NO TIMES)             
         BE    DAED0060            YES                                          
         CLI   DAYTMFLG,3          SEED X'02'S SERIALLY? (NO DAYS)              
         BE    DAED0060            YES                                          
         CLI   DAYTMFLG,1          NEITHER FIELD ENTERED?                       
         BNE   DAED0040            NO  - CONTINUE FLAG CHECK                    
         BAS   RE,COPYX02S         YES - COPY ORIG X'02S' INTO NEW              
         L     RF,AORIGBUY         SET A(ORIGINAL RECORD)                       
         MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  INSERT START-END DAY FOR ALL X'02'           
*                                     ELTS FROM ORIGINAL RECORD                 
         B     DATEDIT             SKIP FURTHER CHECKS ON FIELDS                
DAED0040 EQU   *                   ***>>>ONLY ERRORS REMAIN<<<***               
         LA    R2,MGOMDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R3,DAYERR                                                        
         CLI   DAYTMFLG,9          DAY  FIELD IN ERROR?                         
         BE    ERROR               YES                                          
         LA    R2,MGOMTMSH         NO  - TIME FIELD IN ERROR                    
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
DAED0060 EQU   *                                                                
         CLI   DAYTMFLG,3          SEEDING OF X'02' NEEDED?                     
         BNE   DAED0080            NO  -                                        
*                                  YES - NO DAYS ENTERED                        
         L     RF,AORIGBUY         SET A(ORIGINAL RECORD)                       
         MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  INSERT START-END DAY FOR ALL X'02'           
*                                     ELTS FROM ORIGINAL RECORD                 
         B     DAED0100                                                         
DAED0080 EQU   *                                                                
         CLI   DAYTMFLG,2          SEEDING OF X'02' NEEDED?                     
         BNE   DAED0120            NO                                           
*                                  YES - NO TIMES ENTERED                       
DAED0100 EQU   *                                                                
         L     RF,ADYTMELT         SET A(X'02' ELEMENT)                         
         MVC   WORK2(9),0(RF)      MOVE X'02' ELT FROM ORIGINAL                 
*                                     RECORD TO WORK2                           
         LA    RF,9(RF)            BUMP TO NEXT ELEMENT                         
         ST    RF,ADYTMELT         SAVE ADDRESS                                 
DAED0120 EQU   *                                                                
         LA    R2,MGOMDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    DAED0200            NO  - DON'T CHECK DAY FIELD                  
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         CLI   DAYTMFLG,2          SEEDING OF X'02' DONE? (NO TIMES)            
         BE    DAED0140            YES - DON'T CLEAR THE FIELD                  
         CLI   DAYTMFLG,3          NO  - SEEDING OF X'02'? (NO DAYS)            
         BE    DAED0140            YES - DON'T CLEAR THE FIELD                  
         XC    WORK2+2(6),WORK2+2                                               
DAED0140 EQU   *                                                                
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAED0160                                                         
*                                  NO DAY LENGTH                                
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                  GET START/END DAYS FOR                       
*                                     ALL X'02' ELEMENTS                        
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RMKGSTED                                                      
*                                                                               
         B     DATEDIT             EDIT NEXT FIELD                              
DAED0160 EQU   *                                                                
         MVC   DMCB2(4),DMCB+8     DAY FIELD ADDR + LEN                         
*                                                                               
*                                  EDIT DAY FIELD                               
*        GOTO1 =V(DAYVAL),DMCB2,,WORK2+3,WORK2+2,RR=YES                         
         GOTOX (RFDAYVAL,VREPFACS),DMCB2,,WORK2+3,WORK2+2                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*                                 GET FIRST START/LAST END DAY                  
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    DAED0180                                                         
*                                  MORE THAN 7 DAYS COVERED?                    
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR               MORE THAN 7 DAYS                             
DAED0180 EQU   *                                                                
         CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
DAED0200 EQU   *                                                                
*                                  EDIT TIME FIELD                              
         LA    R2,MGOMTMSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    DAED0280            NO  - DON'T CHECK THE FIELD                  
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAED0220                                                         
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'VARY'                                                 
         B     DAED0280                                                         
DAED0220 EQU   *                                                                
         EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAED0240            OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'NONE'                                                 
         B     DAED0280                                                         
DAED0240 EQU   *                                                                
         MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
*        GOTO1 =V(RETIMVAL),DMCB,,WORK2+4,RR=YES  EDIT TIME                     
         GOTOX (RFTIMVAL,VREPFACS),DMCB,,WORK2+4                                
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         CLC   =C'CC',WORK2+6      CC NOT SUPPORTED IF DARE                     
         BNE   DAED0250                                                         
*                                                                               
         LA    R6,RCONREC          FOR DARE ORDERS, ORBITS ARE NOT              
         MVI   ELCODE,X'1D'        SUPPORTED                                    
         BAS   RE,GETEL                                                         
         BNE   DAED0250                                                         
         USING RCONDREL,R6                                                      
         OC    RCONDRLK,RCONDRLK                                                
         BZ    DAED0250                                                         
         LA    R3,623                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
DAED0250 EQU   *                                                                
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAED0280            NO                                           
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         B     DAED0280            BYPASS AM VALIDATION                         
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAED0260            6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAED0280            NO                                           
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAED0280            NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
DAED0260 EQU   *                                                                
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAED0280            NO                                           
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAED0280            NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                  ADD DAY-TIME ELEMENT TO MKGREC               
DAED0280 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAED0300            PARTICULARLY, IF BIAS                        
         DROP  RF                                                               
                                                                                
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAED0300 EQU   *                                                                
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         CLI   DAYTMFLG,0          BOTH FIELDS ENTERED?                         
         BE    DAED0120            YES - DON'T BOTHER WITH SEEDING              
         ZIC   RF,DAYTMCTR         NO  - CHECK COUNTER FOR ANOTHER              
*                                     SET OF FIELDS                             
         BCTR  RF,0                DECREMENT BY 1                               
         STC   RF,DAYTMCTR         SAVE COUNTER BACK                            
         LTR   RF,RF               ANYTHING LEFT?                               
         BNZ   DAED0060            NO  - EDIT NEXT DAY/TM FLD COMBO             
*                                  GET START/END DAYS FOR                       
*                                     ALL X'02' ELEMENTS                        
         CLI   DAYTMFLG,3          DAYS NOT ENTERED?                            
         BE    DAED0320            YES - SET RMKGSTED FROM ORIG REC             
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RMKGSTED                                                      
         B     DATEDIT                                                          
*                                                                               
DAED0320 EQU   *                                                                
         L     RF,AORIGBUY                                                      
         MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  USE ORIG FIELD TO SET                        
         B     DATEDIT             GO EDIT THE DATE                             
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* CHECK TO MAKE SURE 5 MINUTES HAVE ELAPSED BEFORE NEXT ACTION                  
* THIS WOULD ENSURE THAT EDICT HAD A CHANCE TO PROCESS OUTSTANDING              
* ENTRIES                                                                       
***********************************************************************         
CHKTIME  NTR1                                                                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKTMYES                                                         
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
CHKT10   DS    0H                                                               
         USING RMKGATEM,R6                                                      
         TM    RMKGATAT,X'20'+X'10'                                             
         BNZ   CHKT20              SKIP CHECK FOR APPROVAL/REJECTION            
         MVC   WORK(5),RMKGATDT                                                 
         DROP  R6                                                               
*                                                                               
CHKT20   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    CHKT10                                                           
*                                                                               
         OC    WORK,WORK                                                        
         BZ    CHKTMYES                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK+5)                                     
*                                                                               
         CLC   WORK(2),WORK+5      WAS LAST ACTION TODAY?                       
         BNE   CHKTMYES                                                         
*                                                                               
         THMS  DDSTIME=YES         YES, NEED TO CHECK TIME                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         SP    DUB(4),=P'500'      MUST BE AT LEAST 5 MINUTES APART             
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK+7                                                      
*                                                                               
* NEED TO ADJUST FOR MINUTES. IF ORIGINAL TIME IS 15:03:14, AND WE PACK         
* SUBTRACT 5 MINUTES, THE RESULT IS 15:98:14. THIS CAN BE ADJUSTED BY           
* SUBTRACTING AN ADDITIONAL 40 MINUTES = 15:58:14                               
*                                                                               
         CLI   WORK+8,X'60'                                                     
         BL    CHKT30                                                           
         SP    DUB(4),=P'4000'                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK+7                                                      
*                                                                               
CHKT30   DS    0H                                                               
         CLC   WORK+2(3),WORK+7                                                 
         BNL   CHKTMNO                                                          
*                                                                               
CHKTMYES SR    RC,RC                                                            
CHKTMNO  LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*&&                                                                             
*                                                                               
*                                                                               
*   CHKBLANK:  IF EITHER DAY OR TIME FIELD IS EMPTY, DEFAULT IS                 
*        FROM TARGET BUY, FOR 1ST LINE, OR PRECEDING OFFER LINE.                
*        MUST CHECK TO ENSURE THAT SAME NUMBER OF DAYS AND TIMES                
*        ARE PRESENT.                                                           
*                                                                               
CHKBLANK NTR1                                                                   
         MVI   DAYTMCTR,0          CLEAR COUNTER                                
         MVI   DAYTMFLG,0          CLEAR FLAG                                   
         LA    R2,MGOMDYSH         DAY FIELD                                    
         A     R2,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         LA    R3,MGOMTMSH         DAY FIELD                                    
         A     R3,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         CLI   5(R2),0             ANY DATA IN DAY FIELD?                       
         BE    CHKB0200            NO  - CHECK TIMES                            
         CLI   5(R3),0             YES - ANY DATA IN TIME FIELD?                
         BE    CHKB0400            NO  -                                        
         B     CHKB0900            YES - DATA IN BOTH FIELDS                    
*                                     NORMAL VALIDATION                         
CHKB0200 EQU   *                   NO DATA IN DAY FIELD                         
         CLI   5(R3),0             ANY DATA IN TIME FIELD?                      
         BNE   CHKB0240            YES - TIMES ENTERED                          
         MVI   DAYTMFLG,1          NO  - SET FLAG TO 1 TO SIGNAL                
*                                     THAT X'02' ELTS MUST BE COPIED            
         B     CHKB0900            EXIT                                         
CHKB0240 EQU   *                                                                
         BAS   RE,COUNT02S         COUNT X'02'S IN ORIG REC                     
         GOTO1 SCANFLD,DMCB,(R3)   COUNT NUMBER ENTRIES IN TIME                 
         MVI   DAYTMFLG,9          SET 'ERROR IN TIME FIELD' FLAG               
         CLC   WORK(1),DAYTMCTR    # X'02'S = ENTRIES IN TIME?                  
         BNE   CHKB0900            NO  - ERROR                                  
         MVI   DAYTMFLG,3          YES - MUST SEED X'02'S FROM                  
*                                     ORIGINAL ORDER                            
         B     CHKB0900            EXIT                                         
CHKB0400 EQU   *                                                                
         BAS   RE,COUNT02S         COUNT X'02'S IN ORIG REC                     
         GOTO1 SCANFLD,DMCB,(R2)   COUNT NUMBER ENTRIES IN DAY                  
         MVI   DAYTMFLG,8          SET 'ERROR IN DAY  FIELD' FLAG               
         CLC   WORK(1),DAYTMCTR    # X'02'S = ENTRIES IN DAY?                   
         BNE   CHKB0900            NO  - ERROR                                  
         MVI   DAYTMFLG,2          YES - MUST SEED X'02'S FROM                  
*                                     ORIGINAL ORDER                            
         B     CHKB0900            EXIT                                         
CHKB0900 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
COUNT02S NTR1                                                                   
         XC    ADYTMELT,ADYTMELT   CLEAR A(1ST DAY/TIME ELEMENT)                
         L     R2,AORIGBUY         A(ORIGINAL/PRECEDING RECORD)                 
         LA    R2,34(R2)           SET A(X'01' DESCRIPTOR ELEMENT)              
         SR    RF,RF               CLEAR COUNTER                                
CONS0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CONS0080            YES - FINISHED                               
         CLI   0(R2),X'02'         DAY/TIME ELEMENT?                            
         BNE   CONS0040            NO                                           
         OC    ADYTMELT,ADYTMELT   1ST DAY/TIME ELEMENT?                        
         BNZ   CONS0030            NO                                           
         ST    R2,ADYTMELT         YES - SAVE ITS ADDR                          
CONS0030 EQU   *                                                                
         LA    RF,1(RF)            BUMP COUNTER                                 
CONS0040 EQU   *                                                                
         ZIC   RE,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RE                                                            
         B     CONS0020            GO BACK FOR NEXT                             
CONS0080 EQU   *                                                                
         STC   RF,DAYTMCTR         SAVE COUNT OF X'02' ELTS                     
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   SCANFLD:   P1 CONTAINS A(SCREEN FIELD HEADER).  A COUNT                     
*        MUST BE MADE TO DETERMINE WHETHER SAME NUMBER OF                       
*        FIELDS ARE BEING ENTERED AS EXIST WITHIN THE RECORD FROM               
*        WHICH X'02' ELEMENTS ARE BEING SEEDED.                                 
*        FIELDS ARE SEPARATED BY '*'                                            
*                                                                               
SCANFLD  NTR1                                                                   
         L     R2,0(R1)            SET A(FIELD HEADER)                          
         ZIC   R3,5(R2)            SET L(INPUT)                                 
         LA    RF,1                INITIALIZE CNTR: ALWAYS 1 ENTRY              
         LA    R2,8(R2)            SET A(DATA WITHIN FIELD)                     
SCFL0020 EQU   *                                                                
         CLI   0(R2),C'*'          IS FIELD A SEPARATOR?                        
         BNE   SCFL0040            NO                                           
         LA    RF,1(RF)            YES - COUNT IT                               
SCFL0040 EQU   *                                                                
         LA    R2,1(R2)            BUMP TO NEXT FIELD                           
         BCT   R3,SCFL0020         GO BACK AND CHECK NEXT FIELD                 
         STC   RF,WORK             PASS BACK RESULT                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   COPYX02S:  NEITHER DAY NOR TIME INFORMATION WAS ENTERED FOR                 
*        THIS MAKEGOOD BUYLINE.  THE X'02'S FROM THE ORIGINAL                   
*        BUY LINE MUST BE COPIED OVER TO THE NEW OFFER LINE.                    
*                                                                               
COPYX02S NTR1                                                                   
         L     R2,AORIGBUY         SET A(ORIGINAL BUYLINE)                      
         LA    R2,34(R2)           SET TO DESCRIPTOR ELEMENT                    
CPYX0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CPYX0080            YES - FINISHED                               
         CLI   0(R2),X'02'         DAY/TIME ELEMENT?                            
         BNE   CPYX0040            NO                                           
         GOTO1 VADDELEM,DMCB,RMKGREC,(R2)                                       
*                                  YES - ADD ELT FROM ORIGINAL REC              
CPYX0040 EQU   *                                                                
         ZIC   RF,1(R2)                                                         
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     CPYX0020            GO BACK FOR NEXT                             
CPYX0080 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
*   THIS ELEMENT WILL BE ADDED HERE, BUT THE CONTRACT RECORD WILL               
*     NOT BE UPDATED.  UPON FINALLY ADDING THE RECORDS, THE                     
*     CROSS-DAY VALIDATION WILL HAVE TO BE ACCOMPLISHED.                        
* ***   SERIOUS NOTE   ***                                                      
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   VCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         LA    R2,MGODAYSH                                                      
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
                                                                                
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         LA    R2,MGODAYSH                                                      
         B     ERROR                                                            
                                                                                
VCROSS20 DS    0H                                                               
         MVC   RCONCCCD,WORK       UPDATE NEW CROSS DAY                         
****>>>  OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     VCROSSX                                                          
         DROP  R6                                                               
                                                                                
VCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
****>>>  OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
VCROSSX  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
         TITLE 'REPPAK EFFECTIVE DATES EDIT'                                    
*                                  VALIDATE DATES                               
DATEDIT  EQU   *                                                                
         XC    MGTOTSPT,MGTOTSPT   CLEAR TOTAL SPOTS FOR LINE                   
         XC    MGTOTWKS,MGTOTWKS   CLEAR TOTAL WEEKS FOR LINE                   
         LA    R2,MGOMDTSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
*                                                                               
*                                  DELETE ALL PREV DATE ELTS (CHANGE)           
         GOTO1 VDELELEM,DMCB,(3,RMKGREC)                                        
*                                  SET UP 2 DATE I/P FLDS IN WORK3              
*                                     PRETEND 1 FIELD                           
*                                                                               
         MVC   WORK3(L'MGOMDTS+8),0(R2)                                         
         ZIC   RE,5(R2)            INSERT LENGTH OF FIELD                       
*                                  LEN OF DATE FIELD                            
         LA    R4,WORK3+8(RE)      END OF DATE FIELD                            
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*                                                                               
*                                  SET EARLIEST START DATE/LATEST               
*                                     END DATE IN DAY ELTS                      
         SR    R5,R5                                                            
         ZIC   R4,RMKGSTED                                                      
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         STM   R4,R5,DMCB+16                                                    
*                                                                               
         XC    WORK+24(6),WORK+24                                               
         LA    R7,WORK3+7                                                       
         ST    R7,DMCB+12                                                       
DATD0040 EQU   *                   EDIT START DATE                              
         MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 SCAN,DMCB,,WORK3    SCAN FOR NEXT DATE FIELD                     
*                                                                               
         CLI   DMCB,0              ANY DATE                                     
         BNE   DATD0060                                                         
*                                  NO DATE                                      
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    ERROR                                                            
         B     BUYED2                                                           
DATD0060 EQU   *                                                                
         L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      CONTRACT START INDICATOR                     
         BNE   DATD0080                                                         
*                                                                               
*                                  GET DATE IN FIRST WEEK OF CONTRACT           
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),MYSPACES    VALID CONTRACT START DATE?                   
         BNE   *+6                                                              
         DC    H'0'                CONTRACT ERROR                               
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4)                                     
*                                  GET FIRST DATE                               
         LA    R5,2(R5)            NEXT FIELD                                   
*                                                                               
         B     DATD0220                                                         
DATD0080 EQU   *                                                                
*                                                                               
*                                  EDIT START DATE                              
*                                                                               
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
         MVC   WORK+12(2),WORK     CONTRACT START YEAR                          
         CLC   WORK+14(4),WORK+2   BUY MMDD V CONTRACT MMDD                     
         BNL   DATD0120                                                         
DATD0100 EQU   *                   BUY MMDD LOW                                 
         CLC   WORK(2),WORK+6      CONTRACT START AND END YEARS SAME?           
         BE    ERROR                                                            
         MVC   WORK+12(2),WORK+6   USE CONTRACT END YEAR                        
DATD0120 EQU   *                                                                
         GOTO1 GETDAY,DMCB,WORK+12,FULL                                         
*                                  VALIDATE START DATE                          
*                                                                               
         CLC   FULL(3),MYSPACES                                                 
         BE    ERROR                                                            
         LA    R3,SDYERR                                                        
         ZIC   R4,DMCB             START DAY                                    
         C     R4,DMCB+16          SAME AS 1ST DAY?                             
         BE    DATD0220                                                         
         CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR?                   
         BNE   DATD0100            FOR CONTRACTS MORE THAN 1 CALENDAR           
*                                     YEAR                                      
         B     ERROR                                                            
*                                                                               
DATD0140 EQU   *                   GET CORRECT START DAY                        
         L     R7,DMCB+16          DAY FIELD DAY NO.                            
*                                  YEAR - BUY DATE COULD BE SECOND YEAR         
*                                  GET CONTRACT START DAY  --                   
*                                     CHECK OUT-OF-WEEK DATES                   
*                                                                               
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB             CONTRACT START DAY                           
         LR    R3,R7                                                            
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
         CR    R3,R6               DAY VS CONTRACT START DAY                    
         BL    DATD0180                                                         
         CR    R4,R6               DATE DAY VS CONTRACT START DAY               
         BNL   DATD0200                                                         
         SH    R7,=H'7'            PREVIOUS WEEK                                
         B     DATD0200                                                         
DATD0180 EQU   *                                                                
         CR    R4,R6               DATE DAY VS CONTRACT START DAY               
         BL    DATD0200                                                         
         AH    R7,=H'7'            NEXT WEEK                                    
DATD0200 EQU   *                   GET PROPER DATE IN WEEK                      
         GOTO1 ADDAY,(R1),WORK+12,DUB,(R7)                                      
         MVC   WORK+12(6),DUB                                                   
         CLC   WORK+12(6),WORK     DATE VS CONTRACT START DATE                  
         BNL   *+12                                                             
         LA    R3,SDYERR                                                        
         B     ERROR               JUST IN CASE                                 
DATD0220 EQU   *                                                                
         XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START VS CONTRACT END                    
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
         CLC   WORK+12(6),WORK+24  BUY STRT DATE VS LAST ELT END DATE           
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
*              EDIT END DATE                                                    
*              END DATE                                                         
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATD0240                                                         
*                                  GET END DATE FROM CONTRACT END DATE          
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB             CONTRACT END DAY                             
*                                                                               
         S     R4,DMCB+20          BUY END DAY                                  
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            PREVIOUS WEEK                                
         LNR   R4,R4                                                            
*                                  BACK UP CONTRACT END DATE TO                 
*                                     LAST BUY DATE                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R4)                                   
*                                                                               
         LA    R5,1(R5)                                                         
         B     DATD0440                                                         
DATD0240 EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATD0280                                                         
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATD0260                                                         
***      CLI   0(R6),C'('                                                       
***      BE    DATD0260                                                         
         CLI   0(R6),0                                                          
         BE    DATD0260                                                         
         CLI   0(R6),C' '                                                       
         BNE   DATD0300                                                         
DATD0260 EQU   *                   NO END DATE GIVEN                            
         LR    R5,R6                                                            
DATD0280 EQU   *                   NO END DATE GIVEN                            
         LM    R6,R7,DMCB+16       START AND END DAYS                           
         CR    R6,R7               END IN NEXT WEEK?                            
         BNH   *+8                                                              
         LA    R7,7(R7)                                                         
         SR    R7,R6                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R7)                                  
         B     DATD0460                                                         
DATD0300 EQU   *                                                                
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATD0400                                                         
*                                                                               
*                                  CHECK FOR END WEEKS OPTION                   
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATD0320                                                         
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
DATD0320 EQU   *                   W HAS BEEN ENTERED - PACK WEEKS              
         LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
DATD0340 EQU   *                                                                
         CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,DATD0340                                                      
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         MVC   WORK+30(6),WORK+12  START DATE                                   
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
         BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATD0380                                                         
*                                                                               
*                                  TEST FOR ALTERNATE WEEKS                     
         CLI   0(R5),C'A'                                                       
         BNE   DATD0360                                                         
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
DATD0360 EQU   *                   GET NEXT WEEK                                
         GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATD0360         GET NUMBER OF WEEKS-1                        
*                                                                               
DATD0380 EQU   *                   GET DAY SPAN FOR WEEK                        
         L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATD0480                                                         
*                                                                               
DATD0400 EQU   *                   END DATE IS VALID MONTH-DAY                  
         MVC   WORK+18(2),WORK+6   CONTRACT END YEAR                            
         CLC   WORK+20(4),WORK+8   BUY END MMDD VS CONTRACT END MMDD            
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE CONTRACT START YEAR                     
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
*                                  VALIDATE END DATE                            
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DATD0440                                                         
*                                  FLIGHT BUY NEED NOT HAVE PROPER              
*                                     END DATE - FIND WK, ADJUST DATE           
         L     R7,DMCB+20          END DAY                                      
*                                  GET CONTRACT END DAY                         
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB                                                          
         LR    R3,R7                                                            
*                                                                               
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
DATD0420 EQU   *                                                                
         GOTO1 ADDAY,(R1),WORK+18,DUB,(R7)                                      
         MVC   WORK+18(6),DUB                                                   
*                                                                               
         CLC   WORK+18(6),WORK+6   DATE VS CONTRACT END                         
         BNH   *+12                                                             
         LA    R3,EDYERR                                                        
         B     ERROR               JUST IN CASE                                 
DATD0440 EQU   *                                                                
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATD0460                                                         
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATD0460 EQU   *                                                                
         OI    WORK2+8,X'80'       EVERY WEEK                                   
DATD0480 EQU   *                                                                
         LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    ERROR                                                            
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    ERROR                                                            
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR                        
*                                     CONSECUTIVE TEST                          
         MVI   WORK2+9,1           NUMBER OF SPOTS GIVEN                        
*                                     DEFAULTS TO 1                             
         GOTO1 NPWEDIT             VALIDATE NUMBER PER WEEK FIELD               
         BZ    DATD0520            VALID                                        
         LA    R3,NPWERR           INCORRECT:  PUT OUT ERROR MESSAGE            
         LA    R2,MGOMSPTH         SET CURSOR ADDRESS FOR ERROR                 
         A     R2,OFFRDISP         DISPLACE TO LINE                             
         B     ERROR                                                            
DATD0520 EQU   *                   NOW GET TOTAL WEEKS                          
         SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATD0540 EQU   *                                                                
         LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATD0540                                                         
*                                                                               
         STC   R7,WORK2+10                                                      
*                                  CONVERT DATES FOR MKGREC                     
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)                                  
*                                  START DATE                                   
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)                                    
*                                  END DATE                                     
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
*                                                                               
*        KEEP A WEEK COUNT, BECAUSE WEEKS CAN'T OVERLAP                         
*                                                                               
         L     RF,MGTOTWKS         KEEP WEEK COUNT                              
         ZIC   RE,WORK2+10         LOAD NUMBER WEEKS FROM X'03'                 
         AR    RF,RE                                                            
         ST    RF,MGTOTWKS         RESTORE TOTAL WEEKS                          
*                                                                               
*        CALCULATE NUMBER OF SPOTS/X'03' ELEMENT, ACCUMULATE                    
*                                                                               
         SR    RE,RE                                                            
         ZIC   RF,WORK2+9          LOAD NUMBER SPOTS FROM X'03'                 
         ZIC   R1,WORK2+10         LOAD NUMBER WEEKS FROM X'03'                 
         MR    RE,R1               # SPOTS X # WEEKS = TOTL SPOTS               
         L     RE,MGTOTSPT         LOAD SPOT COUNTER                            
         AR    RF,RE               ACCUMULATE                                   
         ST    RF,MGTOTSPT         RESTORE TOTAL SPOTS                          
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                  ADD DATE ELT TO MKGREC                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     DATD0040            GO BACK FOR NEXT                             
         EJECT                                                                  
*                                                                               
*   NPWEDIT:  VALIDATION OF NPW FIELD, INSERTION INTO APPROPRIATE               
*        AREAS.                                                                 
NPWEDIT  NTR1                                                                   
         LA    R2,MGOMSPTH                                                      
         A     R2,OFFRDISP                                                      
*                                                                               
         ZIC   RE,5(R2)            GET FIELD INPUT LENGTH                       
         SR    R1,R1                                                            
         LA    R2,8(R2)            BUMP TO DATA FIELD                           
         LR    RF,R2               SAVE A(DATA FIELD)                           
NPWE0040 EQU   *                   CHECK NUMBER PER WEEK                        
         CLI   0(R2),X'F0'         CHECK VALID NUMERIC                          
         BL    NPWE0080            ERROR EXIT                                   
         CLI   0(R2),X'F9'                                                      
         BH    NPWE0080            ERROR EXIT                                   
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         BCT   RE,NPWE0040         GO BACK AND CHECK NEXT FIELD                 
*                                                                               
         BCTR  R1,00               SUBTRACT 1 FOR EX STATEMENT                  
         EX    R1,*+8              PACK THE FIELD                               
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB              CONVERT IT TO BINARY                         
         CH    R1,=H'255'                                                       
         BH    NPWE0080            ERROR: CAN'T HAPPEN - ONLY                   
*                                     ALLOWING 2 POSITIONS IN FIELD             
         STC   R1,WORK2+9          NPW IN EFF DATE ELT                          
         STC   R1,RMKGNW           NPW IN DESCRIPTION ELEMENT                   
         SR    R0,R0               SET CC = ZERO                                
         B     NPWE0120                                                         
NPWE0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
NPWE0120 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
BUYED2   EQU   *                                                                
         MVI   BSTATUS,0           CLEAR OUT STATUS BYTE                        
ADDSPOT  DS    0H                                                               
         LA    R3,265                                                           
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   RATEDIT                                                          
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BAS   RE,GETEL                                                         
         BE    RATEDIT                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         CLI   TWASPES,0                                                        
         BNE   ADDSP10                                                          
                                                                                
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    RATEDIT             IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     ERROR                                                            
*                                                                               
ADDSP10  DS    0H                                                               
         XC    WORK2,WORK2                                                      
WK2      USING RMKGSPEL,WORK2                                                   
         MVC   WK2.RMKGSPCD(2),=X'0830'                                         
         MVC   WK2.RMKGSPAG,TWASPAG   SPOTPAK AGENCY POWER CODE                 
         MVC   WK2.RMKGSPMD,TWASPMD   SPOTPAK MEDIA CODE                        
         MVC   WK2.RMKGSPCL,TWASPCL   SPOTPAK CLIENT CODE                       
         MVC   WK2.RMKGSPPD,TWASPPD   SPOTPAK PRODUCT CODE                      
         MVC   WK2.RMKGSPES,TWASPES   SPOTPAK ESTIMATE NUMBER                   
         MVC   WK2.RMKGSPPP,TWASPPP   SPOTPAK PIGGY PRODUCT CODE                
         MVC   WK2.RMKGSPP1,TWASPP1   SPOTPAK PRODUCT 1 SPLIT                   
         MVC   WK2.RMKGSPP2,TWASPP2   SPOTPAK PRODUCT 2 SPLIT                   
         MVC   WK2.RMKGSPST,RCONKSTA  STATION CALL LETTERS                      
         MVC   WK2.RMKGSADV,RCONKADV  REPPAK ADVERTISER CODE                    
         MVC   WK2.RMKGSPRD,RCONPRD   REPPAK PRODUCT CODE                       
         DROP  WK2                                                              
         DROP  RF                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2   ADD SPOTPAK INTERFACE ELEM         
*                                                                               
RATEDIT  LA    R2,MGOMRATH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  RATE EDIT                                    
         LA    RE,MGOMDYSH                                                      
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   REDE0010                                                         
         LA    R3,622              NO INPUT FOR PREEMPT                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
*                                                                               
REDE0010 DS    0H                                                               
         LA    R3,RATERR                                                        
*                                                                               
         XC    RMKGCOMB,RMKGCOMB   DEFAULT BUY TO NON-COMBO                     
         XC    RMKGCOS,RMKGCOS                                                  
         CLI   5(R2),0             ANYTHING IN RATE FIELD?                      
         BNE   REDE0020            YES                                          
         L     RF,AORIGBUY         NO  - TAKE RATE FROM 'ORIG BUY'              
         MVC   RMKGCOS,RMKGCOS-RMKGREC(RF)                                      
         B     REDE0040                                                         
REDE0020 EQU   *                   EDIT RATE                                    
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,8(R2)                                               
*                                                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    ERROR                                                            
*                                                                               
         MVC   RMKGCOS,DMCB+4                                                   
*                                                                               
REDE0040 EQU   *                                                                
         CLC   =C'BONUS',MGOMMSD                                                
         BNE   REDE0050                                                         
         LA    R3,621                                                           
         OC    RMKGCOS,RMKGCOS     RATE MUST BE $0 FOR BONUS                    
         BNZ   ERROR                                                            
*                                                                               
*   CALCULATE TOTAL SPOTS, WEEKS, $$ FOR THIS BUYLINE                           
*                                                                               
         SR    RE,RE                                                            
REDE0050 EQU   *                                                                
         L     RF,MGTOTSPT         TOTAL SPOTS                                  
         ZICM  R1,RMKGCOS,4        INSERT COST                                  
         MR    RE,R1               TOTL SPOTS X COST = TOTAL $$                 
         STCM  RF,15,RMKGTCOS      INSERT TOTAL COST                            
         MVC   RMKGTSPT,MGTOTSPT+2                                              
*                                  INSERT TOTAL SPOTS (2 CHARS ONLY)            
         MVC   RMKGTWKS,MGTOTWKS+3                                              
*                                  INSERT TOTAL WEEKS (1 CHAR ONLY)             
         XC    MGTOTSPT,MGTOTSPT                                                
         XC    MGTOTWKS,MGTOTWKS                                                
*                                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'REPPAK BUY LENGTH EDIT'                                         
*                                  EDIT LENGTH                                  
LENEDIT  LA    R2,MGOMLENH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  RATE EDIT                                    
         LA    RE,MGOMDYSH                                                      
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   LENE0005                                                         
         LA    R3,622              NO INPUT FOR PREEMPT                         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
*                                                                               
LENE0005 EQU   *                                                                
         LA    R3,LENERR                                                        
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BNE   LENE0010            YES                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'                                                   
         BO    ERROR               BONUS NEEDS LENGTH INPUT                     
         DROP  RF                                                               
*                                                                               
         L     RF,AORIGBUY         NO  - TAKE LENGTH FROM ORIG BUY              
         MVC   RMKGDUR,RMKGDUR-RMKGREC(RF)                                      
*                                  INSERT INTO NEW RECORD                       
         B     DETCOMNT                                                         
LENE0010 EQU   *                                                                
         GOTO1 VPACK               LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0040                                                         
* VALID SECONDS                                                                 
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0020                                                         
         CH    R0,=H'120'                                                       
         BNH   LENE0020                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
LENE0020 EQU   *                                                                
         STH   R0,HALF                                                          
         MVC   RMKGDUR,HALF        GO PROCESS DETAIL COMMENT                    
         B     DETCOMNT                                                         
* TEST FOR MINUTES                                                              
LENE0040 EQU   *                                                                
         LA    R4,4                                                             
         LA    R5,MGOMLEN                                                       
         A     R5,OFFRDISP         OFFSET TO SPECIFIC LINE                      
LENE0060 EQU   *                                                                
         CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0080                                                         
         CLI   0(R5),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R5),X'F9'                                                      
         BH    ERROR                                                            
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0060                                                      
         B     ERROR                                                            
*                                  PACK MINUTES (MINUTES NOT ALLOWED            
*                                     FOR SPOTPAK TRANSFER)                     
LENE0080 EQU   *                                                                
*                                                                               
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0100                                                         
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0100 EQU   *                                                                
         LA    R2,MGOMLEN          PREPARE TO PACK LEN                          
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   ERROR                                                            
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     LENE0120                                                         
         PACK  DUB,0(0,R2)         PACK LENGTH BY LENGTH                        
LENE0120 EQU   *                                                                
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   RMKGDUR,HALF                                                     
         OI    RMKGDUR,X'80'       MINUTES IND                                  
*                                                                               
*        LA    R3,638              NOT SURE HOW LONG MINUTES SHOULD BE          
*        CLC   HALF,=H'60'                                                      
*        BH    ERROR                                                            
         EJECT                                                                  
*                                                                               
*   PROCESS DETAIL COMMENT, IF ANY                                              
*   EXPAND TO PROCESS PROGRAM NAME AS WELL, IF ANY :)                           
DETCOMNT EQU   *                                                                
         XC    ELTBILD,ELTBILD     CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBILD,X'21'       YES - SET ELEMENT TYPE CODE                  
         LA    R2,MGOOPGNH         SET A(PROGRAM NAME)                          
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
DPRG0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    DPRGX               NO  - GO SET NEXT LINE #                     
*                                                                               
         LA    R3,627              MUST LEAVE BLANK FOR ZERO SPOT               
         OC    RMKGNW,RMKGNW                                                    
         BZ    ERROR                                                            
*                                                                               
DPRG0030 EQU   *                                                                
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,DPRG0040         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            ADD FOR CONTROL, EX                          
         STC   RF,ELTBILD+1        INSERT LENGTH INTO ELEMENT                   
*                                  ADD ELEMENT TO RECORD                        
         B     DPRG0100                                                         
DPRG0040 EQU   *                                                                
         MVC   ELTBILD+2(0),8(R2)  MOVE COMMENT BY LENGTH                       
*                                  ADD ELEMENT TO RECORD                        
DPRG0060 EQU   *                                                                
*&&DO                                                                           
* DISABLE AUTO PROGRAM NAME POPULATION PER CLIENT'S REQUEST                     
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGDS  READ ORIGINAL BUY RECORD VIA                 
         DROP  RF                     DISK ADDRESS                              
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DPRGX                                                            
*                                                                               
         ZIC   R5,1(R6)                                                         
         SHI   R5,3                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELTBILD+2(0),2(R6)                                               
         AHI   R5,3                ADD FOR CONTROL, EX                          
*MN                                                                             
         CHI   R5,19                                                            
         BNH   *+8                                                              
         LA    R5,19                                                            
*MN                                                                             
         STC   R5,ELTBILD+1        INSERT LENGTH INTO ELEMENT                   
*&&                                                                             
DPRG0100 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
*                                                                               
DPRGX    EQU   *                                                                
*                                                                               
*  PROCESS COMMENT                                                              
*                                                                               
DCMT0000 EQU   *                                                                
         XC    ELTBILD,ELTBILD     CLEAR ELEMENT BUILD AREA                     
         LA    R2,MGODCOMH         SET A(DETAIL COMMENT)                        
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         LA    RE,MGOMDYSH                                                      
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   DCMT0020                                                         
         CLI   MGOMOPT,C'A'                                                     
         BE    DCMT0030                                                         
         LA    R2,MGOMOPTH                                                      
         LA    R3,620              MUST OFFER ALL                               
         B     ERROR                                                            
*                                                                               
DCMT0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    DDEM0000            NO  - GO SET NEXT LINE #                     
*                                                                               
         LA    R3,627              MUST LEAVE BLANK FOR ZERO SPOT               
         OC    RMKGNW,RMKGNW                                                    
         BZ    ERROR                                                            
*                                                                               
DCMT0030 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    DDEM0000            NO  - GO SET NEXT LINE #                     
         LA    R2,MGODCOMH         SET A(DETAIL COMMENT)                        
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    DDEM0000            NO  - GO SET NEXT LINE #                     
*                                                                               
         MVI   ELTBILD,X'11'       YES - SET ELEMENT TYPE CODE                  
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,DCMT0040         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            ADD FOR CONTROL, EX                          
         STC   RF,ELTBILD+1        INSERT LENGTH INTO ELEMENT                   
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
*                                  ADD ELEMENT TO RECORD                        
         B     DDEM0000                                                         
DCMT0040 EQU   *                                                                
         MVC   ELTBILD+2(0),8(R2)  MOVE COMMENT BY LENGTH                       
*                                                                               
DDEM0000 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OC    TWADEM,TWADEM                                                    
         BZ    LIN#0020                                                         
         GOTOR CKSTPROF                                                         
         BNE   LIN#0020                                                         
         GOTOX =A(GETDEMV),RR=Y                                                 
         DROP  RF                                                               
*  PROCESS DEMO VALUE                                                           
*                                                                               
*                                  SET NEXT LINE NUMBER                         
LIN#0020 EQU   *                                                                
*                                  DETERMINE MAKEGOOD LINE #...                 
*                                     THIS CODE IS REPEATED IN COMEDIT          
         CLI   NEXTBUYL,0          HAS NEXT BUY # BEEN DETERMINED?              
         BNE   LIN#0220            YES - DON'T REDO SCAN                        
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'      BONUS DOES NOT NEED BUYLINE NUMBER           
         BZ    LIN#0030                                                         
         SR    R0,R0                                                            
         B     LIN#0035                                                         
*                                                                               
LIN#0030 DS    0H                                                               
         LA    R3,MGNEEDBL                                                      
         ZIC   R0,TWAMKGL#         GET BUY NUMBER FROM TWA                      
         LTR   R0,R0                                                            
         BZ    ERROR               NEEDS BUYLINE NUMBER                         
*                                                                               
LIN#0035 DS    0H                                                               
         STC   R0,RMKGKMLN         INSERT BUY # INTO KEY M/G LIN#               
         MVC   RMKGKLIN,RMKGKMLN   INSERT BUY # INTO KEY ORIG LIN#              
         MVI   RMKGKRTY,0          CLEAR RECORD TYPE                            
*                                                                               
         MVI   RMKGKTYP,X'11'      BUY KEY TYPE                                 
         MVC   RMKGKREP,REPALPHA   REP CODE                                     
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE                                
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                         
         MVC   RMKGKCON,TWACNUM    CONTRACT NUMBER                              
         CLC   =C'MGO',CONCACT     ADD ACTION?                                  
         BE    LIN#0040            YES, ALWAYS GET A NEW GROUP CODE             
         OC    TWAMKGDT,TWAMKGDT   ANY GROUP CODE?                              
         BZ    LIN#0040            NO, SET UP NEW CODE                          
         MVC   RMKGKGRP,TWAMKGDT   YES, INSERT INTO KEY                         
         B     LIN#0140                                                         
         DROP  RF                                                               
*                                                                               
LIN#0040 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
*                                  SET KEY THROUGH CONTRACT NUMBER              
         MVC   KEY(RMKGKGRP-RMKGKEY),RMKGREC                                    
         XC    WORK,WORK                                                        
*                                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
*                                                                               
LIN#0050 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
         GOTO1 VHIGH               GET FIRST RECORD OF SET                      
*                                  SAME THROUGH CONTRACT #?                     
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   LIN#0060            NO  - ASSIGN NEXT NUMBER                     
         MVC   WORK(2),RMKGKGRP    SAVE GROUP CODE                              
*                                  CLEAR TO END OF KEY                          
         XC    RMKGKPLN(6),RMKGKPLN                                             
         ZICM  RF,RMKGKGRP,2       BUMP GROUP CODE                              
         LA    RF,1(RF)                                                         
         STCM  RF,3,RMKGKGRP                                                    
         B     LIN#0050            GO BACK FOR NEXT KEY                         
         DROP  R6                                                               
*                                                                               
LIN#0060 EQU   *                                                                
         OC    WORK(2),WORK        IF GROUP CODE PRESENT                        
         BNZ   LIN#0065            FIGURE OUT NEXT VALUE                        
*                                                                               
         MVC   RMKGKGRP,=C'AA'     NO  - START AT 'AA'                          
*                                                                               
* IF TAKEOVER, AVOID CONFLICT WITH PREVIOUS REP'S OFFER CODES BY                
* STARTING OFFER CODE AT BA INSTEAD OF AA                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LIN#0070                                                         
         MVC   RMKGKGRP,=C'DA'                                                  
         B     LIN#0070                                                         
*                                                                               
* IF TAKEOVER, AND OFFER NOT ALREADY STARTED AT DA, FORCE IT TO START           
* AT GROUP CODE DA                                                              
*                                                                               
LIN#0065 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        TAKEOVER?                                    
         BRAS  RE,GETEL                                                         
         BNE   LIN#0100                                                         
         CLC   WORK(2),=C'DA'                                                   
         BNL   LIN#0100                                                         
         MVC   RMKGKGRP,=C'DA'                                                  
*                                                                               
LIN#0070 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   TWAMKGDT,RMKGKGRP   SAVE GROUP CODE IN TWA                       
         DROP  RF                                                               
         B     LIN#0140            USE THE KEY SET UP                           
*                                                                               
LIN#0100 EQU   *                                                                
         LA    RF,LETRTABL                                                      
         LA    R7,WORK+1           PREPARE TO BUMP SECOND CHAR                  
         CLI   WORK+1,C'Z'         LAST VALUE IN SET?                           
         BNE   LIN#0120            NO  - BUMP SECOND CHAR                       
         LA    R7,WORK             YES - PREPARE TO BUMP FIRST CHAR             
         MVI   WORK+1,C'A'         RESET 2ND CHAR TO 'A'                        
LIN#0120 EQU   *                                                                
         CLC   0(1,RF),0(R7)       LETTER FOUND IN TABLE?                       
         BE    LIN#0130            YES - USE NEXT LETTER                        
         LA    RF,1(RF)                                                         
         CLI   0(RF),0             DELIMITER REACHED?                           
         BNE   LIN#0120            NO  - GO BACK FOR NEXT                       
         DC    H'0'                SHOULDN'T HAPPEN                             
LIN#0130 EQU   *                                                                
         LA    RF,1(RF)            BUMP TO NEXT LETTER                          
         MVC   0(1,R7),0(RF)       MOVE FROM TABLE TO CODE                      
         MVC   RMKGKGRP,WORK       LOAD NEW SET OF CODE LETTERS                 
*                                                                               
         MVC   MGOMDTE,RMKGKGRP                                                 
         OI    MGOMDTEH+6,X'80'    XMIT FIELD                                   
*                                                                               
         B     LIN#0140                                                         
LETRTABL DC    C'ABCDEFGHIJ'                                                    
         DC    C'KLMNOPQRST'                                                    
         DC    C'UVWXYZ'                                                        
         DC    X'00'               DELIMITER                                    
         DS    0F                                                               
LIN#0140 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
*                                  FIND NEXT LINE NUMBER                        
         XC    HALF,HALF           LINE NUMBER                                  
         XC    KEY,KEY                                                          
         MVC   KEY(21),RMKGREC     INSERT INTO KEY                              
         GOTO1 VHIGH                                                            
*                                                                               
LIN#0160 EQU   *                                                                
         CLC   KEY(21),KEYSAVE     SAME KEY THROUGH GROUP CODE?                 
         BNE   LIN#0200            NO  - BREAK ON OFFER                         
         OC    KEY+21(6),KEY+21    GROUP COMMENT RECORD?                        
         BZ    LIN#0180            YES - SKIP IT                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLC   TWAMKGL#,KEY+24     SAME MASTER LINE #?                          
         BNE   LIN#0200            NO  - BREAK ON OFFER                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   KEY+25,X'FF'        PLANREC?                                     
         BE    LIN#0180            YES - SKIP IT                                
         CLC   HALF+1(1),KEY+25    ORIGINAL LINE # VS PREV HIGH VAL             
         BNL   *+10                                                             
         MVC   HALF+1(1),KEY+25    HIGHEST LINE NUMBER SO FAR                   
*                                                                               
LIN#0180 EQU   *                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     LIN#0160                                                         
*                                                                               
LIN#0200 EQU   *                                                                
         CLC   CONCACT(3),=C'MGC'  'CHANGE' ACTION?                             
         BNE   LIN#0210            NO  - 'ADD' ACTION                           
         LR    RF,RA               YES - FORCE NEXTBUYL VALUE FROM              
*                                     THAT OF CHANGING MG OFFER                 
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   NEXTBUYL,TWAMKGML   RESET MAKEGOOD LINE #                        
         B     LIN#0220                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
LIN#0210 EQU   *                                                                
         LH    RE,HALF             LAST LINE NUMBER USED                        
         LA    RE,1(RE)            BUMP TO NEXT NUMBER                          
         STC   RE,NEXTBUYL         SAVE VALUE OF BUYLINE                        
         CH    RE,=H'255'          ANY MORE ROOM?                               
         BL    LIN#0240            YES                                          
         LA    R2,CONCACTH         NO  - ISSUE MESSAGE                          
         LA    R3,MAXERR                                                        
         B     ERROR                                                            
*                                                                               
LIN#0220 EQU   *                                                                
         ZIC   RE,NEXTBUYL         CALCULATE NEXT BUYLINE #                     
*                                  IF A MULTI-LINE OFFER OR                     
*                                     A CHOICE, ALL RECEIVE SAME                
*                                     BUYLINE NUMBERS, AND ARE                  
*                                     SEPARATED BY VALUE OF                     
*                                     RMKGKRTY FIELD                            
LIN#0240 EQU   *                                                                
         STC   RE,RMKGKLIN         BUY LINE NUMBER                              
         NI    DMINBTS,X'F7'       TURN OFF DELETE PASS                         
         B     COMEDIT                                                          
         EJECT                                                                  
*              VALIDATE MISSED DATE(S)/# SPOTS                                  
MGNEEDBL EQU   454                 INVALID COMMENT LINE FORMAT                  
MGMISDER EQU   455                 M/G MISSED DATE/SPOTS ERROR                  
*                                                                               
COMEDIT  EQU   *                                                                
         XC    BYTE,BYTE                                                        
         LA    R2,MGOMMSDH         A(MISSED DATE LINE HEADER)                   
*                                                                               
         XC    IOAREA(32),IOAREA   NO OLD MG LINE                               
*                                  DROP OLD M/G REFS, IF ANY                    
*                                  GET OLD MG ELEM                              
         GOTO1 VDELELEM,DMCB,(4,RMKGREC)                                        
*                                                                               
         CLC   =C'BONUS',MGOMMSD                                                
         BE    CEDI0410                                                         
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CEDI0360            NOT FOUND                                    
         DC    H'0'                                                             
*                                  DELETE M/G ELTS CURRENT M/G REC              
CEDI0360 EQU   *                                                                
*                                  DELETE COMMENTS (CHANGE)                     
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         XC    TWAELEM,TWAELEM     CLEAR X'04' BUILD AREA                       
         LA    R3,TWAELEM                                                       
         DROP  RF                                                               
*                                                                               
         MVI   0(R3),X'04'         ELT CODE IN FIRST POSITION                   
*                                                                               
         LA    RE,MGOMDYSH         PREEMPT WILL BECOME A CREDIT                 
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   CEDI0380                                                         
         MVC   2(3,R3),=C'CR='     BUILD CR=NNN, NNN IS TARGET BUY              
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         EDIT  TWAMKGL#,(3,5(R3)),ALIGN=LEFT                                    
         DROP  RF                                                               
*                                                                               
         LR    RF,R0                                                            
         LA    RF,5(RF)            CALCULATE ELEMENT LENGTH                     
         STC   RF,1(R3)                                                         
         B     CEDI0390                                                         
*                                                                               
*                                                                               
CEDI0380 EQU   *                                                                
         MVC   2(3,R3),=C'MG='                                                  
*                                                                               
CEDI0390 EQU   *                                                                
         LA    R3,5(R3)                                                         
         ST    R3,AELTBILD         SAVE A(POINTER WITHIN STRING)                
         MVI   X04FLAG,0           CLEAR 1ST DATE IN STRING FLAG                
         LA    R2,MGOMMSDH         SET A(MISSED DATE/SPOTS HDR)                 
         LA    RE,MGOMMSD          SET A(MISSED DATE/SPOTS FIELD)               
CEDI0400 EQU   *                                                                
         LA    R3,MGMISDER                                                      
         MVI   ANOTHER1,0          CLEAR 'ANOTHER MISSED DATE' FLAG             
         LR    R4,RE               SAVE POINTER WITHIN STRING                   
         XC    WORK(10),WORK                                                    
WKD      USING RMKGMGEL,WORK                                                    
*                                  EDIT MAKE-GOOD REFERENCE                     
         MVI   WKD.RMKGMGCD,X'05'                                               
         MVI   WKD.RMKGMGLN,10                                                  
*********************************************************                       
* CHECKS TO SEE IF THE INPUT IS IN M/D/Y FORM                                   
* IF SO, THE USER SPECIFY THE YEAR EXPLICITLY                                   
* THEREFORE, USE THE YEAR THE USER INPUT INSTEAD OF TRYING TO FIGURE            
* IT OUT                                                                        
*********************************************************                       
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               START OF NEXT SCREEN FIELD                   
         LR    RF,R4               START OF THIS INPUT CHUNK                    
         SR    R1,R1               CLEAR / COUNTER                              
CEDI0404 CR    RF,RE                                                            
         BNL   CEDI0406            REACH THE END OF THIS FIELD                  
         CLI   0(RF),C','          INPUT CHUNK IS COMMA SEPERATED               
         BE    CEDI0406            SO STOP ON COMMA                             
         CLI   0(RF),C'/'          COUNT /                                      
         BNE   *+8                                                              
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    RF,1(RF)            NEXT CHARACTER                               
         B     CEDI0404            LOOP                                         
*                                                                               
CEDI0406 DS    0H                                                               
         STC   R1,BYTE             SAVE COUNTER                                 
         CLI   BYTE,2              2 = M/D/Y, 1 = M/D                           
         BL    CEDI0408                                                         
         GOTO1 DATVAL,DMCB,(0,(R4)),DUB     DATVAL OPTION 0 FOR M/D/Y           
         B     CEDI0409                                                         
*                                                                               
CEDI0408 GOTO1 DATVAL,DMCB,(1,(R4)),DUB     DATVAL OPTION 1 FOR M/D             
CEDI0409 OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         LR    RE,R4               RESET POINTER WITHIN STRING                  
         A     RE,DMCB             ADD LENGTH OF DATE FIELD                     
         LR    R4,RE               SAVE POINTER WITHIN STRING                   
*                                  FORMAT IS JAN15/#                            
         GOTO1 DATCON,DMCB,DUB,(3,WKD.RMKGMGD1)                                 
*                                  CONVERT DATE TO YYMMDD BINARY                
         LR    RE,R4               RESET POINTER WITHIN STRING                  
*                                                                               
CEDI0410 EQU   *                                                                
         MVI   WKD.RMKGMGSP,1      NUMBER OF SPOTS                              
         CLI   BYTE,2              USER INPUT IN M/D/Y?                         
         BNL   CEDI0415            YES,USE THE USER INPUT YEAR                  
         MVC   WKD.RMKGMGD1(1),RCONDATE  CONTRACT START YEAR                    
         CLC   =C'BONUS',MGOMMSD                                                
         BE    CEDI0415                                                         
         CLC   WKD.RMKGMGD1+1(2),RCONDATE+1                                     
*                                  DETERMINE YEAR TO USE                        
         BNL   CEDI0415                                                         
         CLC   WKD.RMKGMGD1(1),RCONDATE+3                                       
         BE    ERROR                                                            
         MVC   WKD.RMKGMGD1(1),RCONDATE+3                                       
*                                  USE CONTRACT END YEAR                        
*                                                                               
CEDI0415 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   WKD.RMKGMGLI,TWAMKGL#  INSERT MISSED LINE NUMBER                 
         DROP  RF                                                               
*                                                                               
         CLC   =C'BONUS',MGOMMSD                                                
         BE    CEDI0610                                                         
*                                  GET NUMBER OF MISSED SPOTS                   
         CLI   0(RE),C','          FIELD SEPARATOR?                             
         BNE   CEDI0440            NO                                           
         MVI   ANOTHER1,1          YES - SET FLAG TO 'YES'                      
         B     CEDI0560            NO EXPLICIT SPOTS:  SET TO 1                 
*                                                                               
* END DATE SUPPORT ONLY FOR PREEMPT FOR NOW                                     
* END DATE SUPPORT DISABLED 8/15/01 SKUI                                        
CEDI0440 EQU   *                                                                
         B     CEDI0470                                                         
*        CLC   =C'PREEMPT',MGOMDYS                                              
*        BNE   CEDI0470            DIFFERENT VALIDATION FOR PREEMPT             
         CLI   0(RE),C'-'          END DATE OR RANGE IS SPECIFIED               
         BNE   CEDI0470                                                         
         LA    R4,1(RE)                                                         
         GOTO1 DATVAL,DMCB,(1,(R4)),DUB                                         
         OC    DMCB(4),DMCB                                                     
         BZ    CEDI0460            RANGE IS SPECIFIED INSTEAD                   
         A     R4,DMCB                                                          
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,WKD.RMKGMGD2)                                 
*                                                                               
         MVC   WKD.RMKGMGD2(1),RCONDATE  CONTRACT START YEAR                    
         CLC   WKD.RMKGMGD2+1(2),RCONDATE+1                                     
*                                  DETERMINE YEAR TO USE                        
         BNL   CEDI0445                                                         
         CLC   WKD.RMKGMGD2(1),RCONDATE+3                                       
         BE    ERROR                                                            
         MVC   WKD.RMKGMGD2(1),RCONDATE+3                                       
*                                  USE CONTRACT END YEAR                        
CEDI0445 EQU   *                                                                
         LR    RE,R4                                                            
         B     CEDI0470                                                         
*                                                                               
CEDI0460 EQU   *                                                                
         CLI   0(R4),C'E'          FORMAT IS MMMDD-E                            
         BNE   CEDI0462                                                         
*                                                                               
         L     R6,AORIGBUY                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CEDI0461 LR    R2,R6               GET TARGET BUY END DATE                      
         BAS   RE,NEXTEL                                                        
         BE    CEDI0461                                                         
*                                                                               
         LR    R6,R2                                                            
         USING RBUYDTEL,R6                                                      
         MVC   WKD.RMKGMGD2,RBUYDTED                                            
         DROP  R6                                                               
*                                                                               
         LA    RE,1(R4)                                                         
         B     CEDI0470                                                         
*                                                                               
CEDI0462 EQU   *                   FORMAT IS MMMDD-MMMDD                        
         LA    R1,1                                                             
         CLI   1(R4),C'W'          WEEKS IND?                                   
         BE    CEDI0463                                                         
         LA    R1,2                                                             
         CLI   2(R4),C'W'                                                       
         BE    CEDI0463                                                         
         LR    RE,R4                                                            
         B     CEDI0470                                                         
*                                                                               
CEDI0463 DS    0H                                                               
         LR    RE,R1                                                            
         LA    R2,0(R4)                                                         
*                                                                               
CEDI0465 CLI   0(R2),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R2),C'9'                                                       
         BH    ERROR                                                            
         AHI   R2,1                                                             
         BCT   RE,CEDI0465                                                      
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R4)                                                      
         CVB   R7,DUB                                                           
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
         MHI   R7,7                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,WKD.RMKGMGD1),WORK+20                             
         GOTO1 ADDAY,DMCB,WORK+20,WORK+20,(R7)                                  
         GOTO1 DATCON,DMCB,WORK+20,(3,WKD.RMKGMGD2)                             
*                                  CALCULATE CREDIT END DATE                    
         GOTO1 =A(CRDENDDT),RR=Y                                                
*                                                                               
         LA    RE,1(R2)                                                         
*                                                                               
CEDI0470 EQU   *                                                                
         CLI   0(RE),C','          FIELD SEPARATOR?                             
         BNE   CEDI0475            NO                                           
         MVI   ANOTHER1,1          YES - SET FLAG TO 'YES'                      
         B     CEDI0560            NO EXPLICIT SPOTS:  SET TO 1                 
*                                                                               
CEDI0475 CLI   0(RE),C' '          LAST FIELD?                                  
         BE    CEDI0560            YES - FINISHED                               
         CLI   0(RE),X'00'         LAST FIELD?                                  
         BE    CEDI0560            YES - FINISHED                               
         CLI   0(RE),C'('          # SPOTS FOLLOWS?                             
         BNE   ERROR               NO  - UNRECOGNIZED CHARACTER                 
         LA    RE,1(RE)            YES - EDIT NUMBER OF SPOTS                   
         LR    R4,RE                                                            
         SR    R1,R1                                                            
CEDI0480 EQU   *                                                                
         CLI   0(RE),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'F9'                                                      
         BH    ERROR                                                            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         CLI   0(RE),C')'          # SPOTS TERMINATOR?                          
         BNE   CEDI0480            NO  - GO BACK FOR NEXT                       
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         MISSED NUMBER OF SPOTS                       
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CLI   0(RE),C' '          NO MORE DATA?                                
         BE    CEDI0520            YES - FINISHED                               
         CLI   0(RE),X'00'         NO MORE DATA?                                
         BE    CEDI0520            YES - FINISHED                               
         CLI   0(RE),C','          ANOTHER FIELD FOLLOWS                        
         BNE   ERROR               NO  - UNRECOGNIZED CHARACTER                 
         MVI   ANOTHER1,1          YES - SET FLAG FOR ANOTHER                   
*                                                                               
*  NOTE:  IF NOT FIRST OFFER LINE, # MISSED SPOTS MUST BE ZERO!                 
*    ELSE WE WILL TAKE THE SPOT(S) OUT OF THE ORIGINAL LINE FOR                 
*    EACH OF THE OFFERS, WHICH IN INCORRECT!                                    
*                                                                               
CEDI0520 EQU   *                                                                
         STC   R0,WKD.RMKGMGSP     NO. MISSED SPOTS                             
         LA    R3,MGOMDYSH                                                      
         A     R3,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(R3)                                                
         BNE   CEDI0560                                                         
         LTR   R0,R0                                                            
         BNZ   CEDI0560                                                         
         LA    R3,752              CANNOT PREEMPT ZERO MISSED SPOT              
         B     ERROR                                                            
*                                                                               
CEDI0560 EQU   *                                                                
         CLI   LINECNTR,1          1ST LINE?                                    
         BE    CEDI0600            YES - USE # MISSED SPOTS                     
         CLI   MGOMOPT,C'A'        'CHOICE' OF LINES REQUESTED?                 
         BNE   CEDI0600            YES - USE # MISSED SPOTS BECAUSE             
*                                     ONLY ONE LINE WILL BE ACCEPTED.           
         MVI   WKD.RMKGMGSP,0      NO  - CLEAR MISSED # SPOTS                   
CEDI0600 EQU   *                                                                
         LA    RE,1(RE)            SKIP SEPARATOR                               
         ST    RE,AMISSED          SAVE A(POINTER W/IN MISSED STRING)           
*                                                                               
         LA    RE,MGOMDYSH         PREEMPT WILL BECOME A CREDIT                 
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   CEDI0605                                                         
         MVC   RMKGNW,WKD.RMKGMGSP #SPOTS CREDITED                              
         B     CEDI0610                                                         
*                                                                               
CEDI0605 EQU   *                                                                
         GOTO1 BILDX04                                                          
*                                                                               
*   SHOULDN'T CARE WHETHER MAKEGOOD TARGET AND THIS M/G OFFER                   
*        LINE HAVE SAME NUMBER BECAUSE THEY ARE TWO DIFFERENT                   
*        RECORD TYPES.  NUMBERS WILL BE REASSIGNED WHEN FINAL                   
*        RECORDS ARE GENERATED AT ACCEPTANCE.                                   
*                                                                               
CEDI0610 EQU   *                                                                
****>>>> CLC   WORK+2(1),RMKGKLIN  SAME AS THIS (CHANGE)?                       
****>>>> BE    ERROR                                                            
         CLC   WKD.RMKGMGLI,IOAREA+25 MISSED LINE ALREADY IN CORE?              
         BE    CEDI0840            YES                                          
*        LA    R7,RBUYREC          NO  - CHECK ALTERNATE STORAGE FOR            
*                                     ORIGINAL BUY RECORD                       
*        A     R7,=F'7000'         BUMP TO ALTERNATE STORAGE                    
         L     R7,ASPULAR          NO  - CHECK ALTERNATE STORAGE FOR            
*                                     ORIGINAL BUY RECORD                       
         A     R7,=F'4000'         BUMP TO ALTERNATE STORAGE                    
         OC    0(64,R7),0(R7)      ANYTHING IN STORAGE?                         
         BZ    CEDI0640            NO                                           
         GOTO1 VMOVEREC,DMCB,(R7),IOAREA                                        
*                                  YES - MOVE IT TO IOAREA                      
         MVC   KEY,IOAREA          SET VALUE OF KEY FROM STORAGE                
         MVC   RMKGKPLN,KEY+22     INSERT ORIG BUY PLAN #                       
         B     CEDI0840            BYPASS RETRIEVAL OF ORIG BUY RECORD          
*                                  DOES M/G OFFER TARGET EXIST?                 
CEDI0640 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R7,KEY                                                           
         MVI   KEY,X'0B'           INSERT BUY RECORD TYPE                       
         MVC   KEY+RBUYKREP-RBUYREC(2),RMKGKREP                                 
*                                  INSERT REP                                   
         MVC   KEY+RBUYKCON-RBUYREC(4),RMKGKCON                                 
*                                  INSERT CONTRACT #                            
*                                                                               
         LA    R3,MGNERR           ORIGINAL BUY RECORD ERROR                    
         GOTO1 VHIGH                                                            
         B     CEDI0720                                                         
CEDI0680 EQU   *                                                                
         GOTO1 VSEQ                                                             
CEDI0720 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME CONTRACT NUMBER?                        
         BE    CEDI0760            YES - CHECK BUYLINE NUMBER                   
         DC    H'0'                MUST HAVE BEEN FOUND FOR DISPLAY             
CEDI0760 EQU   *                                                                
         CLC   KEY+26(1),WKD.RMKGMGLI BUYLINE FOUND?                            
         BNE   CEDI0680            NO  - GO BACK FOR NEXT                       
*        CLC   KEY+25(1),WKD.RMKGMGLI ORIGINAL LINE (NOT M/G)?                  
*        BNE   CEDI0680            NO  - GO BACK FOR NEXT                       
*                                                                               
CEDI0800 EQU   *                                                                
         MVC   RMKGKPLN,KEY+22     YES - INSERT ORIG BUY PLAN #                 
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA YES - RETRIEVE RECORD                        
*                                                                               
* CANNOT HAVE MAKEGOOD AGAINST CREDIT                                           
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   CEDI0810                                                         
         USING RBUYCMEL,R6                                                      
         CLC   =C'CR=',RBUYCMNT                                                 
         BNE   CEDI0810                                                         
         LA    R3,650                                                           
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
CEDI0810 EQU   *                                                                
         CLC   CONCACT(3),=C'MGC'  'CHANGE' ACTION?                             
         BNE   CEDI0840            NO                                           
         BRAS  RE,DELOLD66       YES - DROP X'66' ELEMENTS FOR THIS             
*                                     MG OFFER FROM BUY RECORD                  
*                                                                               
*                                  CHECK FOR VALID DATE -                       
*                                     NOT 2/29 IN NON-LEAP-YEAR                 
CEDI0840 EQU   *                                                                
         CLC   =C'BONUS',MGOMMSD   SKIP FOR BONUS                               
         BE    CEDI1450                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,WKD.RMKGMGD1),DUB                                 
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),MYSPACES                                                 
         BE    MISSERR                                                          
CEDI0880 MVC   FULL(1),DMCB        DAY OF WEEK                                  
*                                  GET VALID DAYS FOR MISSED LINE               
         MVI   FULL+1,0                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   CEDI0920                                                         
         OC    FULL+1(1),3(R6)     GET VALID DAYS IN FULL+1                     
         BAS   RE,NEXTEL                                                        
         BE    *-10                                                             
*                                  CHECK MISSED DAY IN M/G VS                   
*                                     VALID MISSED LINE DAYS                    
CEDI0920 ZIC   R4,FULL             MISSED DAY OF WEEK                           
         LA    R5,128              X'80'                                        
         SRL   R5,1                                                             
         BCT   R4,*-4                                                           
*                                                                               
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    FULL+1,X'00'                                                     
         BZ    MISSERR                                                          
*                                  CHECK MISSED DAY VS EFFECT                   
*                                     WEEKS OF MISSED LINES                     
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   MISSERR                                                          
*                                                                               
         GOTO1 DATCON,(R1),(3,WKD.RMKGMGD1),WORK+14                             
*                                                                               
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         LA    RE,IOAREA                                                        
         IC    R4,RBUYSTED-RBUYREC(RE)                                          
*                                  LOW AND HIGH DAYS                            
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         CR    R4,R5                                                            
         BNH   *+8                                                              
         LA    R5,7(R5)            OUT OF WEEK ROTATOR                          
         SR    R5,R4               R7 NOW HAS DAY SPAN IN WEEK                  
CEDI0960 EQU   *                                                                
         GOTO1 DATCON,(R1),(3,2(R6)),WORK+20                                    
*                                  START DATE                                   
         GOTO1 (RF),(R1),(3,5(R6)),WORK+26                                      
*                                  END   DATE                                   
*                                  GET LAST DAY IN WEEK                         
CEDI1000 EQU   *                                                                
         GOTO1 ADDAY,(R1),WORK+20,WORK+32,(R5)                                  
*                                                                               
         CLC   WORK+14(6),WORK+20  DATE WITHIN THIS WEEK?                       
         BL    MISSERR             NO                                           
*                                                                               
         CLC   WORK+14(6),WORK+32  DATE VS END OF WEEK                          
         BNH   CEDI1040            YES                                          
*                                                                               
         LA    R3,7                NO  - TRY NEXT WEEK                          
         TM    8(R6),X'40'         ALTERNATE WEEKS?                             
         BZ    *+8                 NO                                           
         LA    R3,14               YES - BUMP TWO WEEKS                         
         GOTO1 ADDAY,(R1),WORK+20,WORK+38,(R3)                                  
         MVC   WORK+20(6),WORK+38                                               
         CLC   WORK+20(6),WORK+26  RUNNING DATE V ELEM END DATE                 
         BNH   CEDI1000                                                         
*                                                                               
         BAS   RE,NEXTEL           GET NEXT DATE ELEMENT                        
         BE    CEDI0960                                                         
         B     MISSERR                                                          
*                                                                               
*                                  ENOUGH SPOTS IN WEEK?                        
CEDI1040 SR    R3,R3                                                            
         IC    R3,9(R6)            NO. SPOTS PER WEEK                           
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    CEDI1080                                                         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   CEDI1160                                                         
*                                  MISSED SPOT IN THIS WEEK?                    
CEDI1080 EQU   *                                                                
         GOTO1 DATCON,(R1),(3,2(R6)),WORK+38                                    
*                                  WITHIN RELEVANT WEEK?                        
         CLC   WORK+38(6),WORK+20  BEFORE START OF WEEK?                        
         BL    CEDI1120            YES - GO TO NEXT ELEMENT                     
         CLC   WORK+38(6),WORK+32  NO  - AFTER  END   OF WEEK?                  
         BH    CEDI1160            YES - GO TO NEXT ELEMENT                     
         SR    RE,RE               NO  - WITHIN WEEK                            
         IC    RE,6(R6)            NO. OF MISSED SPOTS                          
         SR    R3,RE               SUBTRACT FROM NUMBER PER WEEK                
CEDI1120 ZIC   R4,1(R6)            BUMP TO NEXT ELEM                            
         AR    R6,R4                                                            
         CLI   0(R6),6             MISSED ELEM?                                 
         BE    CEDI1080            YES - CHECK ITS DATES                        
         CLI   0(R6),7             NO  - CREDIT ELEMENT?                        
         BE    CEDI1080            YES - CHECK ITS DATES                        
*                                  THERE SHOULD BE NO CREDIT ELEMENTS           
CEDI1160 EQU   *                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'66'        NOW CHECK M/G OFFER CONTROLS                 
         BAS   RE,GETEL                                                         
         BNE   CEDI1280                                                         
*                                  MISSED SPOT IN THIS WEEK?                    
CEDI1200 EQU   *                                                                
         CLC   RMKGKGRP,RBMGMSGD-RBMGMSEL(R6)                                   
         BE    CEDI1220            DON'T CLEAN UP IF 66 POINTS TO THE           
*                                  MAKEGOOD WE ARE CREATING                     
*                                                                               
         GOTOR CLEANUP             CLEAN UP BOGUS 66 ELEMENT                    
         BNE   CEDI1250                                                         
CEDI1220 GOTO1 DATCON,(R1),(3,7(R6)),WORK+38                                    
*                                  WITHIN RELEVANT WEEK?                        
         CLC   WORK+38(6),WORK+20  BEFORE START OF WEEK?                        
         BL    CEDI1240            YES - GO TO NEXT ELEMENT                     
         CLC   WORK+38(6),WORK+32  NO  - AFTER  END   OF WEEK?                  
         BH    CEDI1280            YES - GO TO NEXT ELEMENT                     
         ZIC   RE,6(R6)            RETRIEVE MULTI-LINE COUNTER                  
         SLL   RE,28               DROP HI-ORDER NYBBLE                         
         SRL   RE,28               MOVE LO-ORDER NYBBLE BACK                    
*                                                                               
*   TEST                                                                        
*        CLI   LINECNTR,2                                                       
*        BNE   *+6                 NO                                           
*        DC    H'0'                DUMP AT THIS POINT                           
*   TEST END                                                                    
*                                                                               
         CH    RE,=H'1'            LINE-COUNT > 1?                              
         BH    CEDI1240            YES - # SPOTS SHOULD ONLY BE                 
*                                     SUB'ED FOR 1ST LINE OF OFFER              
         SR    RE,RE               NO  - WITHIN WEEK                            
         IC    RE,11(R6)           NO. OF MISSED SPOTS                          
         SR    R3,RE               SUBTRACT FROM NUMBER PER WEEK                
CEDI1240 ZIC   R4,1(R6)            BUMP TO NEXT ELEM                            
         AR    R6,R4                                                            
CEDI1250 DS    0H                                                               
         CLI   0(R6),X'66'         M/G MISSED ELEM?                             
         BE    CEDI1200            YES - CHECK ITS DATES                        
*                                  THERE SHOULD BE NO CREDIT ELEMENTS           
CEDI1280 EQU   *                                                                
         ST    R3,DMCB             SPOT CTR                                     
*                                                                               
         CLI   LINECNTR,1          FIRST LINE?                                  
         BH    CEDI1320            NO  - ONLY SUB SPOTS FOR 1ST LINE            
*                                     OF OFFER                                  
         LA    R3,TOTMISD          INVALID MISSED DATE                          
         L     R6,DMCB             SPOT CTR                                     
         SR    RE,RE                                                            
         ZIC   RE,WKD.RMKGMGSP     NO. OF MISSED SPOTS                          
         SR    R6,RE               ANY LEFT IN WEEK?                            
         BM    ERROR                                                            
*                                                                               
TOTMISD  EQU   457                 TOTAL MISSED SPOTS EXCEEDS SPTS/WK           
         EJECT                                                                  
*                                                                               
*                                  ADD MISSED REF TO MISSED LINE                
CEDI1320 MVC   ELTBILD(2),=X'660C'                                              
*                                  ELEMENT CODE + LENGTH                        
         MVC   ELTBILD+2(2),RMKGKGRP                                            
*                                  GROUP CODE                                   
         MVC   ELTBILD+4(2),RMKGKMLN                                            
*                                  MG: MISSED BUY#/MG BUY#                      
         MVC   ELTBILD+6(1),LINECNTR                                            
*                                  MG: LINE COUNTER W/IN OFFER                  
         CLI   MGOMOPT,C'A'        'CHOICE' OF LINES REQUESTED?                 
         BE    CEDI1360            NO                                           
         OI    ELTBILD+6,X'10'     YES - SET 'CHOICE' INDICATOR                 
CEDI1360 EQU   *                                                                
         MVC   ELTBILD+7(3),WKD.RMKGMGD1                                        
*                                  MISSED DATE                                  
         MVC   ELTBILD+10(1),RMKGKLIN                                           
*                                  M/G LINE NUMBER                              
         MVC   ELTBILD+11(1),WKD.RMKGMGSP                                       
*                                  NO. OF MISSED SPOTS                          
*                                                                               
         LA    R3,IOAREA+34                                                     
         SR    R4,R4                                                            
CEDI1400 IC    R4,1(R3)            NEXT ELEM                                    
         AR    R3,R4                                                            
         CLI   0(R3),0             LAST?                                        
         BE    CEDI1440                                                         
         CLI   0(R3),X'66'         MG MISSED SPOT ELEMENT?                      
         BNE   CEDI1400            NO  - IGNORE IT                              
         CLC   ELTBILD+2(8),2(R3)                                               
*                                  CHECK GROUP CODE,BUY#/SUB#/CTR               
         BH    CEDI1400                                                         
         BNE   CEDI1440                                                         
*                                                                               
*        DC    H'0'                MISSED ELEM ALREADY IN MISSED LINE           
*                                                                               
         LA    R6,RMKGREC          DON'T DIE, JUST MERGE THE DUPLICATE          
         MVI   ELCODE,X'05'        MKG ELTS IN BOTH BUY REC AND MKG REC         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(RMKGMGSP-RMKGMGEL,R6),WKD.RMKGMGCD                             
         BE    CEDI1420                                                         
         BAS   RE,NEXTEL                                                        
         BNE   CEDI1440                                                         
*                                                                               
CEDI1420 DS    0H                                                               
         USING RBMGMSEL,R3                                                      
         ZIC   R0,9(R6)                                                         
         ZIC   R4,ELTBILD+11                                                    
         AR    R0,R4                                                            
         STC   R0,9(R6)                                                         
*                                                                               
         ZIC   R0,RBMGMSSP         INCREASE SPOTS                               
         ZIC   R4,ELTBILD+11                                                    
         AR    R0,R4                                                            
         STC   R0,RBMGMSSP                                                      
         B     CEDI1445                                                         
         DROP  R3                                                               
*                                  ADD MISSED ELEM                              
*                                                                               
CEDI1440 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,IOAREA),(0,ELTBILD)             
*                                  SAVE NEW MISSED LINE                         
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK                                       
         DROP  WKD                                                              
*                                                                               
CEDI1445 EQU   *                                                                
         L     RE,AMISSED          RESET A(WITHIN MISSED DATE STRING)           
         CLI   ANOTHER1,1          MORE DATES ON MISSED LINE?                   
         BE    CEDI0400            YES - GO BACK AND PROCESS IT                 
*                                                                               
CEDI1450 EQU   *                                                                
         LA    RE,MGOMDYSH                                                      
         A     RE,OFFRDISP                                                      
         CLC   =C'PREEMPT',8(RE)                                                
         BNE   CEDI1460                                                         
*        GOTO1 =V(PROCPREM),DMCB,(RC),RR=Y                                      
*                                                                               
CEDI1460 EQU   *                                                                
         L     R7,ASPULAR                                                       
         A     R7,=F'4000'         BUMP TO ALTERNATE STORAGE                    
         GOTO1 VMOVEREC,(R1),IOAREA,(R7)                                        
*                                  SAVE ORIGINAL BUY RECORD IN                  
*                                     SPOOLAR AFTER THREE MG                    
*                                     RECORD STORAGE AREAS                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         MVC   RMKGKPLN,=3X'FF'    DEFAULT                                      
*                                                                               
         TM    TWAMKGFG,X'08'      BONUS??                                      
         BO    CEDI1470            NO ORIGINAL BUY NEEDED                       
         DROP  RF                                                               
*                                                                               
         MVC   RMKGKPLN,KEY+22     MOVE MISSED PLAN CODE FROM                   
*                                     ORIGINAL BUY KEY                          
CEDI1470 EQU   *                                                                
         MVC   RMKGKMLN,WORK+2     MISSED LINE NUMBER                           
*                                                                               
         GOTO1 =A(CHECKPRE),DMCB,(RC),RR=Y                                      
*                                                                               
         CLC   =C'BONUS',MGOMMSD                                                
         BE    ENDEDIT                                                          
*                                  ADD MAKE-GOOD ELEMENT                        
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         GOTO1 VADDELEM,DMCB,RMKGREC,TWAELEM                                    
         DROP  RF                                                               
*                                  ADD COMMENT MG=                              
*                                                                               
         TITLE 'REPPAK EDIT END'                                                
ENDEDIT  DS    0H                                                               
***      OI    CONBACTH+4,X'20'                                                 
***      OI    CONBNUMH+4,X'20'                                                 
         LA    R2,CONCACTH         CURSOR                                       
*                                                                               
*                                  PUT RMKGNW IN 03 (EFF DATE) ELTS             
*                                     IF NO OVERRIDE AND IT'S NOT               
*                                     A 'DARE' ORDER (CAN'T CHANGE              
*                                     EFF DATES IN DARE ORDERS)                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ELEMENT?                                
         BAS   RE,GETEL                                                         
         BNE   ENED0040            NO  - PROCEED                                
         LA    R6,RCONREC          YES - CHECK FOR 'CONFIRMED'                  
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,GETEL                                                         
         BNE   ENED0120            NOT FOUND - NEW DARE ORDER.                  
         TM    RCONCONF-RCONXEL(R6),X'E0'                                       
*                                  ANY 'CONFIRMED' FLAG ON?                     
         BZ    ENED0120            NO  - TREAT AS DARE ORDER                    
*                                  YES - UPDATE # SPOTS/WEEK                    
*                                                                               
*   NOTE - THIS TEST MAY HAVE TO BE CHANGED FOR ORDERS WHICH ARE                
*        CONFIRMED/UNCONFIRMED FOR DARE.                                        
*                                                                               
ENED0040 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   ENED0120                                                         
ENED0080 DS    0H                                                               
         TM    8(R6),X'01'                                                      
         BO    *+10                                                             
         MVC   9(1,R6),RMKGNW                                                   
         BAS   RE,NEXTEL                                                        
         BE    ENED0080                                                         
ENED0120 EQU   *                                                                
         GOTO1 =A(NEW10ELT),RR=Y   ADD X'10' CONTROL DESC ELT                   
         EJECT                                                                  
ENED0160 EQU   *                   ADD BUY/UPDATE CHANGE INDICATORS             
         MVC   RMKGCREA,TODAY      CREATION DATE                                
         MVC   RMKGKMOD,RCONMOD                                                 
         TM    RCONMODR+1,X'C0'    IF ACE/GRAPHNET SKIP THIS CODE               
         BNZ   ENED0200            (X'FF' IS VALID MOD #)                       
         SPACE 1                                                                
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RMKGKMOD,0                                                       
ENED0200 MVC   RMKGCHGI,=C'A '     ADD IND                                      
         CLI   RCONKSTA+4,C'F'     RADIO COMMENT - MUST KEEP                    
         BE    ENED0240                                                         
         CLI   RCONKSTA+4,C'A'     RADIO COMMENT - MUST KEEP                    
         BE    ENED0240                                                         
         CLI   RCONKSTA+4,C'C'     RADIO COMMENT - MUST KEEP                    
         BE    ENED0240                                                         
*                                                                               
ENED0240 DS    0H                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ENED0400                                                         
         TM    STAT2,X'80'         BUY CHANGED-UP VER/MOD                       
         BO    ENED0280                                                         
****>>>  OI    BYTE4,X'02'         NO, BUT STILL DO PUTREC                      
         B     ENED0400                                                         
*        SPACE 1                                                                
ENED0280 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ENED0320                                                         
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
         DS    H'0'                                                             
ENED0320 EQU   *                                                                
         ZIC   R1,RCONSRV          REP VERSION NUMBER                           
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    ENED0360                                                         
         STC   R1,RCONSRV                                                       
*                                                                               
         ZIC   R1,RCONSRV          RESTORE R1                                   
*        OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
ENED0360 STC   R1,RMKGVER          STORE REP VERSION NO. IN BUY                 
*                                                                               
ENED0400 EQU   *                                                                
         L     R2,MGSTOR#0         SET A(MG RECORD STORAGE)                     
         GOTO1 VMOVEREC,DMCB,RMKGREC,(R2)                                       
*                                  MOVE MG REC TO STORAGE                       
         LA    R2,1000(R2)         BUMP TO NEXT STORAGE AREA                    
         ST    R2,MGSTOR#0         SAVE IT                                      
*                                                                               
*   'ORIGINAL' WILL NOW BECOME THE NEW MAKEGOOD RECORD.  IN THIS                
*        MANNER, THE 'NEXT' LINE ALWAYS USES THE PREVIOUS LINE'S                
*        VALUES TO FILL IN MISSING FIELDS.                                      
*                                                                               
         L     R2,AORIGBUY         SET A(ORIGINAL BUY STORAGE)                  
         GOTO1 VMOVEREC,DMCB,RMKGREC,(R2)                                       
*                                  SET MG REC TO BE NEW 'ORIGINAL'              
ENED0440 EQU   *                                                                
         LA    R2,MGOMDYSH         A(1ST LINE OF MG OFFER)                      
         L     RF,OFFRDISP         BUMP TO NEXT SCREENLINE                      
         LA    RF,MGOMDY2H-MGOMDYSH(RF)                                         
         ST    RF,OFFRDISP         STORE IT BACK                                
         LA    RE,MGOLAST          COMPARE FOR END OF SCREEN                    
         AR    R2,RF               ADD DISPLACEMENT TO A(1ST LINE)              
         CR    R2,RE               END OF SCREEN REACHED?                       
         BNL   ENED0560            YES - LAST LINE DONE - WRAP IT UP            
         LA    RF,5                FIELD LOOP                                   
         CLC   8(2,R2),=C'**'      '**' IN FIRST FIELD OF LINE?                 
         BE    ENED0440            YES - REQUEST TO DELETE LINE                 
ENED0480 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BNZ   ENED0520            YES - REINITIALIZE WHATEVER                  
         ZIC   RE,0(R2)            NO  - BUMP TO NEXT FIELD                     
         AR    R2,RE                                                            
         BCT   RF,ENED0480         GO BACK AND CHECK NEXT FIELD                 
         B     ENED0440            CHECK FOR ANOTHER LINE                       
ENED0520 EQU   *                                                                
         XCEFL RMKGREC+25,975      CLEAR THE MAKEGOOD RECORD OUT                
*                                     AFTER MASTER LINE # (1ST 25)              
         B     BUED0170            GO BACK AND VALIDATE NEXT LINE               
ENED0560 EQU   *                                                                
         GOTO1 =A(CKZEROSP),RR=Y                                                
*                                                                               
         SR    R4,R4               CLEAR A COUNTER                              
         LA    R3,3                SET LOOP CONTROL                             
         L     R2,MGSTOR#1         SET A(1ST MAKEGOOD AREA)                     
         LA    R6,SV20ELTS         SET A(1ST X'20' ELEMENT STORE)               
*        GOTO1 DUMPMGS             CLEAR OLD MAKEGOOD RECORDS                   
         GOTO1 =A(DUMPMGS),DMCB,(RC),RR=Y                                       
*                                     AND SAVE X'20' ELEMENTS                   
         XC    SAVMGKEY,SAVMGKEY   CLEAR KEY SAVE AREA                          
ENED0600 EQU   *                                                                
         OC    0(34,R2),0(R2)      ANYTHING IN AREA?                            
         BZ    ENED0760            NO  - SET FOR REDISPLAY                      
         OC    0(10,R6),0(R6)      ANY ORIGINAL X'20' ELEMENT?                  
         BZ    ENED0640            NO  - LEAVE NEW ONE                          
         GOTO1 VDELELEM,DMCB,(X'20',(R2))                                       
*                                  DROP X'20' STATUS CONTROL ELT                
         GOTO1 VADDELEM,DMCB,(R2),(R6)                                          
*                                  INSERT OLD X'20' ELT INTO RECORD             
ENED0640 EQU   *                                                                
         CLI   CHOYSCTR,0          IS THIS A 'CHOICE' PASS?                     
         BE    ENED0660            NO                                           
         ZIC   RF,CHOYSCTR         YES - RETRIEVE CHOYSCTR                      
         BCTR  RF,0                DECREMENT COUNTER BY 1                       
         STC   RF,CHOYSCTR         PUT IT BACK                                  
         LTR   RF,RF               ANYTHING LEFT?                               
         BNZ   ENED0660            YES                                          
         GOTO1 SETCHOYS            NO  - THIS IS THE CHOICE                     
ENED0660 EQU   *                                                                
         L     RF,MGSTOR#2         CHECK FOR MULTIPLE LINES                     
         OC    0(34,RF),0(RF)      ANYTHING IN SECOND AREA?                     
         BZ    ENED0680            NO                                           
         LA    R4,1(R4)            YES - BUMP COUNTER                           
         STC   R4,RMKGKRTY-RMKGREC(R2)                                          
*                                  INSERT COUNTER INTO KEY                      
         CLI   MGOMOPT,C'A'        CHOICE?                                      
         BE    ENED0680            NO                                           
         OI    RMKGKRTY-RMKGREC(R2),X'10'                                       
*                                  YES - SET HIGH-NYBBLE FLAG                   
ENED0680 EQU   *                                                                
         OC    SAVMGKEY,SAVMGKEY   ANY KEY SAVED?                               
         BNZ   ENED0720            YES                                          
         MVC   SAVMGKEY,0(R2)      NO  - SAVE KEY                               
*                                                                               
ENED0720 EQU   *                                                                
         USING RMKGREC,R2                                                       
         NI    RMKGRTS,X'FF'-X'20'                                              
         CLC   =C'BONUS',MGOMMSD                                                
         BNE   ENED0730                                                         
         OI    RMKGRTS,X'20'       SET MKGD OFFER AS BONUS                      
         DROP  R2                                                               
*                                                                               
ENED0730 EQU   *                                                                
         GOTO1 RECWRITE,DMCB,(R2),0                                             
*                                  ADD/REWRITE MAKEGOOD RECORD                  
         LA    R2,1000(R2)         BUMP TO NEXT STORAGE AREA                    
         BCT   R3,ENED0600         GO BACK AND CHECK IT                         
ENED0760 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVMGKEY    GET DISK ADDR OF 1ST NEW RECORD              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   ENED0800            NO  - MUST BE A NON-UPDATE TEST              
         MVI   REDISPIT,1          SET REDISPLAY FLAG TO 1                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   TWAMKGD2,KEY+28     SAVE D/A                                     
         DROP  RF                                                               
*                                                                               
*                                  REWRITE ORIGINAL BUYLINE                     
ENED0800 EQU   *                   HANDLE GROUP COMMENT                         
         GOTO1 =A(GRPCOMNT),DMCB,(RC),RR=Y                                      
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'      BONUS DOES NOT HAVE ORIGINAL BUY REC         
         BZ    ENED0803                                                         
         NI    TWAMKGFG,X'FF'-X'08'                                             
         B     ENED0805            BONUS ALL DONE, RESET FLAG                   
         DROP  RF                                                               
*                                                                               
ENED0803 DS    0H                                                               
         L     R7,ASPULAR          CHECK ALTERNATE STORAGE FOR                  
*                                     ORIGINAL BUY RECORD                       
         A     R7,=F'4000'         BUMP TO ALTERNATE STORAGE                    
         GOTO1 RECWRITE,DMCB,(R7),0                                             
         EJECT                                                                  
*                                                                               
* UPDATE X'21' MAKEGOOD OFFER ELEMENT                                           
*                                                                               
ENED0805 DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
* UPDATE MAKEGOOD OFFER WORK-IN-PROGRESS                                        
*                                                                               
         LA    R6,RCONREC                                                       
         USING RCONMGEL,R6                                                      
         MVI   ELCODE,X'21'        MG OFFER ELEMENT PRESENT?                    
         BAS   RE,GETEL                                                         
         BE    ENED0810                                                         
*                                                                               
         LA    R6,WORK2            NO, ADD ONE                                  
         XC    WORK2,WORK2                                                      
         MVI   RCONMGCD,X'21'                                                   
         MVI   RCONMGLN,RCONXMGQ                                                
         MVI   RCONMGFG,X'80'                                                   
*                                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
ENED0810 DS    0H                                                               
         OI    RCONMGFG,X'80'      SET WIP (WORK-IN-PROGRESS)                   
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         DROP  R6                                                               
*                                                                               
         LA    R3,121                                                           
         CLC   CONCACT(3),=C'MGO'  'OFFER' REQUEST?                             
         BE    INFEXIT                                                          
         LA    R3,122                                                           
         B     INFEXIT                                                          
*                                                                               
*&&DO                                                                           
         XC    DUB,DUB                                                          
         MVC   DUB(2),=H'121'      SET 'OFFER ADDED' MESSAGE                    
         CLC   CONCACT(3),=C'MGO'  'OFFER' REQUEST?                             
         BE    EXXMOD                                                           
         MVC   DUB(2),=H'122'      SET 'OFFER CHANGED' MESSAGE                  
         B     EXXMOD                                                           
*&&                                                                             
*                                                                               
MISSERR  MVI   TEMP,X'FF'                                                       
*                                                                               
         LA    R3,MG2ERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
INFEXIT  DS    0H                                                               
         GOTO1 GETTXT,DMCB,(R3),0,(C'I',0),0,0,0                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   SETCHOYS:  FIND THE X'20' ELEMENT IN THE RECORD, AND TURN ON                
*        THE CHOICE BIT.                                                        
*                                                                               
SETCHOYS NTR1                                                                   
         LR    R6,R2               R2 --> RECORD IN BUFFER                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND - ???                              
         OI    RMKGSTCH-RMKGSTEL(R6),X'01'                                      
*                                  SET 'CHOICE' FLAG                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* GET CURRENT TIME                                                              
* P1 HAS A(2-BYTE) HHMM DESTINATION                                             
*                                                                               
GETTIME  NTR1                                                                   
         L     R4,0(R1)                                                         
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         STCM  R0,4,1(R4)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R4)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R4),2,0                                      
         B     EXXMOD                                                           
*                                                                               
*                                                                               
*   RECWRITE:  CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*        ADDS, UPDATES PASSIVES IF FROM GROUP COMMENT UPDATE                    
*        P2  =  1:  GROUP COMMENT UPDATE                                        
*                                                                               
RECWRITE NTR1                                                                   
         L     R3,0(R1)            RESET A(IOAREA)                              
         L     R7,4(R1)            SAVE GROUP COMMENT FLAG                      
         MVC   KEY,0(R3)           SET KEY FROM NEW RECORD                      
*                                                                               
         CLI   0(R3),X'0B'                                                      
         BNE   RECW0010                                                         
         CLC   27(2,R3),=AL2(972)  MAX SIZE FOR BUYS                            
         BNH   RECW0010                                                         
         LA    R3,339              REC FULL, CHANGE NOT PROCESSED               
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
* CHECK FOR DUPE KEY ON FILE INSTEAD OF USING ADDREC WITH RETURN                
* ERROR. THE PROBLEM IS THAT ADDREC STILL MAKES IT TO THE RECOVERY              
* FILE WHICH SCREWS UP THE ESS DB2 PROCESS                                      
*                                                                               
RECW0010 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    RECW0100            YES                                          
         MVC   KEY,0(R3)           SET KEY FROM NEW RECORD                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,            X        
               (R3),MGWORK                                                      
*                                  ATTEMPT TO ADD NEW RECORD                    
         CLI   DMCB+8,0            ERROR RETURN?                                
*        BNZ   RECW0100            YES - CHECK FOR DUPE KEY                     
         BZ    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN NOW                      
*                                                                               
*                                  SUCCESSFUL ADD:  NOW ADD PASSIVES            
         LTR   R7,R7               GROUP COMMENT IN PROGRESS?                   
         BZ    RECWX               NO  - NO FURTHER PROCESSING                  
         MVC   FULL,KEY            SAVE D/A OF ADDREC                           
         BAS   RE,PASSADDR         ADDREC PASSIVE ADDING                        
         B     RECWX               EXIT                                         
RECW0100 EQU   *                                                                
*&&DO                                                                           
         CLI   DMCB+8,X'20'        YES - DUPE KEY?                              
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - CAN'T PROCESS: DUMP                    
*                                                                               
*                                  RESTORE THE KEY/OVERWRITE RECORD             
         MVC   KEY,0(R3)           SET UP KEY AGAIN                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    *+6                 YES                                          
         DC    H'0'                ???                                          
*&&                                                                             
*                                                                               
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         LA    R4,RBUYREC          ESTABLISH ADDITIONAL IOAREA                  
*                                     USE IO3 FOR IT                            
         L     R4,AIO3                                                          
         MVC   FULL,KEY+28         SET D/A OF RECORD                            
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,         X        
               (R4),MGWORK                                                      
*                                  RETRIEVE INTO ALT IO AREA                    
         NI    29(R3),X'FF'-X'80'                                               
*                                  RESTORE CONTROL IN RECORD IN                 
*                                     ORIGINAL IOAREA                           
*                                                                               
         CLI   0(R3),X'0B'                                                      
         BNE   RECW0200                                                         
         CLC   27(2,R3),=AL2(972)  MAX SIZE FOR BUYS                            
         BNH   RECW0200                                                         
         LA    R3,339              REC FULL, CHANGE NOT PROCESSED               
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
RECW0200 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,         X        
               (R3),MGWORK                                                      
*                                  REWRITE FROM ORIG IOAREA                     
         GOTO1 DATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY              
*                                  REWRITE CLEARED KEY                          
         LTR   R7,R7               GROUP COMMENT IN PROGRESS?                   
         BZ    RECWX               NO  - NO FURTHER PROCESSING                  
         BAS   RE,PASSREWR         REWRITE PASSIVE KEYS                         
         B     RECWX                                                            
RECWX EQU      *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   PASSREWR:  RETRIEVE AND REWRITE PASSIVE KEYS                                
*   R3 HAS HEADER RECORD                                                        
*                                                                               
PASSREWR NTR1                                                                   
*                                                                               
*   DELETE OLD PASSIVE POINTERS                                                 
*                                                                               
         CLC   =C'MGO',CONACT      SKIP IF ACTION MGO                           
         BE    PASA0010                                                         
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),10(R3)   **ALL MOVES FROM REC IN STORAGE**            
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),6(R3)      MOVE REP CODE TO NEW POSITION                
         MVC   KEY+11(3),SAVSAL    INSERT ORIGINAL S/P  INTO KEY                
         MVC   KEY+11(2),SAVTEM    INSERT ORIGINAL TEAM INTO KEY                
PAWR0010 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0020            YES -                                        
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0040            PROCESS NEXT KEY                             
PAWR0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
PAWR0040 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY ALREADY DONE?                     
         BE    PAWR0100            YES                                          
         MVI   KEY,X'A1'           NO  - SET TO NEXT PASSIVE KEY                
         MVC   KEY+11(2),SAVTEM    INSERT TEAM INTO KEY HIGH                    
         MVC   KEY+13(3),SAVSAL    INSERT SAL  INTO KEY LOW                     
         B     PAWR0010            GO BACK AND DO NEXT KEY                      
*                                                                               
PAWR0100 DS    0H                                                               
*                                                                               
*        DELETE A0 PASSIVES                                                     
*                                                                               
PAWR0101 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         USING RMKGKEY,R3          ESTABLISH MAKEGOOD HEADER KEY                
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         MVC   27(1,R5),29(R2)     STATUS                                       
*                                                                               
         MVC   RMGSPKEY.RMGSPSAL,SAVSAL   INSERT S/P INTO KEY                   
         MVC   RMGSPKEY.RMGSPADV,SAVADV   ADVERTISER CODE                       
         MVC   RMGSPKEY.RMGSPDAT,SAVDAT   FIRST OFFERED DATE                    
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   SET WIP STATUS                        
         MVC   RMGSPKEY.RMGSPSTT,SAVSTT   OFFER STATUS                          
         MVC   RMGSPKEY.RMGSPDST,SAVDST   DARE  STATUS                          
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0520            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0540            PROCESS NEXT KEY                             
*                                                                               
PAWR0520 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
*                                                                               
PAWR0540 EQU   *                                                                
*                                                                               
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SALESPER                      
         BZ    PAWR0600                                                         
*                                                                               
         MVI   1(R5),X'02'         SET NEXT MG PASSIVE                          
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL INSERT DEV SALESPER                     
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0560            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0580            PROCESS NEXT KEY                             
*                                                                               
PAWR0560 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
*                                                                               
PAWR0580 EQU   *                                                                
*                                                                               
PAWR0600 DS    0H                                                               
         B     PASA0010                                                         
*                                                                               
*   PASSADDR:  ADD PASSIVES FOR GROUP COMMENT RECORDS                           
*                                                                               
PASSADDR NTR1                                                                   
*                                                                               
*   RESTRUCTURE THE GROUP COMMENT CODE TO PASSIVE LAYOUT                        
*                                                                               
PASA0010 EQU   *                                                                
         CLC   =C'MGX',CONCACT     IS ACTION MAKEGOOD DELETE? (MGX)             
         BE    PASAX               DON'T ADD PASSIVE KEYS                       
         XC    KEY,KEY             CLEAR NEW KEY                                
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),10(R3)   **ALL MOVES FROM REC IN STORAGE**            
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),6(R3)      MOVE REP CODE TO NEW POSITION                
         LA    RF,RMKGELEM-RMKGREC(R3)                                          
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
PASA0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASAX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0040            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0020            GO BACK FOR NEXT                             
PASA0040 EQU   *                                                                
         MVC   KEY+11(3),RMKGXSAL-RMKGXEL(RF)                                   
*                                  INSERT SALESPERSON CODE                      
         MVC   KEY+14(2),RMKGXTEM-RMKGXEL(RF)                                   
*                                  INSERT S/P TEAM    CODE                      
PASA0060 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         MVC   KEY+28(4),FULL      INSERT D/A                                   
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0080            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0100            GO PROCESS NEXT KEY (IF ANY)                 
PASA0080 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VADD                ADD NEW KEY                                  
PASA0100 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY PROCESSED?                        
         BE    PASA0120            YES - BOTH KEYS DONE                         
         MVI   KEY,X'A1'           NO  - SET SECOND KEY TYPE                    
         MVC   WORK(3),KEY+11      SAVE S/P  COMPONENT OF KEY                   
         MVC   KEY+11(2),KEY+14    SLIDE TEAM UP IN KEY                         
         MVC   KEY+13(3),WORK INSERT S/P BACK INTO KEY                          
         B     PASA0060            GO BACK AND PROCESS                          
PASA0120 EQU   *                                                                
*                                                                               
*        ADD MAKEGOOD A001, A002 PASSIVE KEYS                                   
*                                                                               
         XC    KEY,KEY             CLEAR NEW KEY                                
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         USING RMKGKEY,R3          ESTABLISH MAKEGOOD HEADER KEY                
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         LA    RF,RMKGELEM-RMKGREC(R3)                                          
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
         USING RMKGSDEM,RF         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   RMGSPKEY.RMGSPDAT,RMKGFOFD RMGSPKEY.RMGSPE 1ST OFFERED           
         MVC   RMGSPKEY.RMGSPWIP,RMKGSFG2 WIP STATUS                            
         MVC   RMGSPKEY.RMGSPSTT,RMKGSCST OFFER STATUS                          
         NI    RMGSPKEY.RMGSPSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                 
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    RMGSPKEY.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   RMGSPKEY.RMGSPDST,RMKGSFG1 DARE STATUS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASAX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0240            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0220            GO BACK FOR NEXT                             
PASA0240 EQU   *                                                                
         USING RMKGXEL,RF                                                       
         MVC   RMGSPKEY.RMGSPSAL,RMKGXSAL SALESPERSON                           
         MVC   RMGSPKEY.RMGSPADV,RMKGXADV ADVERTISER                            
         MVC   SAVDSL,RMKGXDSP     SAVE DEVELOPMENTAL SALESPERSON               
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0260 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         MVC   KEY+28(4),FULL      INSERT D/A                                   
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0280            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0300            GO PROCESS NEXT KEY (IF ANY)                 
PASA0280 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VADD                ADD NEW KEY                                  
PASA0300 EQU   *                                                                
         CLI   KEY+1,X'02'         SECOND KEY PROCESSED?                        
         BE    PASA0320            YES - BOTH KEYS DONE                         
         MVI   KEY+1,X'02'         NO  - SET SECOND KEY TYPE                    
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL   USE DEV SALESPERSON                   
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SAL                           
         BZ    PASA0320                                                         
         B     PASA0260            GO BACK AND PROCESS                          
PASA0320 EQU   *                                                                
PASAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
CHECK    EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         DROP  R3,RMGSPKEY                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*   LOCAL VALUES                                                                
*                                                                               
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRITE  DC    CL8'DMWRT   '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
         EJECT                                                                  
* ROUTINE TO ADD CODE TO RMKGCHGI  (CODE IN BYTE)                               
ADDCODE  NTR1                                                                   
         CLI   BYTE,0                                                           
         BE    ADDCXIT                                                          
         MVC   RMKGKMOD,RCONMOD    CONTRACT MOD NUMBER                          
         SPACE 1                                                                
*                                                                               
* FOR ACE/GRAPHNET -  THE VERSION, NOT THE DAY, IS WHAT MATTERS                 
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDC50                                                           
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWAACCS,C'$'        STATION ONLY HAS 1 CHG CODE, SO IT           
         BE    ADDC75              DOESN'T MATTER IF UPDATED ALREADY            
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ADDC10                                                           
         DC    H'0'                                                             
ADDC10   TM    4(R6),X'20'         REP VERSION NOT ADVANCED                     
         BNO   ADDC30                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
         SPACE 1                                                                
ADDC30   CLC   5(1,R6),RMKGVER     COMPARE CONTRACT TO BUY VERSION              
         BE    ADDC35                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
ADDC35   CLI   RMKGCHGI,C'A'       IF ADDED THIS VERSION,                       
         BE    ADDCXIT             DON'T CARE ABOUT OTHER CHANGES               
         B     ADDC100                                                          
         SPACE 1                                                                
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
         SPACE 1                                                                
ADDC50   CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RMKGKMOD,0                                                       
         CLC   TODAY,RMKGCHGD                                                   
         BE    ADDC100                                                          
ADDC75   MVC   RMKGCHGD,TODAY                                                   
         MVC   RMKGCHGI(1),BYTE    CHANGE CODE                                  
         MVI   RMKGCHGI+1,C' '                                                  
*                                                                               
ADDCXIT  B     EXXMOD                                                           
* BUY ALREADY CHANGED TODAY                                                     
ADDC100  CLI   RMKGCHGI,C' '                                                    
         BNE   *+14                                                             
         MVC   RMKGCHGI(1),BYTE    FIRST                                        
         B     ADDCXIT                                                          
         CLI   RMKGCHGI,C'*'                                                    
         BE    ADDCXIT                                                          
         CLC   RMKGCHGI(1),BYTE                                                 
         BE    ADDCXIT                                                          
         CLI   RMKGCHGI+1,C' '                                                  
         BNE   *+14                                                             
         MVC   RMKGCHGI+1(1),BYTE  2D                                           
         B     ADDCXIT                                                          
         CLC   RMKGCHGI+1(1),BYTE                                               
         BE    ADDCXIT                                                          
         MVC   RMKGCHGI,=C'* '     MORE THAN 2                                  
         B     ADDCXIT                                                          
         EJECT                                                                  
*                                                                               
*   BUILD X'04' ELEMENT (MG=DATE-LIN#(#SPOTS) FORMAT)                           
*                                                                               
BILDX04  NTR1                                                                   
         L     R7,AELTBILD         SET POINTER IN BUILD AREA                    
         CLI   X04FLAG,0           1ST DATE IN STRING?                          
         BE    BILX0020            YES                                          
         MVI   0(R7),C','          NO  - INSERT SEPARATOR                       
         LA    R7,1(R7)            INCREMENT ADDR                               
BILX0020 EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(X'83',WORK+3),(4,(R7))                              
         PRINT NOGEN                                                            
         ZIC   RF,DMCB+4           GET L(DATE OUTPUT)                           
         AR    R7,RF               ADD TO PTR TO ELTBILD                        
         CLI   X04FLAG,0           1ST DATE IN STRING?                          
         BNE   BILX0040            NO                                           
         MVI   X04FLAG,1           YES - SET OFF                                
         MVI   0(R7),C'-'          INSERT SEPARATOR                             
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         EDIT  (1,WORK+2),(3,(R7)),ALIGN=LEFT,WRK=ELTBILD+32                    
*                                  INSERT MISSED LINE NUMBER                    
         AR    R7,R0               ADD SIGNIFICANT DIGITS TO ADDRESS            
BILX0040 EQU   *                                                                
         CLI   WORK+9,1            1 SIGNIFICANT SPOT MISSED?                   
         BE    BILX0100            YES                                          
         MVI   0(R7),C'('          NO  - INSERT SPOT # OVERRIDE                 
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         CLI   WORK+9,0            ANY SPOTS HERE?                              
         BNE   BILX0060            YES - INSERT IT INTO LINE                    
         MVI   0(R7),C'0'          NO  - INSERT A ZERO                          
         LA    R0,1                SET LENGTH OF DATA TO 1                      
         B     BILX0080                                                         
BILX0060 EQU   *                                                                
         EDIT  (1,WORK+9),(3,(R7)),ALIGN=LEFT,WRK=ELTBILD+32                    
*                                  INSERT # MISSED SPOTS                        
*                                     CONTINUE TO USE ELTBILD+32 AS             
*                                        WORK AREA FOR EDIT                     
BILX0080 EQU   *                                                                
         AR    R7,R0               ADD SIGNIFICANT DIGITS TO ADDRESS            
         MVI   0(R7),C')'          CLOSE FRAME                                  
         LA    R7,1(R7)                                                         
BILX0100 EQU   *                                                                
         ST    R7,AELTBILD         SAVE A(NEXT SPACE IN STRING)                 
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         LA    RE,TWAELEM          CALCULATE OVERALL LENGTH OF ELT              
         SR    R7,RE                                                            
         STC   R7,TWAELEM+1        INSERT LENGTH INTO ELT                       
         DROP  RF                                                               
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS                        
*          P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                      
*          P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR FOUND.          
*          AN ASTERISK DELIMITS SUB-FIELDS                                      
*                                                                               
* NOTE: FOR DARE ONLY, ORBITS ARE NOT SUPPORTED AT THIS TIME                    
*                                                                               
SCAN     NTR1                                                                   
*                                                                               
         L     R7,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R7)       TOTAL LENGTH OF INPUT                             
         LA    R7,8(R4,R7)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R7          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         B     EXXMOD                                                           
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   FIELD110                                                         
         MVI   4(R1),C'*'                                                       
*                                                                               
         LA    RF,MGOMDTSH         SKIP ORBIT CHECK FOR DATES FIELD             
         A     RF,OFFRDISP                                                      
*        CR    RF,R2                                                            
*        BE    FIELD50                                                          
*                                                                               
         B     FIELD50                                                          
*&&DO                                                                           
         LA    R6,RCONREC          FOR DARE ORDERS, ORBITS ARE NOT              
         MVI   ELCODE,X'1D'        SUPPORTED                                    
         BAS   RE,GETEL                                                         
         BNE   FIELD50                                                          
         USING RCONDREL,R6                                                      
         OC    RCONDRLK,RCONDRLK                                                
         BZ    FIELD50                                                          
         LA    R3,585                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*&&                                                                             
*                                                                               
FIELD110 DS    0H                                                               
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
*                                                                               
GETSTAT  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+22(5),RCONKSTA  INSERT STATION LETTERS                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GEST0100            FOUND                                        
         DC    H'0'                                                             
         DS    0F                                                               
GEST0100 EQU   *                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         B     EXXMOD                                                           
         EJECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTE5D                                                       
*                                                                               
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE SPDARMKGDD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMMYWRK  DS    XL20                                                             
IMSVKEY  DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
* CLEANUP 66 ELEMENT IF THERE IS NO CORRESPONDING MAKE GOOD RECORD              
* EXIT NO = 66 POINTS TO BOGUS RECORD AND HAS BEEN CLEAN UP                     
*                                                                               
T8022A   CSECT                                                                  
*                                                                               
*   DELOFFER:  DROP X'66' ELEMENTS FROM ORIGINAL BUY RECORD, AND                
*        BLOW AWAY THE MAKEGOOD OFFER RECORDS WITH THIS SET.                    
*                                                                               
DELOFFER NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LR    R6,RA                                                            
         AHI   R6,TWAWORKQ                                                      
         USING TWAWORK,R6                                                       
*                                                                               
         LA    R3,576              NO D/A, REC NOT FOUND                        
         OC    TWAMKGDS,TWAMKGDS                                                
         BNZ   DOFF0010                                                         
         TM    TWAMKGFG,X'08'      BONUS??                                      
         BZ    DOFFERR             BONUS DOES NOT NEED PRIMARY BUY REC          
*                                                                               
DOFF0010 EQU   *                                                                
         OC    TWAMKGD2,TWAMKGD2                                                
         BZ    ERROR                                                            
         TM    TWAMKGFG,X'08'      BONUS??                                      
         BO    DOFF0020            BONUS DOES NOT NEED PRIMARY BUY REC          
*                                                                               
         CLC   TWAMKGDS,=F'-2'     MISSING TARGET BUY?                          
         BE    DOFF0020            YES, JUST DELETE OFFER                       
*                                                                               
         MVC   KEY+28(4),TWAMKGDS  SET D/A OF PRIMARY (BUY) REC                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  RETRIEVE ORIGINAL BUYLINE                    
         GOTOR DELOLD66            DROP ORIGINAL BUYLINES                       
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                  REWRITE THE UPDATED RECORD                   
DOFF0020 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),TWAMKGD2  SET D/A OF MG RECORD SET                     
         DROP  R6                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  RETRIEVE INDICATED RECORD                    
*                                                                               
         ZIC   RF,IOAREA+RMKGKRTY-RMKGKEY                                       
*                                  CHECK IF 1ST OF SET                          
         SLL   RF,28               DROP ADD/CHOICE BIT                          
         SRL   RF,28               RESTORE RECORD #                             
         CH    RF,=H'1'            1ST RECORD?                                  
         BNH   DOFF0040            YES                                          
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(27),IOAREA      INSERT KEY FROM RECORD                       
         MVI   KEY+RMKGKRTY-RMKGKEY,0                                           
*                                  NO  - CLEAR RECORD COUNTER                   
         GOTO1 VHIGH                                                            
         CLC   KEY(26),KEYSAVE     RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NO  - ERROR                                  
*                                  YES - RETRIEVE THE RECORD                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
DOFF0040 EQU   *                                                                
         LA    R2,IOAREA           SET A(RECORD) FOR DUMP                       
*        GOTO1 DUMPMGS             DROP RECORDS FOR THIS SET                    
         GOTO1 =A(DUMPMGS),DMCB,(RC),RR=Y                                       
*                                                                               
* IF NO OFFERS ARE LEFT IN THIS GROUP                                           
* GET GROUP COMMENT HEADER RECORD AND DELETE IT                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VHIGH                                                            
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                                                              
         DC    H'0'                HEADER REC MUST BE THERE                     
         DROP  R6                                                               
*                                                                               
* ANY OFFERS IN THIS GROUP?                                                     
*                                                                               
         GOTO1 VSEQ                                                             
*                                                                               
* EXIT IF YES                                                                   
*                                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    DOFFX                                                            
*                                                                               
* REREAD HEADER RECORD AGAIN                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VHIGH                                                            
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                                                              
         DC    H'0'                HEADER REC MUST BE THERE                     
         DROP  R6                                                               
*                                                                               
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         OI    RMKGCNTL,X'80'      MARK RECORD DELETED                          
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 VWRITE                                                           
*                                  REWRITE DELETED KEY                          
DOFFX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DOFFERR  GOTO1 VERROR                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*   DELOLD66:  DROPS X'66' ELEMENTS WHICH MATCH DATE/TIME/BUY#/MG#              
*                                                                               
DELOLD66 NTR1  BASE=*,LABEL=*                                                   
         XC    ELTBILD,ELTBILD     CLEAR WORK AREA                              
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   ELTBILD(2),TWAMKGDT       INSERT GROUP CODE                      
         MVC   ELTBILD+2(1),TWAMKGL#     INSERT ORIGINAL LINE NUMBER            
         MVC   ELTBILD+3(1),TWAMKGML     INSERT MAKEGOOD LINE NUMBER            
*                                                                               
         DROP  RF                                                               
*                                                                               
DOLD0040 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'66',IOAREA),           X        
               (4,ELTBILD),0                                                    
*                                  DELETE ALL X'66' ELEMENTS FOR                
*                                     CODE,ORIG BUY#, MG#                       
DOLD0080 EQU   *                                                                
         B     DOLDX                                                            
CLEANUP  NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         USING IMWORKD,R4               IOAREA = BUY REC                        
         MVC   IMMYWRK,2(R6)            R6-> 66 ELEMENT                         
         MVC   IMSVKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
MKGD     USING RMKGKEY,KEY                                                      
         MVI   MKGD.RMKGKTYP,X'11'                                              
         MVC   MKGD.RMKGKREP,REPALPHA                                           
         MVC   MKGD.RMKGKOFF,RCONKOFF   INSERT OFFICE                           
         MVC   MKGD.RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                    
         LA    R3,IOAREA                                                        
         USING RBUYREC,R3                                                       
         MVC   MKGD.RMKGKCON,RBUYKCON   CONTRACT NUMBER                         
         DROP  R3                                                               
         MVC   MKGD.RMKGKGRP,RBMGMSGD-RBMGMSEL(R6)                              
         DROP  MKGD                                                             
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(21),KEY          GROUP EXIST?                            
         BE    CLUPYES                  YES, DON'T DO CLEAN UP                  
*                                                                               
CLUP0100 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'66',IOAREA),           >        
               (10,IMMYWRK),0                                                   
         MVC   KEY(27),IOAREA                                                   
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IMIO                                                
         GOTO1 VMOVEREC,DMCB,IOAREA,IMIO                                        
         GOTO1 VPUTREC,DMCB,IMIO                                                
*                                                                               
         LR    R5,RB               MAKE SURE R5 IS NOT ZERO                     
         B     CLUPNO                                                           
*                                                                               
CLUPYES  SR    R5,R5                                                            
CLUPNO   LTR   R5,R5                                                            
         MVC   KEY,IMSVKEY                                                      
CLEANUPX DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
DOLDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   CKSTPRY                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
         DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CRDENDDT:  CALCULATE THE END DATE TO BE CREDITED                              
*                                                                               
T8022A   CSECT                                                                  
CRDENDDT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AORIGBUY                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CRDED10  DS    0H                  POINT TO LAST X'02' ELEMENT TO GET           
         LR    R2,R6               END DAY                                      
         BAS   RE,NEXTEL                                                        
         BE    CRDED10                                                          
         LR    R6,R2                                                            
         USING RBUYDYEL,R6                                                      
*                                                                               
WKD      USING RMKGMGEL,WORK                                                    
         GOTO1 DATCON,DMCB,(3,WKD.RMKGMGD2),WORK+20                             
         GOTO1 GETDAY,DMCB,WORK+20,FULL                                         
         MVI   FULL,0                                                           
         MVN   FULL,RBUYDYIN                                                    
         DROP  R6                                                               
*                                                                               
         ZIC   RE,FULL             ADJUST TO ACTUAL END DATE                    
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    CRDED30                                                          
         BP    CRDED20                                                          
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     CRDED40                                                          
*                                                                               
CRDED20  DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     CRDED40                                                          
*                                                                               
CRDED30  DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
CRDED40  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK+20,WORK+20,(R2)                                  
         GOTO1 DATCON,DMCB,(0,WORK+20),(3,WKD.RMKGMGD2)                         
         DROP  WKD                                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   DUMPMGS:  DROPS OLD RECORDS FOR THIS KEY BEFORE WRITING NEW,                
*        CHANGED RECORDS.  RECORDS HAVE BEEN COMPLETELY VALIDATED.              
*        SAVES X'20' ELEMENTS FROM RECORDS FOR CARRY-OVER OF FLAGS.             
*                                                                               
***********************************************************************         
DUMPMGS  CSECT                                                                  
         NMOD1 0,*DUMPMG*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY             CLEAR KEY AREA                               
         XC    WORK,WORK           CLEAR KEY STORAGE AREA                       
         LA    R6,SV20ELTS         SAVE AREA FOR X'20' ELEMENTS                 
         MVC   KEY,0(R2)           R2 POINTS TO STORED RECORD                   
         MVC   WORK(26),KEY        SAVE 1ST 26 CHARS OF KEY                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  RETURN KEY FOR UPDATE - SKIP                 
*                                     DELETED RECORDS                           
         B     DMGS0040                                                         
DMGS0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY               
*                                  RETURN NEXT KEY FOR UPDATE                   
DMGS0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME DATE/TIME/ORIG BUY/MGL#?                
         BNE   DMGS0400            NO  - FINISHED                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                  RETRIEVE RECORD INTO MAKEGOOD REC            
         LA    R1,RMKGELEM         FIND X'20' ELEMENT                           
DMGS0060 EQU   *                                                                
         ZIC   RF,1(R1)                                                         
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R1),0             END OF RECORD?                               
         BE    DMGS0080            YES - NO ELEMENT FOUND                       
         CLI   0(R1),X'20'         X'20' ELEMENT?                               
         BNE   DMGS0060            NO  - GO BACK FOR NEXT                       
         ZIC   RF,1(R1)            YES - MOVE TO STORAGE                        
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,DMGS0900         MOVE BY LENGTH                               
         NI    RMKGSTCH-RMKGSTEL(R6),X'FF'-X'01'                                
*                                  TURN OFF 'CHOSEN' FLAG                       
DMGS0080 EQU   *                                                                
         LA    R6,20(R6)           BUMP TO NEXT SAVE AREA                       
         OI    RMKGCNTL,X'80'      MARK RECORD DELETED                          
         GOTO1 DATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 DATAMGR,DMCB,(X'80',DMWRITE),=C'REPDIR',KEYSAVE,KEY    X         
*                                  REWRITE DELETED KEY                          
         B     DMGS0020            GO BACK FOR NEXT                             
DMGS0400 EQU   *                                                                
         B     EXXMOD                                                           
DMGS0900 EQU   *                                                                
         MVC   0(0,R6),0(R1)       SAVE ENTIRE ELEMENT                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   MRKCHOYS:  MARK SELECTED RECORD AS INDICATED BY CHOICE FIELD                
*                                                                               
MRKCHOYS CSECT                                                                  
         NMOD1 0,*MCHOYS*                                                       
         L     RC,0(R1)                                                         
         XC    KEY,KEY                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAMKGD2                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                                                               
         MVC   KEY(27),RMKGREC                                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
         LA    R2,MGOMOPTH                                                      
         LA    R3,464              ERROR IN CHOICE FIELD                        
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   ERROR                                                            
         LA    R4,1                                                             
         B     MCHOYS30                                                         
*                                                                               
MCHOYS10 EQU   *                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'REPDIR',KEYSAVE,KEY               
*                                                                               
MCHOYS20 EQU   *                   SAME GROUP AND OFFER?                        
         CLC   KEY(RMKGKRTY-RMKGREC),KEYSAVE                                    
         BNE   MCHOYSX                                                          
*                                                                               
MCHOYS30 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                  RETRIEVE RECORD INTO MAKEGOOD REC            
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      RECORD PREVIOUSLY SELECTED?                  
         BO    MCHOYS40                                                         
         ZIC   RF,CHOYSCTR         NO, IS THIS REC CURRENTLY SELECTED?          
         CR    R4,RF                                                            
         BNE   MCHOYS50                                                         
         OI    RMKGSTCH,X'01'      YES, MARK RECORD AND WRITE IT BACK           
         B     MCHOYS45                                                         
*                                                                               
MCHOYS40 DS    0H                  RECORD PREVIOUSLY SELECTED?                  
         ZIC   RF,CHOYSCTR                                                      
         CR    R4,RF               YES, IS THIS REC CURRENTLY SELECTED?         
         BE    MCHOYS50            YES, SKIP WRITE BACK                         
         NI    RMKGSTCH,X'FF'-X'01' NO, REMOVE MARK AND WRITE BACK              
*                                                                               
MCHOYS45 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                                                               
MCHOYS50 DS    0H                                                               
         LA    R4,1(R4)                                                         
         B     MCHOYS10                                                         
*                                                                               
MCHOYSX  DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF ONLY ONE PREEMPT PER GROUP                                           
***********************************************************************         
CHECKPRE CSECT                                                                  
         NMOD1 0,*CHKPRE*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,632              ONLY ONE PREEMPT PER GROUP                   
         SR    R4,R4               NUMBER OF RECORDS READ COUNTER               
         MVC   KEY(21),RMKGREC                                                  
         GOTO1 VHIGH                                                            
         B     CHKP30                                                           
*                                                                               
CHKP20   DS    0H                                                               
         GOTO1 VSEQ                                                             
*                                                                               
CHKP30   DS    0H                                                               
         CLC   KEY(21),KEYSAVE                                                  
         BNE   CHKP50                                                           
         OC    KEY+21(6),KEY+21                                                 
         BZ    CHKP20                                                           
         CLC   KEY(27),RMKGREC     SKIP THE ONE WE ARE PROCESSING               
         BE    CHKP20                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,AIO3                                                
*                                                                               
         LA    R4,1(R4)                                                         
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'       IS THERE A PREEMPT IN THIS GROUP?            
         BZ    CHKP20                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
CHKP50   DS    0H                  IS THE CURRENT RECORD A PREEMPT?             
         TM    RMKGRTS,X'10'                                                    
         BZ    CHKPX               NO, GET OUT                                  
         LTR   R4,R4               YES, ARE THERE ANY OTHER OFFERS IN           
         BNZ   ERROR               THIS GROUP? YES, ERROR!                      
*                                                                               
CHKPX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IF DARE, CANNOT OFFER ZERO AND MISS ZERO SPOTS                                
***********************************************************************         
CKZEROSP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKZX                                                             
*                                                                               
         L     R6,MGSTOR#1         SET A(1ST MAKEGOOD AREA)                     
         USING RMKGMGEL,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKZX                                                             
*                                                                               
CKZ10    DS    0H                                                               
         CLI   RMKGMGSP,0          CHECK IF MISSED ZERO SPOT                    
         BNE   CKZX                ON ALL TARGET DATES                          
         BAS   RE,NEXTEL                                                        
         BE    CKZ10                                                            
*                                                                               
         LA    R5,3                                                             
         LA    R2,MGOMSPTH                                                      
         LA    R3,826              CANNOT MISS AND OFFER ZERO SPOT              
*                                                                               
CKZ20    DS    0H                                                               
         CLI   8(R2),C'0'                                                       
         BE    ERROR                                                            
         LA    R2,MGOMDY2H-MGOMDYSH(R2)                                         
         BCT   R5,CKZ20                                                         
*                                                                               
CKZX     B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS FOR PREEMPT MAKEGOODS                                                 
***********************************************************************         
PROCPREM CSECT                                                                  
         NMOD1 0,*PRCPRM*                                                       
         L     RC,0(R1)                                                         
                                                                                
         L     R6,AORIGBUY                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROCP20                                                          
*                                                                               
PROCP10  DS    0H                  ADD DAY-TIME ELEM FROM ORIGINAL BUY          
         XC    MYP,MYP                                                          
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
*                                                                               
PROCP20  DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6               PREEMPT SUPPORTS ONLY 1 MISSED DATE          
         LA    R3,631                                                           
         BAS   RE,NEXTEL                                                        
         BE    ERROR                                                            
         LR    R6,R5                                                            
*                                                                               
PROCP30  DS    0H                                                               
         USING RMKGMGEL,R6                                                      
*                                                                               
ELEM     USING RBUYDYEL,MYP                                                     
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),MYP+64                                  
         GOTO1 GETDAY,DMCB,MYP+64,MYP+70                                        
         MVC   ELEM.RBUYDYIN,DMCB                                               
         MVO   ELEM.RBUYDYIN,ELEM.RBUYDYIN                                      
*                                  CALCULATE DAYS OF THE WEEK                   
         ZIC   R3,DMCB                                                          
         MVI   ELEM.RBUYDAYS,X'40'                                              
         CLI   DMCB,1                                                           
         BE    PROCP40                                                          
         ZIC   R4,ELEM.RBUYDAYS                                                 
         BCTR  R3,0                                                             
*                                                                               
PROCP35  DS    0H                                                               
         SRL   R4,1                                                             
         BCT   R3,PROCP35                                                       
         STC   R4,ELEM.RBUYDAYS                                                 
         DROP  ELEM                                                             
*                                                                               
PROCP40  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,MYP                                        
*                                  THE X'02' ELEMENT                            
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2(64),WORK2                                                  
WK2      USING RBUYDTEL,WORK2                                                   
         MVI   WK2.RBUYDTCD,X'03'                                               
         MVI   WK2.RBUYDTLN,X'0B'                                               
*                                                                               
         GOTO1 GETDAY,DMCB,RMKGMGD1,WORK2+64                                    
*                                                                               
         MVC   WK2.RBUYDTST,RMKGMGD1                                            
         MVC   WK2.RBUYDTED,RMKGMGD1                                            
*                                                                               
         MVI   WK2.RBUYDTIN,X'80'  RUNS WEEKLY                                  
         MVC   WK2.RBUYDTNW,RMKGMGSP                                            
         MVI   WK2.RBUYDTWK,1                                                   
         DROP  R6,WK2                                                           
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                                                               
         L     R6,AORIGBUY                                                      
         USING RBUYREC,R6                                                       
         ZICM  R1,RBUYCOS,4                                                     
         LNR   R1,R1                                                            
         DROP  R6                                                               
*                                                                               
         STCM  R1,15,RMKGCOS                                                    
*                                                                               
PROCPX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE CHANGES TO GROUP COMMENT ONLY FOR OFFEREE REJECTION                      
***********************************************************************         
REJCMT   CSECT                                                                  
         NMOD1 0,*REJCMT*                                                       
         L     RC,0(R1)                                                         
                                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         LA    R3,576              NO D/A, REC NOT FOUND                        
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    ERROR                                                            
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  RF                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* DELETE EXISTING GROUP COMMENT                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'10',IOAREA),0,0                 
*                                                                               
         LA    R2,MGOGCOMH         SET A(GROUP COMMENT FIELD)                   
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    REJCMT20            SKIP IF NO COMMENT                           
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RMKGGCEM,R6                                                      
         MVI   RMKGGCCD,X'10'                                                   
         ZIC   RF,5(R2)            GET LENGTH OF FIELD                          
         LA    RF,2(RF)                                                         
         STC   RF,RMKGGCLN         INSERT LENGTH INTO ELEMENT                   
         SH    RF,=H'3'            SUBTRACT 2 FOR CONTROL, 1 FOR EX             
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   RMKGGCCM(0),8(R2)                                                
*                                                                               
         GOTO1 VADDELEM,DMCB,IOAREA,WORK2                                       
*                                  INSERT ELEMENT INTO RECORD                   
REJCMT20 EQU   *                                                                
         GOTO1 RECWRITE,DMCB,IOAREA,0                                           
*                                                                               
REJCMT30 EQU   *                                                                
         BAS   RE,CHGCOMNT                                                      
*                                                                               
REJCMTX  EQU   *                                                                
         XC    DUB,DUB                                                          
         MVC   DUB,=H'140'         INSERT RETURN MESSAGE                        
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   CHGCOMNT:  RETRIEVE EACH RECORD OF THE OFFER, DELETE THE ORIGINAL           
*        DETAIL COMMENT, AND APPLY THE NEW COMMENT, IF COMMENT HAS              
*        BEEN CHANGED.                                                          
*                                                                               
***********************************************************************         
CHGCOMNT NTR1                                                                   
         XC    KEY,KEY                                                          
         LR    R7,RA                                                            
         AHI   R7,TWAWORKQ                                                      
         USING TWAWORK,R7                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),TWAMKGD2  SET D/A OF MG RECORD SET                     
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE INDICATED RECORD                    
*                                  ALWAYS RESTART SEQUENTIAL KEY                
*                                     READABILITY                               
         LA    R2,MGOMDESH         SET A(MISSED LINE COMMENT)                   
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   RMKGKRTY,0          CLEAR RECORD COUNTER                         
         MVC   KEY(27),RMKGREC     INSERT KEY FROM RECORD                       
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VHIGH               READ FOR 1ST OF SET                          
         CLC   KEY(26),KEYSAVE     RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NO  - 1ST ONE MUST BE THERE                  
*                                                                               
* PROCESS FOR MISSED LINE COMMENT                                               
*                                                                               
         TM    4(R2),X'20'         PREVALID SET ON COMMENT?                     
         BNO   CCMT0010            NO  - CHANGE COMMENT                         
         LA    R2,MGOOPGNH         SET A(1ST DETAIL COMMENT)                    
         B     CCMTP050                                                         
*                                                                               
CCMT0010 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE THE RECORD                          
         LA    R6,RMKGREC                                                       
         USING RMKGCDEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SAVE PREVIOUS DATE/TIME STAMP                
         XC    ELTBILD,ELTBILD                                                  
         MVC   ELTBILD(RMKGCDDS-RMKGCDEL),RMKGCDEL                              
         MVI   ELTBILD+1,RMKGCDDS-RMKGCDEL                                      
         DROP  R6                                                               
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    CCMT0015            NO                                           
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   ELTBILD+10(0),8(R2)                                              
         LA    RF,11(RF)           ADD BACK EX + 10 FOR CONTROL                 
         STC   RF,ELTBILD+1        INSERT LENGTH                                
*                                                                               
CCMT0015 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'10',RMKGREC),0,0                
*                                  DROP PREVSIOU MISSED LINE COMMENT            
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
*                                                                               
         LA    R2,MGOOPGNH         SET A(1ST DETAIL COMMENT)                    
         B     CCMTP050                                                         
*                                                                               
CCMT0020 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VSEQ                READ FOR NEXT OF SET                         
CCMT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY?                                    
         BNE   CCMT0400            NO  - FINISHED                               
*                                                                               
CCMTP050 EQU   *                                                                
         TM    4(R2),X'20'         PREVALID SET ON COMMENT?                     
         BNO   CCMTP060            NO  - CHANGE COMMENT                         
         LA    R2,MGODCOMH-MGOOPGNH(R2)                                         
*                                  YES - NO CHANGE: GO TO COMMENT               
         B     CCMT0050                                                         
CCMTP060 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE THE RECORD                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'21',RMKGREC),0,0                
*                                  DROP ANY DETAIL COMMENT ELT                  
         XC    ELTBILD,ELTBILD                                                  
         MVI   ELTBILD,X'21'       INSERT LENGTH INTO KEY                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    CCMT0080            NO  - DON'T INSERT NEW X'11' ELT             
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,CCMT0800         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            ADD BACK EX + 2 FOR CONTROL                  
         STC   RF,ELTBILD+1        INSERT LENGTH                                
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
*                                                                               
*                                                                               
CCMT0050 EQU   *                                                                
         TM    4(R2),X'20'         PREVALID SET ON COMMENT?                     
         BNO   CCMT0060            NO  - CHANGE COMMENT                         
         LA    R2,MGODCM2H-MGODCOMH(R2)                                         
*                                  YES - NO CHANGE: GO TO NEXT RECORD           
         B     CCMT0020                                                         
CCMT0060 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET RETRIEVE FOR UPDATE                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE THE RECORD                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'11',RMKGREC),0,0                
*                                  DROP ANY DETAIL COMMENT ELT                  
         XC    ELTBILD,ELTBILD                                                  
         MVI   ELTBILD,X'11'       INSERT LENGTH INTO KEY                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    CCMT0080            NO  - DON'T INSERT NEW X'11' ELT             
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,CCMT0800         MOVE COMMENT BY LENGTH                       
         LA    RF,3(RF)            ADD BACK EX + 2 FOR CONTROL                  
         STC   RF,ELTBILD+1        INSERT LENGTH                                
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
CCMT0080 EQU   *                                                                
         LA    R2,MGOOPG2H-MGODCOMH(R2)                                         
*                                  BUMP TO NEXT SCREEN COMMENT                  
         B     CCMT0020            GO BACK FOR NEXT                             
CCMT0400 EQU   *                                                                
         B     EXXMOD                                                           
*                                                                               
CCMT0800 EQU   *                                                                
         MVC   ELTBILD+2(0),8(R2)                                               
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   GRPCOMNT:  CHECK FOR ACTIVE RECORDS FOR THIS GROUP.  IF NONE,               
*        DELETE THE GRPCOMNT RECORD.  IF SOME STILL EXIST,                      
*        UPDATE THE GROUP COMMENT.                                              
*                                                                               
***********************************************************************         
GRPCOMNT CSECT                                                                  
         NMOD1 0,*GRPCMT*                                                       
         L     RC,0(R1)                                                         
                                                                                
         XC    WORK2,WORK2         CLEAR TEMPORARY SPACE                        
         XC    KEY,KEY                                                          
         L     RF,MGSTOR#1         SET A(1ST STORAGE AREA)                      
         MVC   KEY(21),0(RF)       ESTABLISH KEY FOR GROUP                      
         GOTO1 VHIGH               GET 1ST RECORD OF GROUP                      
         B     GCOM0020                                                         
GCOM0010 EQU   *                                                                
         GOTO1 VSEQ                GET NEXT RECORD IN GROUP                     
GCOM0020 EQU   *                                                                
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD?                        
         BNE   GCOM0030            NO  -                                        
*                                  YES - SAVE STATUS BYTES                      
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                  RETRIEVE RECORD INTO MAKEGOOD REC            
         MVC   WORK2(RMKGSELQ),RMKGSDEM                                         
*                                  SAVE STATUS BYTES                            
         B     GCOM0010            NOW BYPASS GROUP COMMENT RECORD              
GCOM0030 EQU   *                                                                
         CLC   KEY(21),KEYSAVE     NO  - GROUP STILL ACTIVE?                    
         BNE   GCOM0240            NO  - DELETE GROUP COMMENT RECORD            
         GOTO1 VMOVEREC,DMCB,MGSTOR#1,RMKGREC                                   
         XC    RMKGSDEM(RMKGSELQ),RMKGSDEM                                      
*                                  CLEAR TO BUILD GROUP COMMENT RECORD          
         XC    RMKGKPLN(6),RMKGKPLN                                             
*                                  CLEAR KEY TO GROUP COMMENT FORMAT            
         CLC   =C'MGO',CONCACT                                                  
         BE    GCOM0040                                                         
*                                                                               
* UPDATE EXISTING HEADER RECORD FOR ACTION MGC                                  
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         LA    R3,576              NO D/A, REC NOT FOUND                        
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    ERROR                                                            
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  RF                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
* DELETE AND REBUILD X'01' ELEMENT FOR ACTION MGC                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'01',RMKGREC),0,0                
*                                                                               
GCOM0040 EQU   *                                                                
         LA    R6,WORK2                                                         
         USING RMKGSDEM,R6                                                      
         MVI   RMKGSDEM,X'01'                                                   
         MVI   RMKGSELN,RMKGSELQ   SET INITIAL LENGTH                           
*                                                                               
* IF MGO/ADD, SAVE REP OR STATION CREATOR/ORIGINATOR                            
*                                                                               
         CLC   =C'MGO',CONCACT                                                  
         BNE   GCOM0050                                                         
*                                                                               
* STATION WIP                                                                   
*                                                                               
         MVI   RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         CLI   TWAACCS,C'$'        STATION SIDE?                                
         BE    GCOM0090            YES                                          
         OI    RMKGSCST,RMKGSCRQ   NO, REP                                      
*                                                                               
* REP WIP                                                                       
*                                                                               
         MVI   RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         B     GCOM0090                                                         
                                                                                
GCOM0050 EQU   *                                                                
         CLC   =C'MGC',CONCACT     FOR ACTION MGC/CHANGE                        
         BNE   GCOM0090                                                         
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    GCOM0070                                                         
*                                                                               
* IF FIRST TO CHANGE AFTER MGS                                                  
*                                                                               
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    GCOM0080                                                         
         OI    RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED                     
         NI    RMKGSFG2,X'FF'-RMGF2STQ                                          
*                                                                               
* REP WIP                                                                       
*                                                                               
         OI    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         B     GCOM0080                                                         
*                                                                               
* IF FIRST TO CHANGE AFTER MGS                                                  
*                                                                               
GCOM0070 EQU   *                                                                
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    GCOM0080                                                         
         OI    RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED                     
         NI    RMKGSFG2,X'FF'-RMGF2RPQ                                          
*                                                                               
* STATION WIP                                                                   
*                                                                               
         OI    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
*                                                                               
GCOM0080 EQU   *                                                                
         TM    RMKGSCST,RMKGSRCQ+RMKGSRJQ                                       
         BZ    GCOM0090            OFFER WAS RECALLED/REJECTED                  
         NI    RMKGSCST,X'FF'-RMKGSRCQ-RMKGSRJQ                                 
         OI    RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED                     
         DROP  R6                                                               
*                                                                               
GCOM0090 EQU   *                                                                
WK2      USING RMKGSDEM,WORK2                                                   
         MVC   WK2.RMKGSMOD,RCONMOD CURRENT CONTRACT MOD NUMBER                 
         LA    RF,RCONELEM         LOOK FOR 'DARE' ORDER                        
*                                                                               
GCOM0110 EQU   *                                                                
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0             END OF RECORD?                               
         BE    GCOM0120            YES - NOT A DARE ORDER                       
         CLI   0(RF),X'1D'         DARE ORDER ELEMENT?                          
         BNE   GCOM0110            SHOULD BE                                    
         USING RCONDREL,RF         LINKED, APPROVED OR TAKEOVER                 
*        TM    RCONDRFG,X'80'+X'40'+X'01'                                       
*        BZ    GCOM0120                                                         
         OC    RCONDRLK,RCONDRLK                                                
         BZ    GCOM0120                                                         
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GCOM0115                                                         
         TM    RCONDRF2,X'08'      ORDER WAS REMOVED??                          
         BO    GCOM0120            YES, NOT A DARE ORDER ANYMORE                
*                                                                               
GCOM0115 EQU   *                                                                
         MVC   WK2.RMKGDARN,RCONDRLK GET AGENCY DARE ORDER NUMBER               
         DROP  RF                                                               
*                                                                               
GCOM0120 EQU   *                                                                
         OI    WK2.RMKGSCST,RMKGSPAQ   DEFAULT:PLEASE ADVISE                    
         CLI   MGOMPAD,C'Y'                                                     
         BE    *+8                                                              
         NI    WK2.RMKGSCST,X'FF'-RMKGSPAQ                                      
*                                                                               
* CREATION DATE                                                                 
*                                                                               
         CLC   =C'MGO',CONCACT     FOR ACTION MGO/ADD                           
         BNE   GCOM0130                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,WK2.RMKGSCRD)                               
         GOTO1 GETTIME,DMCB,WK2.RMKGSCRT                                        
*                                                                               
* EXPIRATION DATE                                                               
*                                                                               
GCOM0130 DS    0H                                                               
**** TRAP TO FIND MAKEGOOD BUG                                                  
         OC    WK2.RMKGSCRD,WK2.RMKGSCRD                                        
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE CREATION DATE!                     
**** TRAP TO FIND MAKEGOOD BUG                                                  
         LA    R2,MGOEXPDH                                                      
         CLI   5(R2),0                                                          
         BE    GCOM0140                                                         
         GOTO1 DATVAL,DMCB,MGOEXPD,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    GCOM0140            SKIP IT                                      
         GOTO1 DATCON,DMCB,WORK,(2,WK2.RMKGSEXD)                                
*                                                                               
* LAST ACTIVITY DATE                                                            
*                                                                               
GCOM0140 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WK2.RMKGSLAD)                               
         GOTO1 GETTIME,DMCB,WK2.RMKGSLAT                                        
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
         DROP  WK2                                                              
*                                                                               
* FIND AND SAVE OFF FIRST OFFERED AIR DATE FOR RIS OFFER LISTING                
*                                                                               
         BRAS  RE,FIRSTAIR                                                      
*                                                                               
* ADD OR DELETE/REPLACE EXISTING SWITCH/PASSIVE KEY/SP-TEAM ELEMENT             
*                                                                               
         BRAS  RE,OLD0AELT         FIND OLD X'0A' ELEMENT                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0A',RMKGREC),0,0                
         XC    WORK2,WORK2         CLEAR ELEMENT                                
         MVI   WORK2,X'0A'         INSERT ELEMENT CODE                          
         MVI   WORK2+1,RMKGXLNQ    INSERT ELEMENT LENGTH                        
WK2      USING RMKGXEL,WORK2                                                    
         MVC   WK2.RMKGXSAL,RCONSAL                                             
*                                  INSERT SALESPERSON CODE                      
         MVC   WK2.RMKGXOFF,RCONKOFF                                            
*                                  INSERT OFFICE CODE                           
         MVC   WK2.RMKGXTEM,RCONTEM                                             
*                                  INSERT TEAM CODE                             
         MVC   WK2.RMKGXSTA,RCONKSTA                                            
*                                  INSERT STATION CODE                          
         MVC   WK2.RMKGXADV,RCONKADV                                            
*                                  INSERT ADVERTISER CODE                       
         MVC   WK2.RMKGXAGY,RCONKAGY                                            
*                                  INSERT AGENCY CODE                           
         MVC   WK2.RMKGXAOF,RCONKAOF                                            
*                                  INSERT AGENCY OFFICE CODE                    
         MVC   WK2.RMKGXGRP,RCONKGRP                                            
*                                  INSERT GROUP/SUBGROUP                        
         MVC   WK2.RMKGXFLT,RCONDATE                                            
*                                  INSERT FLIGHT DATES                          
         LA    R6,RCONREC          CHECK FOR USE OF REVISED FLIGHT              
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   GCOM0141                                                         
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    GCOM0141                                                         
         MVC   WK2.RMKGXFLT,RCONRFLT  USE REVISED FLIGHT                        
         DROP  R6                                                               
*                                                                               
GCOM0141 DS    0H                                                               
         LA    RF,RCONELEM         FIND DEVELOPMENT ELT                         
GCOM0142 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    GCOM0148            YES - NO DEV ELT PRESENT                     
         CLI   0(RF),X'18'         DEVELOPMENT ELT?                             
         BE    GCOM0144            YES -                                        
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     GCOM0142            GO BACK FOR NEXT                             
GCOM0144 EQU   *                                                                
         MVC   WK2.RMKGXDSP,RCONDVSP-RCONDVEL(RF)                               
*                                  INSERT DEV S/P                               
         MVC   WK2.RMKGXDCT,RCONDVCT-RCONDVEL(RF)                               
*                                  INSERT DEV CON TYPE                          
GCOM0148 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
         DROP  WK2                                                              
*                                                                               
*                                                                               
* DELETE EXISTING GROUP COMMENT                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'10',RMKGREC),0,0                
*                                                                               
         LA    R2,MGOGCOMH         SET A(GROUP COMMENT FIELD)                   
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    GCOM0150            SKIP IF NO COMMENT                           
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RMKGGCEM,R6                                                      
         MVI   RMKGGCCD,X'10'                                                   
         ZIC   RF,5(R2)            GET LENGTH OF FIELD                          
         LA    RF,2(RF)                                                         
         STC   RF,RMKGGCLN         INSERT LENGTH INTO ELEMENT                   
         SH    RF,=H'3'            SUBTRACT 2 FOR CONTROL, 1 FOR EX             
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   RMKGGCCM(0),8(R2)                                                
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                  INSERT ELEMENT INTO RECORD                   
GCOM0150 EQU   *                                                                
         GOTO1 RECWRITE,DMCB,RMKGREC,1                                          
         B     GCOM0400                                                         
GCOM0240 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE GROUP KEY                          
         XC    WORK,WORK           CLEAR KEY STORAGE AREA                       
         MVC   KEY(21),MGSTOR#1    ESTABLISH KEY FOR GROUP                      
         MVC   WORK(26),KEY        SAVE 1ST 26 CHARS OF KEY                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE THE KEY                                 
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  RETURN GROUP COMMT KEY FOR UPDATE            
         CLC   KEY(26),KEYSAVE     GROUP COMMENT FOUND?                         
         BNE   GCOM0400            NO  - FINISHED                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
*                                  RETRIEVE RECORD INTO MAKEGOOD REC            
         OI    RMKGCNTL,X'80'      MARK RECORD DELETED                          
         GOTO1 DATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,         X        
               RMKGREC,MGWORK                                                   
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 DATAMGR,DMCB,(X'80',DMWRITE),=C'REPDIR',KEYSAVE,KEY    X         
*                                  REWRITE DELETED KEY                          
*                                                                               
GCOM0400 EQU   *                                                                
         B     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FIND AND SAVE OFF FIRST OFFERED AIR DATE FOR RIS OFFER LISTING                
***********************************************************************         
FIRSTAIR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,3                SET LOOP CONTROL                             
         LA    R7,RMKGREC          SET A(MAKEGOOD HEADER AREA)                  
MHEADD   USING RMKGREC,R7                                                       
         L     R2,MGSTOR#1         SET A(1ST MAKEGOOD AREA)                     
*                                                                               
FSTAIR10 DS    0H                                                               
         LR    R6,R2                                                            
         USING RMKGDTEL,R6                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   FSTAIR50            PREEMPT WILL NOT HAVE THIS                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,RMKGDTST),(2,FULL)                                
         OC    MHEADD.RMKGFOFD,MHEADD.RMKGFOFD                                  
         BZ    FSTAIR30                                                         
         CLC   MHEADD.RMKGFOFD,FULL                                             
         BL    FSTAIR50                                                         
*                                                                               
FSTAIR30 DS    0H                                                               
         MVC   MHEADD.RMKGFOFD,FULL                                             
*                                                                               
FSTAIR50 DS    0H                                                               
         LA    R2,1000(R2)         BUMP TO NEXT STORAGE AREA                    
         OC    0(34,R2),0(R2)      ANYTHING IN AREA?                            
         BZ    FSTAIRX                                                          
         BCT   R3,FSTAIR10         GO BACK AND CHECK IT                         
*                                                                               
FSTAIRX  XIT1                                                                   
         DROP  R6,MHEADD                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   OLD0AELT:  FIND EXISTING 0A ELT, SAVE S/P, TEAM CODES FOR                   
*        DELETING OLD KEY                                                       
*                                                                               
OLD0AELT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*         READ MAKEGOOD HEADER                                                  
*                                                                               
         XC    WORK,WORK           INIT WOORKAREA                               
         XC    DUB,DUB             USE DUB FOR INTERMEDIATE                     
*                                                                               
         MVC   WORK(32),KEY        SAVE CURRENT KEY                             
*                                                                               
         CLC   =C'MGO',CONACT      SKIP IF ACTION MGO                           
         BE    OLD0AX                                                           
*                                                                               
         OC    KEY+21(6),KEY+21    IF NULLS WE HAVE A MKG HEADER                
         BNZ   *+12                                                             
         LA    R6,RMKGREC             AND WE CAN USE IT                         
         B     OLD0A10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC     INSERT KEY OF SELECTED REC                   
         XC    KEY+21(6),KEY+21    CLEAR LOW ORDER OF KEY                       
*                                                                               
         GOTO1 VHIGH               READ KEY                                     
*                                                                               
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD FOUND?                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
         L     R6,AIO2             POINT TO MAKEGOOD HEADER                     
*                                                                               
OLD0A10  DS    0H                                                               
*                                                                               
         LA    R1,RMKGELEM-RMKGREC(R6)                                          
*                                                                               
*        SAVE FIELDS FOR DELETING PASSIVES                                      
*                                                                               
         USING RMKGSDEM,R1         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   SAVDAT,RMKGFOFD     SAVE FIRST OFFERED DATE                      
         MVC   SAVWIP,RMKGSFG2     WIP STATUS                                   
         MVC   SAVSTT,RMKGSCST     OFFER STATUS                                 
         NI    SAVSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                            
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    SAVSTT,RMKGSLFQ        SET INDICATOR                             
*                                                                               
         MVC   SAVDST,RMKGSFG1     DARE  STATUS                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0020 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    OLDA0100            YES - FINISHED - NO ELT                      
         CLI   0(R1),X'0A'         0A ELT?                                      
         BE    OLDA0040            YES - PROCESS IT                             
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     OLDA0020            GO BACK FOR NEXT                             
OLDA0040 EQU   *                                                                
         USING RMKGXEL,R1                                                       
         MVC   DUB(3),RMKGXSAL     SAVE S/P CODE                                
         MVC   DUB+3(2),RMKGXTEM   SAVE TEAM CODE                               
*                                                                               
*        SAVE FIELDS FOR PASSIVES                                               
*                                                                               
         MVC   SAVSAL,RMKGXSAL     SALESPERSON                                  
         MVC   SAVTEM,RMKGXTEM     TEAM                                         
         MVC   SAVADV,RMKGXADV     ADVERTISER                                   
         MVC   SAVDSL,RMKGXDSP     DEVELOPMENTAL SALESPERSON                    
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0100 EQU   *                                                                
*                                                                               
         OC    WORK,WORK           SKIP IF NEW REC NOT READ                     
         BZ    OLD0AX                                                           
*                                                                               
         MVC   KEY,WORK            RESTORE INCOMING KEY                         
         GOTO1 VHIGH               RESTORE FILE POINTERS                        
*                                                                               
OLD0AX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CANCEL MAKEGOOD                                                               
***********************************************************************         
MKGCAN   CSECT                                                                  
         NMOD1 0,*MKGCAN*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    MKGCANX                                                          
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  RF                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* SEND CANCELLATION TO DARE AGENCY IF NOT ALREADY MARKED 'CANCELLED'            
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    MKGCANX                                                          
         TM    RMKGSFG1,RMGF1MCN                                                
         BO    MKGCANX                                                          
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETDAREH         RETRIEVE DARE HEADER RECORD INTO IO4         
         BNZ   MKGCANX             NOT FOUND, SKIP                              
*                                                                               
         L     R7,ASPULAR                                                       
         USING SPOOLD,R7                                                        
*                                                                               
         BAS   RE,MYPQOPEN                                                      
*                                                                               
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
         MVI   P+34,C'W'      WIDE REPORT - 132 CHARS                           
*                                  SEND SPECIAL PRINT LINE                      
         BAS   RE,PRINT                                                         
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,=C'MKG'     TYPE=MAKEGOOD                                
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         DROP  R3                                                               
*                                  SEND SPECIAL PRINT LINE                      
         BAS   RE,PRINT                                                         
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCAND,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCNTID,=C'MKGCAN'                                               
                                                                                
* ORDER NUMBER                                                                  
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,MOCNORDR),FILL=0                                   
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCNFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCNTOID,RDARSNDR                                                
                                                                                
* ROUTING CODE                                                                  
         MVC   MOCNROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCNDATE)                               
                                                                                
* CURRENT TIME                                                                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,MOCNTIME,2,0                                    
*                                                                               
* STATION                                                                       
         MVC   MOCNQSTA,RDARKSTA                                                
         CLI   MOCNQSTA+4,C'L'                                                  
         BE    MKGCAN10                                                         
         MVI   MOCNQSTA+5,C'V'     TV OR RADIO?                                 
         CLI   MOCNQSTA+4,C'T'                                                  
         BE    *+8                                                              
         MVI   MOCNQSTA+5,C'M'                                                  
                                                                                
* CONTRACT NUMBER                                                               
MKGCAN10 DS    0H                                                               
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RCONKCON                                                
         EDIT  (P5,WORK2),(8,MOCNRPCN),FILL=0                                   
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCNRTNS,RDARRTS                                                 
         DROP  R6                                                               
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOCNOFRI(2),RMKGKGR1                                             
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOCNSEQN),FILL=0                                     
                                                                                
* OFFER NO MORE TO FOLLOW                                                       
         MVI   MOCNNEWO,C'N'                                                    
                                                                                
         BAS   RE,PRINT                                                         
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ                                     
         GOTO1 SPOOL,PARAS,ASPULAR                                              
                                                                                
MKGCANX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
*********************************************************************           
* PRINT A LINE                                                                  
*********************************************************************           
PRINT    NTR1                                                                   
         GOTO1 SPOOL,PARAS,ASPULAR                                              
         MVI   LINE,2              FORCE EVERYTHING TO PAGE ONE                 
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
MYPQOPEN NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
*                                                                               
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
*                                                                               
* OPEN PRINT QUEUE                                                              
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         MVC   PLSUBID,=C'DMG'                                                  
         MVC   PLDESC(9),=C'DARE MKGD'                                          
         MVI   PLCLASS,C'G'                                                     
         DROP  RF                                                               
*                                                                               
VPQ20    DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         LA    RE,TWASPKEY                                                      
         DROP  RF                                                               
*                                                                               
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
*                                                                               
         GOTO1 SPOOL,PARAS,ASPULAR                                              
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* RETRIEVE DARE HEADER RECORD X'51' INTO IO4, WHILE IGNORING AGENCY             
* OFFICE                                                                        
**********************************************************************          
GETDAREH NTR1                                                                   
         XC    KEY,KEY                                                          
KEYD     USING RAGY2KEY,KEY                                                     
         MVI   KEYD.RAGK2TYP,RAGK2TYQ                                           
         MVC   KEYD.RAGK2AGY,RCONKAGY                                           
         MVC   KEYD.RAGK2AOF,RCONKAOF                                           
         MVC   KEYD.RAGK2REP,RCONKREP                                           
         DROP  KEYD                                                             
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
*                                                                               
         L     R6,AIO2                                                          
         USING RAGY2REC,R6                                                      
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    GETDAR30            YES -- MISSING AGENCY                        
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   GETDNO                                                           
         XC    WORK2,WORK2                                                      
         MVC   WORK2(4),RCONDRLK     SAVE ORDER NUMBER IN WORK2                 
         DROP  R6                                                               
                                                                                
         L     R6,AIO2                                                          
         USING RAGY2REC,R6                                                      
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,3                COMBINATIONS WE NEED TO CHECK                
         B     GETDAR10                                                         
*                                                                               
*                                                                               
** PREP KEY FOR SKIP READING : SKIP TO NEXT AGY OFFICE IF AGENCY OFFICE         
** DIDN'T CHANGE                                                                
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
GETDAR08 CLC   RDARKAOF,PRVKEY.RDARKAOF DID THE AGENCY OFFICE CHANGE?           
         DROP  PRVKEY                                                           
         BNE   GETDAR09               YES -- DON'T INCREMENT                    
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
GETDAR09 XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
*                                                                               
GETDAR10 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11                                                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,WORK2        MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11                                                         
         CLC   RDARKORD,WORK2        SAME ORDER NUMBER?                         
         BNE   GETDAR08                                                         
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    GETDAR20                                                         
         B     GETDAR30                                                         
*                                                                               
GETDAR11 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETDAR12 LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    GETDAR30              YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,GETDAR12                                                      
         B     GETDAR30                                                         
*                                                                               
         MVC   RDARKAGY,0(R4)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF   CLEAR FIEILDS AFTER AGENCY CODE           
         BCT   R3,GETDAR10                                                      
         B     GETDAR30                                                         
         DROP  R5,R6                                                            
*                                                                               
GETDAR20 DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO4   USE IO4                                      
         B     GETDYES                                                          
*                                                                               
GETDAR30 DS    0H                  CHECK IF TAKEOVER                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETDNO                                                           
         USING RCONTKEL,R6                                                      
*                                                                               
         L     R5,AIO4                                                          
         USING RDARREC,R5                                                       
*                                                                               
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         MVC   RDARKAGY,RCONTKAR   AGENCY ROUTING (FOR JDS)                     
         MVC   RDARSNDR,RCONTKRC   SENDER ID                                    
         MVC   RDARRTS,RCONTKRT    AGENCY RETURN TO SENDER INFO                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         MVC   RDARKORD,RCONDRLK                                                
         DROP  R6                                                               
*                                                                               
* GET RECEIVER ID                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,28(R6)                                                        
GETDAR40 CLI   0(R6),X'02'         GET REP SIGN-ON ID                           
         BE    GETDAR50                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETDAR40                                                         
         DC    H'0'                                                             
*                                                                               
GETDAR50 DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDARRCVR(0),2(R6)                                                
         OC    RDARRCVR,=20C' '                                                 
         DROP  R5                                                               
*                                                                               
GETDYES  SR    RC,RC                                                            
GETDNO   LTR   RC,RC                                                            
GETDARX  B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEW10ELT:  ADDS X'10' CONTROL ELEMENT TO NEW MAKEGOOD RECORDS.              
*        ALSO ADDS X'20' ELEMENT TO RECORDS.  THE X'20' MAY BE FURTHER          
*        UPDATED FROM THE X'20' OF AN ORIGINAL RECORD LATER                     
*                                                                               
NEW10ELT NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2                                                      
         LA    R3,WORK2                                                         
         USING RMKGCDEL,R3                                                      
         MVI   RMKGCDCD,X'10'      INSERT ELEMENT CODE                          
         GOTO1 DATCON,DMCB,(5,DUB),(2,RMKGCDDT)                                 
*                                  INSERT DATE CREATED                          
         MVC   RMKGCDDU,RMKGCDDT                                                
*                                  LAST UPDATE = DATE CREATED                   
         TIME  DEC                 GET THE TIME CREATED                         
*                                  CURRENT TIME IN R0: HH:MM:SS:TT              
         STCM  R0,4,RMKGCDTM+1                                                  
*                                  STORE MM IN AREA                             
         SRL   R0,24               SAVE ONLY HOURS                              
         STCM  R0,1,DUB            STORE HH                                     
         GOTO1 HEXOUT,DMCB,DUB,FULL,1,=C'TOG'                                   
*                                  CONVERT TO EBCDIC VALUE                      
         PACK  DUB(8),FULL(2)      PACK EBCDIC VALUE                            
         CVB   RF,DUB              CONVERT IT TO BINARY                         
         LA    RF,DDSTMADJ(RF)     ADD 8 HR ADJUSTMENT                          
         EDIT  (RF),(2,FULL),FILL=0                                             
*                                  MAKE HH EBCDIC                               
         GOTO1 HEXIN,DMCB,FULL,RMKGCDTM,2                                       
*                                  STRIP ZONES FROM EBCDIC                      
         MVC   RMKGCDTU,RMKGCDTM                                                
*                                  TIME LAST UPDATED = TIME CREATED             
         LA    R2,MGOMDESH         A(DESCRIPTION FIELD)                         
         CLI   5(R2),0             ANYTHING IN DESCRIPTION?                     
         BZ    NELT0040            NO                                           
         ZIC   RF,5(R2)            YES - GET LENGTH                             
         BCTR  RF,0                DECREMENT 1 FOR EX                           
         EX    RF,NELT0020         MOVE DESCRIPTION BY LENGTH                   
         B     NELT0040                                                         
*                                                                               
NELT0020 MVC   RMKGCDDS(0),8(R2)   INSERT DESCRIP BY LENGTH                     
*                                                                               
NELT0040 EQU   *                                                                
         ZIC   RF,5(R2)            CALCULATE L(ELT): DESCRIP LENGTH             
         LA    RF,RMKGCDDS-RMKGCDCD(RF)                                         
*                                  CALCULATE ELEMENT LENGTH WITH DESC           
*                                     = L(DESC=RF) + LENGTH OF REST             
         STC   RF,RMKGCDLN         INSERT LENGTH INTO ELT                       
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                  ADD ELEMENT TO MAKEGOOD RECORD               
         DROP  R3                                                               
*                                  NOW ADD X'20' ELEMENT TO RECORD              
         XC    WORK2,WORK2                                                      
         LA    R3,WORK2                                                         
         USING RMKGSTEL,R3                                                      
         MVC   RMKGSTCD(2),=X'200A'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                  ADD ELEMENT TO MAKEGOOD RECORD               
*                                     ELEMENT IS 'INITIALIZED' ONLY             
         B     EXXMOD                                                           
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* GET DEMO VALUE FROM INPUT, IF NO INPUT, GET FROM TARGET BUY LINE REC          
***********************************************************************         
GETDEMV  NTR1  BASE=*,LABEL=*                                                   
         XC    ELTBILD,ELTBILD     CLEAR ELEMENT BUILD AREA                     
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
MKGDEM   USING RMKGDMEL,ELTBILD                                                 
         MVI   MKGDEM.RMKGDMCD,RMKGDMCQ SET ELEMENT TYPE CODE                   
         MVI   MKGDEM.RMKGDMLN,L'RMKGDMCV+2                                     
         MVC   MKGDEM.RMKGDMCT,TWADEM   TWADEM WAS PASSED FROM 2A MOD           
         MVC   MKGDEM.RMKGDMDM,TWADEM+3 DEFAULT VALUE FROM BUY LINE             
         MVC   MKGDEM.RMKGDM2M,=F'-1'   DEFAULT PREVIOUS VALUE = -1             
         DROP  RF                                                               
*                                                                               
         LA    R2,MGOODEMH         SET A(DEMO INPUT_)                           
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    GETDEMVX            NO  - ADD ELEMENT WITH DEFAULT VAL           
*                                                                               
DDEM0030 EQU   *                                                                
         ZIC   R5,5(R2)            GET LENGTH OF INPUT                          
         GOTO1 CASHVAL,DMCB,(X'81',8(R2)),(X'80',(R5))                          
         CLI   DMCB,X'FF'                                                       
         BE    ERRINVL                                                          
*                                                                               
         L     RF,DMCB+8                                                        
         SRL   RF,4                                                             
         STCM  RF,15,MKGDEM.RMKGDMDM                                            
         B     DDEM0100                                                         
*                                                                               
DDEM0100 EQU   *                                                                
         DROP  MKGDEM                                                           
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
*                                  ADD ELEMENT TO RECORD                        
GETDEMVX XIT1                                                                   
ERRINVL  DS    0H                                                               
         LA    R3,INVINP                                                        
         B     ERROR                                                            
**********   *********                                                          
* GETEL2 * = * GETEL *                                                          
**********   *********                                                          
GETEL2   EQU   *                                                                
         LA    R6,34(R6)                                                        
GETEL2A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL2X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL2A                                                          
GETEL2X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*************************************************                               
* LOOP UNTIL WE FIND A PRIMARY DEMO                                             
**************************************************                              
LOOPDEM  NTR1  BASE=*,LABEL=*       R6->ELT                                     
         ZIC   R5,1(R6)                                                         
         AR    R5,R6                <BOUNDRY>                                   
         LA    R6,2(R6)                                                         
LOOPD10  TM    0(R6),X'40'          PRIMARY DEM?                                
         BO    LOOPDYES                                                         
         LA    R6,L'RBUYDMCV(R6)                                                
         CR    R6,R5                                                            
         BNL   LOOPDNO                                                          
         B     LOOPD10                                                          
LOOPDYES SR    RC,RC                                                            
LOOPDNO  LTR   RC,RC                                                            
LOOPDEMX XIT1  REGS=(R6)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027RECNT2A   10/08/15'                                      
         END                                                                    
