*          DATA SET RESFM0BS   AT LEVEL 052 AS OF 05/01/02                      
*PHASE T8180BA,*                                                                
         TITLE 'T8180B - RESFM0B - MARKET RECORDS'                              
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM0B --- REP SFM MARKET RECORDS                       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* MAY11/90 (MRR) --->CHECK PROFILES AND ALLOW 1)A/N KEY, 2)ONLY   *             
*                     REQUIRE NAME.  REMOVE ARB/NSI FROM SCREEN.  *             
*                   >GET REP RECORD AND CHECK SUBSIDARY/MASTER REP*             
*                                                                 *             
* JUN18/90 (MRR) --->MAKE EACH STATION ENTRY MUTILPLE FIELDS      *             
*                                                                 *             
* JUL02/90 (MRR) --->WHEN A RECORD HAS NO STATIONS, MAKE SURE THAT*             
*                     THE OLD STATION LIST IS EMPTY               *             
*                                                                 *             
* JUL13/90 (MRR) --- >ADD SID MARKET NUMBER TO EACH STATION LINE  *             
*                    >SUPPRESS A PASSIVE POINTER FOR A STATION    *             
*                      WITH A SID MARKET NUMBER                   *             
*                                                                 *             
* AUG03/90 (MRR) --->REMOVE MISSING '8B' KEY TEST WHEN TRYING TO  *             
*                     DELETE A KEY.  PASSIVE KEY MAY NOT BE THERE *             
*                     CAUSE IT HAD A SID #.                       *             
*                                                                 *             
* OCT29/93 (BU ) --->REMOVE REFERENCES TO SID, ARB, AND NSI FIELDS*             
*                                                                 *             
* APR11/94 (BU ) --->FIX PAGE 2 BUG FOR SUBSIDIARY LISTING        *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
T8180B   CSECT                                                                  
         NMOD1 0,**180B**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,XRECADD        ADD PASSIVE POINTER                          
         BE    XADD                                                             
         CLI   MODE,XRECPUT        ADD/DELETE PASSIVE POINTER                   
         BE    XPUT                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         CLI   MODE,RECDEL         DELETE                                       
         BE    *+12                                                             
         CLI   MODE,RECREST        AND RESTORE ARE INVALID                      
         BNE   XIT                                                              
         MVC   RERROR(2),=AL2(INVACT)                                           
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         BAS   RE,CHECKREP                                                      
         BZ    VK05                                                             
         MVC   RERROR(2),=AL2(MASTLOCK)                                         
         LA    R2,CONRECH                                                       
         B     ERREND                                                           
VK05     EQU   *                                                                
         XC    MKT,MKT                                                          
         SPACE 1                                                                
****************************************************************                
*    VALIDATE MARKET (REQUIRED FOR REPORT, OPTIONAL FOR LIST)  *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,MKTMKTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK10                                                             
         GOTO1 ANY                                                              
VK10     CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'80'                                                   
         DROP  RF                                                               
         BO    VK20                                                             
         MVC   RERROR(2),=AL2(NOTNUM)                                           
         TM    4(R2),X'08'                                                      
         BZ    ERREND                                                           
         MVC   RERROR(2),=AL2(INVALID)                                          
         CLI   5(R2),4             MUST BE 4 NUMERIC                            
         BNE   ERREND                                                           
VK20     EQU   *                                                                
         MVC   MKT,8(R2)                                                        
         SPACE 2                                                                
VKXIT    MVC   AIO,AIO1            BUILD MARKET KEY IN AIO1                     
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,MKT                                                     
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     DS    0H                                                               
         XC    OLDSTNS,OLDSTNS                                                  
         XC    NEWSTNS,NEWSTNS                                                  
         MVC   NEWSTNS(5),XFF                                                   
         CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BE    VR45                THERE ARE NO OLD STATIONS                    
         BAS   RE,BLDOSTN          BLD LIST OF OLD STNS FOR PP CHANGES          
         SPACE 1                                                                
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD X'01' ELEMENT                     
         MVI   ELCODE,2                                                         
         GOTO1 REMELEM             DELETE OLD X'02' ELEMENTS                    
         MVI   ELCODE,3                                                         
         GOTO1 REMELEM             DELETE OLD X'03' ELEMENTS                    
         SPACE 1                                                                
VR45     XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         USING RMKTELEM,R6                                                      
         MVC   RMKTELEM(2),=X'0126'                                             
         SPACE 1                                                                
****************************************************************                
*              VALIDATE MARKET NAME                            *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,MKTMKTNH                                                      
         GOTO1 ANY                                                              
         MVC   RMKTNAME,8(R2)                                                   
         OC    RMKTNAME,SPACES                                                  
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
********************************************************************            
*              VALIDATE SIGN ON IDS (OPTIONAL - UP TO 4 ALLOWED)   *            
********************************************************************            
         SPACE 1                                                                
         LA    R2,MKTSIDH                                                       
         LA    R5,4                4 POSSIBLE SIGN ON ID FIELDS                 
         SPACE 1                                                                
VR46     CLI   5(R2),0                                                          
         BE    VR47                                                             
         SPACE 1                                                                
         XC    ELEM,ELEM           BUILD 03 ELEMENTS                            
         LA    R6,ELEM                                                          
         USING RMKTSOEL,R6                                                      
         MVC   RMKTSOEL(2),=X'030C'                                             
         MVC   WORK,SPACES                                                      
         ZIC   R3,5(R2)            LENGTH OF FIELD                              
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         SPACE 1                                                                
         BAS   RE,GETID                                                         
         MVC   RMKTSO,8(R2)                                                     
         OC    RMKTSO,SPACES                                                    
         MVC   RMKTSID,HALF                                                     
         DROP  R6                                                               
         SPACE 1                                                                
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR46                                                          
         SPACE 1                                                                
****************************************************************                
*              VALIDATE STATIONS                               *                
*                  - 1 REQUIRED UNLESS PROFILE SAYS NO         *                
****************************************************************                
         SPACE 1                                                                
VR47     LA    R2,MKTSTATH                                                      
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'80'                                                   
         DROP  RF                                                               
         BO    VR48                                                             
         GOTO1 ANY                                                              
VR48     EQU   *                                                                
         LA    R4,1                                                             
         SPACE 1                                                                
VR50     CLI   5(R2),0                                                          
         BE    VR90                                                             
         XC    ELEM,ELEM           NOW BUILD X'02' ELEMENTS                     
         LA    R6,ELEM                                                          
         USING RMKTSTEL,R6                                                      
         MVC   RMKTSTEL(2),=X'0214'                                             
         STC   R4,RMKTSSEQ         SEQUENCE NUMBER (ORDER IS IMPORTANT)         
         SPACE 1                                                                
         LA    RE,8(R2)                                                         
         LA    R3,RMKTSTAT                                                      
         OC    0(5,R3),SPACES                                                   
         MVC   0(3,R3),0(RE)                                                    
         LA    RE,3(RE)                                                         
         CLI   0(RE),C' '                                                       
         BE    VR55                                                             
         CLI   0(RE),0                                                          
         BE    VR55                                                             
         CLI   0(RE),C'-'                                                       
         BE    VR52                                                             
         MVC   3(1,R3),0(RE)                                                    
         LA    RE,1(RE)                                                         
         CLI   0(RE),C'-'                                                       
         BNE   VR55                                                             
VR52     EQU   *                                                                
         LA    RE,1(RE)                                                         
         MVC   RMKTSTAT+4(1),0(RE) MOVE THE BAND                                
VR55     EQU   *                                                                
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R3,NEWSTNS                                                       
VR70     EQU   *                                                                
         CLI   0(R3),X'FF'                                                      
         BE    VR75                                                             
         LA    R3,5(R3)                                                         
         B     VR70                                                             
VR75     EQU   *                                                                
         MVC   0(5,R3),RMKTSTAT                                                 
         MVC   5(5,R3),XFF                                                      
         LA    R4,1(R4)                                                         
VR80     EQU   *                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    RE,MKTLSTH          LAST FIELD ON SCREEN                         
         CR    R2,RE               ARE WE AT END                                
         BL    VR50                                                             
         DROP  R6                                                               
         SPACE 1                                                                
VR90     EQU   *                                                                
VRXIT    B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 2                                                                
DKEY     DS    0H                                                               
         L     R6,AIO              RECORD SELECTED                              
         USING RMKTKEY,R6                                                       
         SPACE 1                                                                
         LA    R2,MKTMKTH          MARKET CODE                                  
         MVC   8(L'RMKTKMKT,R2),RMKTKMKT                                        
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         MVC   MKT,RMKTKMKT        SAVE FOR LATER USE                           
         SPACE 1                                                                
DKXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RMKTELEM,R6                                                      
         LA    R2,MKTMKTNH         DISPLAY MARKET NAME                          
         MVC   8(L'RMKTNAME,R2),RMKTNAME                                        
         OI    6(R2),X'80'          TRANSMIT FIELD                              
         DROP  R6                                                               
         SPACE 1                                                                
         LA    R2,MKTSIDH          CLEAR OUT SIGN ON ID FIELDS                  
         LA    R3,4                                                             
DREC5    ZIC   R1,0(R2)                                                         
         SH    R1,=H'17'           (EXTENDED FIELD HEADER)                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         ZIC   RE,0(R2)            NEXT SIGN ON ID FIELD                        
         AR    R2,RE                                                            
         BCT   R3,DREC5                                                         
         SPACE 1                                                                
         LA    R2,MKTSIDH          DISPLAY SIGN ON ID FIELD                     
         LA    R3,4                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        LOOK FOR SIGN-ON ID ELEMENTS                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR10     BAS   RE,NEXTEL                                                        
         BNE   DR30                                                             
         SPACE 1                                                                
         USING RMKTSOEL,R6                                                      
         MVC   8(L'RMKTSO,R2),RMKTSO                                            
         SPACE 1                                                                
         ZIC   RE,0(R2)            NEXT SIGN ON ID FIELD                        
         AR    R2,RE                                                            
         BCT   R3,DR10                                                          
         SPACE 1                                                                
         DROP  R6                                                               
         SPACE 1                                                                
DR30     EQU   *                                                                
         LA    R2,MKTSTATH         CLEAR STATION FIELDS                         
         LA    R6,MKTLSTH                                                       
DR31     EQU   *                                                                
         CR    R2,R6                                                            
         BE    DR32                                                             
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         LR    RE,RF                                                            
         SH    RE,=H'16'                                                        
         EX    RE,DRSPACES                                                      
         AR    R2,RF                                                            
         B     DR31                                                             
DR32     EQU   *                                                                
         LA    R2,MKTSTATH         DISPLAY STATION FIELDS                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR60                                                             
         B     DR50                                                             
DR40     BAS   RE,NEXTEL                                                        
         BNE   DR60                                                             
         USING RMKTSTEL,R6                                                      
DR50     EQU   *                                                                
         LA    RF,8(R2)                                                         
         MVC   0(4,RF),RMKTSTAT    PUT TO SCREEN                                
         LA    RF,3(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    DR51                                                             
         CLI   0(RF),0                                                          
         BE    DR51                                                             
         LA    RF,1(RF)                                                         
DR51     EQU   *                                                                
         CLI   RMKTSTAT+4,C' '     BAND?                                        
         BE    DR52                                                             
         CLI   RMKTSTAT+4,0                                                     
         BE    DR52                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(1,RF),RMKTSTAT+4                                               
         LA    RF,2(RF)                                                         
DR52     EQU   *                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR40                                                             
         SPACE 1                                                                
*     NOW CLEAR THE REST OF THE STATION FIELDS                                  
         SPACE 1                                                                
DR60     LA    RF,MKTLSTH          LAST FIELD ON SCREEN                         
DR70     CR    R2,RF               ARE WE THERE YET                             
         BNL   DRXIT                                                            
         MVC   8(L'RMKTSTAT,R2),SPACES                                          
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DR70                                                             
         SPACE 1                                                                
DRXIT    B     XIT                                                              
         SPACE 2                                                                
DRSPACES MVC   8(0,R2),SPACES                                                   
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
LR5      LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         OC    KEY(27),KEY         FIRST TIME THROUGH?                          
         BZ    LR7                 YES                                          
         MVC   RMKTKREP,AGENCY     NO  - FORCE AGENCY INTO KEY                  
         B     LR10                                                             
LR7      EQU   *                                                                
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         LA    R2,MKTMKTH                                                       
         CLI   5(R2),0             DID THEY FILTER ON A MARKET                  
         BE    *+10                                                             
         MVC   RMKTKMKT,8(R2)                                                   
         MVC   SAVEKEY,KEY                                                      
LR10     GOTO1 HIGH                                                             
LR15     CLC   KEY(23),SAVEKEY     CORRECT REP                                  
         BNE   LRXIT                                                            
         SPACE 1                                                                
LR30     MVC   LISTAR,SPACES       CLEAR OUT LIST LINE                          
         LA    R2,LISTAR                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR40                                                             
         LA    R2,P                OR PRINT LINE FOR HARDCOPY REPORT            
         MVC   P,SPACES                                                         
         USING LISTD,R2                                                         
LR40     MVC   LCODE,RMKTKMKT          MARKET CODE                              
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RMKTELEM,R6                                                      
         SPACE 1                                                                
LR50     MVC   LNAME,RMKTNAME          MARKET NAME                              
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        NOW GET STATIONS                             
         LA    R3,6          5 STATIONS FIT ON SCREEN -6TH GETS *               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         LA    R3,13             ALL STATIONS FIT ON REPORT                     
         LA    R4,LSTAT                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR60     BAS   RE,NEXTEL                                                        
         USING RMKTSTEL,R6                                                      
         SPACE 1                                                                
         BNE   LR90                NO MORE STATIONS                             
         BCT   R3,LR80                                                          
         MVI   LSTATX,C'*'                                                      
         B     LR90                                                             
LR80     MVC   0(L'RMKTSTAT,R4),RMKTSTAT                                        
         LA    R4,6(R4)                                                         
         B     LR60                                                             
         DROP  R6                                                               
         SPACE 1                                                                
LR90     CLI   MODE,PRINTREP       FOR REPORT, INFO IS ALREADY IN P             
         BNE   LR150                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         L     R6,AIO              ALSO ON REPORT, ON 2ND LINE, SHOW            
         MVI   ELCODE,X'03'        SIGN ON IDS, IF ANY                          
         LA    R4,LSTAT                                                         
         BAS   RE,GETEL                                                         
         BNE   LR200                                                            
         B     LR110                                                            
LR100    BAS   RE,NEXTEL                                                        
         USING RMKTSOEL,R6                                                      
         SPACE 1                                                                
         BNE   LR120               NO MORE STATIONS                             
LR110    MVC   0(L'RMKTSO,R4),RMKTSO                                            
         LA    R4,10(R4)                                                        
         B     LR100                                                            
         DROP  R6                                                               
         SPACE 1                                                                
LR120    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE 1                                                                
LR150    GOTO1 LISTMON             FOR LIST                                     
         SPACE 1                                                                
LR200    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R6,KEY                                                           
         B     LR15                                                             
         SPACE 1                                                                
LRXIT    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
* MARKET RECORD ADDED - ADD PASSIVE POINTERS FOR EVERY STATION *                
****************************************************************                
         SPACE 3                                                                
XADD     DS    0H                                                               
         MVC   DA,KEY              DISK ADDRESS IS RETURNED IN KEY              
         LA    R3,NEWSTNS          LIST OF STATIONS TO BE ADDED                 
         BAS   RE,PPADD                                                         
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*     MARKET RECORD CHANGED - ADD OR DELETE PASSIVE POINTERS   *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
XPUT     EQU   *                                                                
         MVC   DA,DMWORK+4         SAVE DISK ADDRESS FOR ADD                    
         LA    R1,OLDSTNS          SORT STATIONS FOR COMPARISON                 
         SR    R3,R3               COUNT NUMBER OF OLD STATIONS                 
XP10     EQU   *                                                                
         CLI   0(R1),X'FF'                                                      
         BE    XP11                                                             
         LA    R3,1(R3)                                                         
         LA    R1,5(R1)                                                         
         B     XP10                                                             
XP11     EQU   *                                                                
         LTR   R3,R3                                                            
         BZ    XP15                                                             
         GOTO1 XSORT,DMCB,OLDSTNS,(R3),5,5,0                                    
XP15     EQU   *                                                                
*                                                                               
         LA    R1,NEWSTNS                                                       
         SR    R3,R3               COUNT NUMBER OF NEW STATIONS                 
XP20     EQU   *                                                                
         CLI   0(R1),X'FF'                                                      
         BE    XP21                                                             
         LA    R3,1(R3)                                                         
         LA    R1,5(R1)                                                         
         B     XP20                                                             
XP21     EQU   *                                                                
         LTR   R3,R3                                                            
         BZ    XP25                                                             
         GOTO1 XSORT,DMCB,NEWSTNS,(R3),5,5,0                                    
XP25     EQU   *                                                                
         SPACE 1                                                                
*          CREATE LIST OF STATIONS DELETED FROM RECORD (DELSTNS) AND            
*                 LIST OF STATIONS ADDED TO RECORD (ADDSTNS)                    
         LA    R1,OLDSTNS                                                       
         LA    R3,NEWSTNS                                                       
         LA    RE,ADDSTNS                                                       
         LA    RF,DELSTNS                                                       
         CLI   0(R3),X'FF'         NO NEW STATIONS IF LIST IS EMPTY             
         BNE   XP30                 SO CHECK FOR OLD                            
         CLI   0(R1),X'FF'         NO OLD STATIONS EITHER, ALL DONE             
         BE    XP100                ELSE DELETE THEM                            
         B     XP40                                                             
XP30     EQU   *                                                                
         CLC   0(5,R1),0(R3)                                                    
         BL    XP40                STATION IN OLD LIST IS GONE                  
         BH    XP50                STATION IN NEW LIST IS NEW                   
         BE    XP60                STATION HAS NOT CHANGED                      
XP40     MVC   0(5,RF),0(R1)       DELETE LIST                                  
         LA    R1,5(R1)                                                         
         LA    RF,5(RF)                                                         
         B     XP30                                                             
XP50     MVC   0(5,RE),0(R3)       ADD LIST                                     
         LA    R3,5(R3)                                                         
         LA    RE,5(RE)                                                         
         B     XP30                                                             
XP60     CLI   0(R1),X'FF'                                                      
         BE    XP100                                                            
         LA    R1,5(R1)                                                         
         LA    R3,5(R3)                                                         
         B     XP30                                                             
XP100    MVI   0(RE),X'FF'                                                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
         LA    R3,ADDSTNS          ANY STATIONS ADDED                           
         CLI   0(R3),X'FF'                                                      
         BE    XP110               NO                                           
         CLI   0(R3),X'00'                                                      
         BE    XP110               NO                                           
         BAS   RE,PPADD            YES, ADD PASSIVE POINTERS                    
*                                                                               
XP110    EQU   *                                                                
         LA    R3,DELSTNS          ANY STATIONS DELETED                         
         CLI   0(R3),X'FF'                                                      
         BE    XIT                 NO                                           
         CLI   0(R3),X'00'                                                      
         BE    XIT                 NO                                           
         BAS   RE,PPDEL            YES, DELETE PASSIVE POINTERS                 
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*  ROUTINE TO READ CONTROL FILE AND VALIDATE SIGN ON ID        *                
****************************************************************                
GETID    NTR1                                                                   
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         XC    0(25,R4),0(R4)      BUILD CONTROL FILE KEY                       
         MVI   0(R4),C'I'                                                       
         MVC   15(10,R4),SPACES                                                 
         MVC   15(8,R4),WORK                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                   
         MVC   AIO,AIO1            RESET AIO                                    
         MVC   RERROR(2),=AL2(INVALID)                                          
         CLI   DMCB+8,0                                                         
         BNE   ERREND              NO ID FOUND                                  
*                                                                               
         LA    R4,28(R4)                                                        
         SR    R5,R5                                                            
*                                                                               
TESTELE  CLI   0(R4),0                                                          
         BE    ERREND              NO ID FOUND                                  
         CLI   0(R4),2                                                          
         BNE   NEXTELE                                                          
         MVC   HALF,2(R4)          ID FOUND                                     
         B     XIT                                                              
*                                                                               
NEXTELE  IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     TESTELE                                                          
         EJECT                                                                  
******************************************************************              
*  ROUTINE TO CHECK FOR A SUBSIDARY REP NO-ACCESS TO THIS RECORD *              
*                                                                *              
*  ZERO RETURN IS ACCESS                                         *              
*  NON-ZERO IS NO ACCESS                                         *              
*                                                                *              
******************************************************************              
         SPACE 2                                                                
CHECKREP NTR1                                                                   
*                                                                               
         L     R6,AIO              POINT TO THE IO AREA                         
         USING RREPREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),AGENCY                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    CREP10                                                           
         DC    H'0'                                                             
*                                                                               
CREP10   EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLC   RREPMAST(2),=X'0000'    NO MASTER/SUBSID                         
         BE    CREPOK                                                           
         CLC   RREPMAST(2),=X'4040'    NO MASTER/SUBSID                         
         BE    CREPOK                                                           
         CLC   RREPMAST(2),=X'FFFF'    THIS IS THE MASTER                       
         BE    CREPOK                  MASTER HAS ALL ACCESS                    
*                                                                               
         MVC   KEY+25(2),RREPMAST                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET THE MASTER REP                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    CREP20                                                           
         DC    H'0'                                                             
*                                                                               
CREP20   EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'03'        GET ACCESS ELEMENT                           
         LA    R6,RREPREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CREPOK              NO ELEMENT, MUST BE OK                       
*                                                                               
         CLI   RREPAMKT(R6),C'Y'   'Y' IS FULL ACCESS                           
         BE    CREPOK                                                           
         CLI   RREPAMKT(R6),C'N'   'N' IS NO ACCESS                             
         BE    CREPBAD                                                          
         CLI   RREPAMKT(R6),C' '   SPACE AND ZERO DEFAULT TO OK                 
         BE    CREPOK                                                           
         CLI   RREPAMKT(R6),X'00'                                               
         BE    CREPOK                                                           
         CLI   RREPAMKT(R6),C'D'   ONLY REMAINING VALUE IS (D)ISPLAY            
         BE    CREP50                                                           
         DC    H'0'                                                             
CREP50   EQU   *                                                                
         CLI   CONACT,C'A'         ADD?                                         
         BE    CREPBAD                                                          
         CLI   CONACT,C'C'         CHANGE?                                      
         BE    CREPBAD                                                          
*                                                                               
CREPOK   EQU   *                                                                
         SR    R0,R0                                                            
         B     CREPXIT                                                          
CREPBAD  EQU   *                                                                
         LA    R0,1                                                             
CREPXIT  EQU   *                                                                
         LTR   R0,R0                                                            
         B     XIT                                                              
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*  ROUTINE TO BUILD A LIST OF OLD STATIONS                     *                
****************************************************************                
         SPACE 2                                                                
BLDOSTN  NTR1                                                                   
*                                                                               
         XC    OLDSTNS,OLDSTNS                                                  
*                                                                               
         LA    R3,OLDSTNS                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     D30                                                              
D20      BAS   RE,NEXTEL                                                        
D30      BNE   D40                                                              
         USING RMKTSTEL,R6                                                      
         MVC   0(5,R3),RMKTSTAT                                                 
         LA    R3,5(R3)                                                         
         B     D20                                                              
D40      MVC   0(5,R3),XFF                                                      
         B     XIT                                                              
         SPACE 2                                                                
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*  ROUTINE TO ADD PASSIVE POINTERS                             *                
*  ON ENTRY, R3 POINTS TO LIST OF STATIONS TO BE ADDED         *                
****************************************************************                
         SPACE 2                                                                
PPADD    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RMKTSTYP,R5                                                      
         MVI   RMKTSTYP,X'8B'                                                   
         MVC   RMKTSREP,AGENCY                                                  
         MVC   RMKTSMKT,MKT                                                     
         SPACE 1                                                                
PA20     MVC   RMKTSSTA,0(R3)                                                   
         SPACE 1                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD COULD BE DELETED                      
         BNE   PA30                                                             
         TM    KEY+27,X'80'        IF NOT, IT'S JUST A DUP                      
         BNO   PA50                                                             
         NI    KEY+27,X'7F'        UNDELETE IT                                  
         GOTO1 WRITE                                                            
         B     PA40                                                             
         SPACE 1                                                                
PA30     XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE                                                  
         MVC   KEY+28(4),DA        USE DISK ADDRESS FROM RECORD                 
         GOTO1 ADD                 TO ADD NEW RECORD                            
PA40     LA    R3,5(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   PA20                                                             
PA50     EQU   *                                                                
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
****************************************************************                
*  ROUTINE TO DELETE PASSIVE POINTERS                          *                
*  ON ENTRY, R3 POINTS TO LIST OF STATIONS TO BE DELETED       *                
****************************************************************                
         SPACE 2                                                                
PPDEL    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RMKTSTYP,R5                                                      
         MVI   RMKTSTYP,X'8B'                                                   
         MVC   RMKTSREP,AGENCY                                                  
         MVC   RMKTSMKT,MKT                                                     
         SPACE 1                                                                
PD20     MVC   RMKTSSTA,0(R3)                                                   
         GOTO1 HIGH                READ FOR EXISTING POINTER                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   XIT                                                              
         OI    KEY+27,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         LA    R3,5(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   PD20                                                             
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,52,C'MARKET LISTING'                                          
         SSPEC H2,52,C'--------------'                                          
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R2,MKTMKTH                                                       
         CLI   5(R2),0                                                          
         BE    HK20                                                             
         MVC   H3(18),=C'STARTING AT MARKET'                                    
         MVC   H3+19(4),8(R2)      MARKET CODE                                  
         SPACE 1                                                                
HK20     LA    R3,H6               HEADLINES                                    
         MVC   0(10,R3),LMKHD1                                                  
         MVC   28(18,R3),LMKHD2                                                 
         LA    R3,H7                                                            
         MVC   0(108,R3),DASH                                                   
         MVC   28(11,R3),=C'SIGN ON IDS'                                        
         SPACE 1                                                                
HKXIT    B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
PACK     ST    RE,FULL                                                          
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         MVC   RERROR(2),=AL2(MISSING)                                          
         LTR   R1,R1               ZERO LENGTH IS ERROR                         
         BZ    ERREND                                                           
         MVC   RERROR(2),=AL2(NOTNUM)                                           
         TM    4(R2),X'08'         SO IS  NON-NUMERIC                           
         BZ    ERREND                                                           
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)         * EXECUTED *                                 
PACKX    L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR                                                          
         SPACE 3                                                                
RELO     DS    A                                                                
DASH     DC    110C'-'                                                          
XFF      DC    5X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LCODE    DS    CL4                                                              
         DS    CL2                                                              
LNAME    DS    CL20                                                             
         DS    CL2                                                              
**LARB     DS    CL4                                                            
**         DS    CL1                                                            
**LNSI     DS    CL4                                                            
**         DS    CL1                                                            
LSTAT    DS    CL5                 STATION 1                                    
         DS    CL1                                                              
         DS    CL5                 STATION 2                                    
         DS    CL1                                                              
         DS    CL5                 STATION 3                                    
         DS    CL1                                                              
         DS    CL5                 STATION 4                                    
         DS    CL1                                                              
         DS    CL5                 STATION 5                                    
         DS    CL1                                                              
LSTATX   DS    CL1                 INDICATES MORE THAN 5 STATIONS               
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMWTWA                                                                     
* RESFMFBD                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMEBD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWTWA                                                      
         EJECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
SAVEKEY  DS    CL27                                                             
MKT      DS    CL4                 MARKET CODE                                  
ARBMKT   DS    H                                                                
NSIMKT   DS    H                                                                
DA       DS    CL4                 DISK ADDRESS OF MARKET RECORD                
OLDSTNS  DS    CL110     OLD 21 STATIONS X 5 + X'FFFFFFFFFF' AT END             
NEWSTNS  DS    CL110     NEW 21 STATIONS X 5 + X'FFFFFFFFFF' AT END             
ADDSTNS  DS    CL106     ADD 21 STATIONS X 5 + X'FF' AT END                     
DELSTNS  DS    CL106     DELETE 21 STATIONS X 5 + X'FF' AT END                  
         EJECT                                                                  
RRECORD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
*  DEDBLOCK                                                                     
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*  DDFLDIND                                                                     
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052RESFM0BS  05/01/02'                                      
         END                                                                    
