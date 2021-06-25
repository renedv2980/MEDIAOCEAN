*          DATA SET ACSCR0D    AT LEVEL 021 AS OF 08/27/15                      
*PHASE T60C0DA,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 21 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
         TITLE 'USER DEFINED KEYWORDS MAINTANANCE'                              
T60C0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0D,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECKWD      IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,KWDCODEH         FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT           TABLE     OF   SCREEN    UPDATES   1         
         CLI   INSCRN,SCRKWD       1ST  SCREEN ?                                
         BE    SCR05               YES, SKIP                                    
         LA    R2,SCRTXT2          TABLE     OF   SCREEN    UPDATES   2         
*                                                                               
SCR05    CLI   0(R2),X'FF'         END  OF   TABLE ?                            
         BE    SCR10               FINISHED                                     
         L     R4,0(,R2)           GET  HEADER    ADDRESS                       
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT  FIELD                              
         L     R3,4(,R2)           GET  SCREEN    TEXT NUMBER                   
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO   NEXT                               
         B     SCR05                                                            
         EJECT ,                                                                
         SPACE 1                                                                
*                                                                               
         USING KWDRECD,R2                                                       
*                                                                               
SCR10    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALIDATE KEY                                 
         B     VALREC              VALIDATE RECORD                              
         B     DISKEY              DISPLAY KEY FOR LIST                         
         B     DISREC              DISPLAY RECORD                               
         B     DELREC              DELETE                                       
         B     RESREC              RESTORE                                      
         B     VALSEL              VALSEL                                       
         B     GETSEL              GETSEL                                       
         B     DISSEL              DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*        TM    TWAMODE,TWAMLSM     LIST/SELECT MODE?                            
*        BZ    EXIT30                                                           
*        CLI   APPFKEY,PFKEXIT     EXIT TO LIST                                 
*        BE    EXIT99                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
*        NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APMODE,APMSWP           SWAP                                     
         MVC   APPARM(1),TWASWPRE      SWAP RECORD                              
         MVC   APPARM+1(1),TWASWPAC    SWAP ACTION                              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLI   APMODE,APMVALR      IN   VALIDATE  RECORD                        
         BNE   EXIT999             NO,  SKIP                                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
         BNE   EXIT999             YES, SKIP                                    
         CLI   APPFKEY,0           ANY  PF   KEY  DEPRESSED ?                   
         BNE   EXIT999             YES, SKIP                                    
         TM    TWASWPST,TWASWAP    SWAP TO   NEW  RECORD ACTION ?               
         BO    EXIT999             YES, SKIP                                    
         MVC   APCURSOR,ACURSOR    NO,  SET  APPLICATION CURSOR                 
*                                                                               
EXIT999  CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
*                                                                               
         XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   KWDKEY,SPACES                                                    
         MVI   KWDKTYP,KWDKTYPQ    X'2D'                                        
         MVI   KWDKSUB,KWDKSUBQ    X'06'                                        
         MVC   KWDKCPY,CUABIN      COMPANY CODE                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,KWDCODEH                                                   
         BNE   EXIT                                                             
         GOTO1 VALDEF,KWDCODEH                                                  
         BE    IVALDUPK                                                         
         MVC   KWDKCODE,FVIFLD                                                  
*                                                                               
         MVC   APRECKEY(L'KWDKEY),KWDKEY                                        
         LA    R1,IORDD+IOACCFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR?                                    
         BE    VALKEY10                                                         
*                                                                               
         CLI   APACTN,ACTADD       ACTION ADD?                                  
         BNE   VALKEY99                                                         
         TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            YES SO CAN'T ADD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         L     R2,AIOAREA1                                                      
         XC    KWDKEY(256),KWDKEY  RESET AIO AREA                               
         B     VALKEY98                                                         
*                                                                               
VALKEY10 CLI   APACTN,ACTADD       ACTION ADD?                                  
         BE    IVALRAE             RECORD ALREADY EXISTS                        
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,FFNELQ     X'25'                                        
         GOTO1 GETEL,(R2)                                                       
         BNE   IVALRPTY                                                         
         CLC   APREPCDE,FFNUMBER-FFNELD+1(R1)                                   
         BNE   IVALRPTY                                                         
*                                                                               
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         CLI   APACTN,ACTRES       ACTION RESTORE?                              
         BE    VALKEY98            SOME ERROR                                   
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         CLI   APACTN,ACTCHA                                                    
         BE    VALKEY98                                                         
         MVI   APINDS,APIOKDIS+APIOKDEL                                         
         CLI   APACTN,ACTDEL                                                    
         BE    VALKEY98                                                         
         MVI   APINDS,APIOKDIS                                                  
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
*-------------------------------------------------------*                       
*        DISPLAY KEY IN KEY FIELD                       *                       
*-------------------------------------------------------*                       
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   KWDCODE,KWDKCODE                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
*------------------------------*                                                
*        DELETE  RECORD        *                                                
*------------------------------*                                                
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         OI    KWDRSTA,X'80'       MARK AS DELETED                              
         GOTO1 ADDID,APPARM,(R2)                                                
         BNE   VALREC99            ON   ERROR, EXIT                             
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT ,                                                                
*------------------------------*                                                
*        RESTORE RECORD        *                                                
*------------------------------*                                                
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         NI    KWDRSTA,TURNOFF-X'80'       UNMARK AS DELETED                    
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT ,                                                                
*------------------------------*                                                
*        VALIDATE RECORD       *                                                
*------------------------------*                                                
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,KWDDSCPH                                                   
         BNE   VALREC02            NAME HAS  NOT  BEEN INPUT                    
         GOTO1 ADDNAME,APPARM,(R2),KWDDSCPH  ADD  KEYWORD DISCRIPTION           
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
         USING REPTABD,R3                                                       
         USING KYWDD,R4                                                         
         USING FFNELD,R9                                                        
*                                                                               
VALREC02 MVC   KWDKEY,APRECKEY                                                  
         LA    R9,APELEM           BUILD DEFAULT PROFILE DATA                   
         MVI   APELCODE,FFNELQ     X'25'                                        
         GOTO1 GETEL,(R2)                                                       
         BE    VALREC05                                                         
         XC    APELEM,APELEM       CLEAR AREA                                   
         MVI   FFNEL,FFNELQ        X'25'                                        
         MVI   FFNLN,FFNLN2Q       ELEMENT LENGTH                               
         MVI   FFNUMBER,C'M'       MULTI LEDGER                                 
         MVC   FFNUMBER+1(L'APREPCDE),APREPCDE                                  
         GOTO1 ADDEL,(R2)          ADD REPORT TYPE ELEMENT X'25'                
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
         USING SCMELD,R9                                                        
*                                                                               
VALREC05 XC    APELEM,APELEM                                                    
         MVI   SCMEL,SCMELQ        X'3E'                                        
         MVI   ELEMSEQ,C'H'        HELP TYPE                                    
         GOTO1 DELEL,(R2)          DELETE CURRENT COMMENT                       
         GOTO1 AFVAL,KWDHLPDH                                                   
         BNE   VALREC10            NO NEW COMMENT                               
         MVI   SCMSEQ,C'H'         HELP TYPE                                    
         SR    RF,RF               ADD HELP COMMENT                             
         IC    RF,FVXLEN                                                        
         EXMVC RF,SCMNARR,FVIFLD                                                
         LA    RF,SCMLN1Q+1(,RF)                                                
         STC   RF,SCMLN            LENGTH OF ELEMENT                            
         GOTO1 ADDEL,(R2)          ADD HELP COMMENT ELEMENT X'3E'               
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
         USING LIDELD,R9                                                        
*                                                                               
VALREC10 LA    R4,KWDFRSTH                                                      
         XC    APELEM,APELEM       CLEAR ELEMENT                                
         XC    MULTLDG,MULTLDG     CLEAR FLAG                                   
         MVI   KEYWDTYP,0                                                       
         MVI   LIDEL,LIDELQ        LIST DATA X'1F'                              
         L     R2,AIOAREA1                                                      
         GOTO1 DELEL,(R2)          DELETE ALL X'1F'                             
*                                                                               
VALREC12 L     R3,ACTYPTAB         LOAD REPORT TABLE ADDRESS                    
         GOTO1 AFVAL,KYWDLDGH                                                   
         BNE   VALREC30            NO LEDGER                                    
         XC    APELEM,APELEM       CLEAR ELEMENT                                
         MVI   LIDEL,LIDELQ        LIST DATA X'1F'                              
         MVI   LIDLN,LIDDACCS-LIDELD+KEYWDLN                                    
         MVI   LIDITLN,KEYWDLN                                                  
*                                                                               
VALREC14 CLI   REPCODE,EOT         END OF TABLE?                                
         BE    IVALCULR                                                         
         CLI   REPNUM,C'M'         IF MULTILEDGER ENTRY SKIP                    
         BE    VALREC15                                                         
         LR    R1,R3                                                            
         BAS   RE,EXPRPTY                                                       
         CLC   APREPCDE,SAVRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         SR    R1,R1                                                            
         IC    R1,FVXLEN           LENGTH OF PARAMETER                          
         CLI   FVILEN,2            MUST BE LENGTH TWO                           
         BNE   IVALCULR                                                         
         EXCLC R1,REPUL,FVIFLD     MATCH UNIT/LEDGER                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC18            NO, SO OK                                    
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BNZ   VALREC18            YES, SO OK                                   
*                                                                               
VALREC15 LA    R3,REPLNQ(,R3)      BUMP TO NEXT REPORT TYPE                     
         B     VALREC14            LOOP TO NEXT                                 
*                                                                               
VALREC18 LA    RF,L'REPIND                                                      
         LA    RE,MULTLDG-1(RF)    POINT TO END OF MULTLDG                      
         ICM   R1,15,REPIND                                                     
         EX    R1,*+8                                                           
         BNZ   IVALEDUP            DUPLICATE LEDGER USED                        
         TM    0(RE),0             SEE IF ALREADY HAVE LEDGER                   
         BCTR  RE,0                BUMP BACK ONE BYTE                           
         SRL   R1,8                SHIFT TO CHECK NEXT SET OF BITS              
         BCT   RF,*-18                                                          
*                                                                               
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),KYWDLDG                                               
         GOTO1 AIO,IOREAD+IOACCFIL+IO2                                          
         L     R2,AIOAREA1         RESET R2                                     
         MVC   IOKEY,0(R2)         RESET KEY                                    
         BNE   IVALCULR                                                         
         OC    MULTLDG,REPIND                                                   
         MVC   LIDDLEDG,KYWDLDG    SAVE UNIT/LEDGER                             
         MVC   APREPUL,KYWDLDG     SET UNIT/LEDGER                              
         MVC   APREPCUL,REPCUL     SET CONTRA UNIT/LEDGER                       
         MVC   APREPIND,REPIND     SET VALIDITY BITS FOR U/L                    
         GOTO1 AFVAL,KYWDCDEH                                                   
         GOTO1 VALDEF,KYWDCDEH                                                  
         BNE   IVALKEYW                                                         
         DROP  R3                                                               
*                                                                               
         USING DEFTABD,R6                                                       
         LR    R6,R1                                                            
         CLI   KEYWDTYP,0                                                       
         BNE   VALREC20                                                         
         MVC   KEYWDTYP,DEFTYPE                                                 
         NI    KEYWDTYP,DEFCHAR+DEFPACK+DEFDTE1                                 
*                                                                               
VALREC20 SR    R1,R1                                                            
         ICM   R1,1,KEYWDTYP                                                    
         BZ    IVALKEYW                                                         
         EX    R1,*+8                                                           
         BZ    IVALKEYW                                                         
         TM    DEFTYPE,0                                                        
         MVC   LIDDACCS(KEYWDLN),FVIFLD                                         
         GOTO1 ADDEL,(R2)          ADD KEYWORD DEFINITION TYPE X'1F'            
         BNE   VALREC99            ON  ERROR, EXIT                              
*                                                                               
VALREC30 LA    R4,KYWDQ(,R4)       BUMP TO NEXT UL FIELD                        
         LA    R6,KWDTABH                                                       
         CR    R4,R6                                                            
         BL    VALREC12            LOOP TO PROCESS NEXT                         
*                                                                               
VALREC60 MVC   IOKEY,SAVEKEY1                                                   
*                                                                               
VALREC95 GOTO1 ADDID,APPARM,AIOAREA1                                            
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VALREC97                                                         
         DC    H'0'                      WHAT THE HELL?                         
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD WRITE OR SOMETHING DUDE                  
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         DROP  R2,R4,R6,R9                                                      
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*              DISPLAY USER DEFINED KEYWORD DATA                      *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING KYWDD,R4                                                         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC KWDDSCPH,KWDTABH                                                 
         LA    R4,KWDFRSTH                                                      
         LA    RF,KWDTABH                                                       
*                                                                               
DISREC01 XC    KYWDDSP,KYWDDSP                                                  
         OI    KYWDDSPH+6,FVOXMT                                                
         LA    R4,KYWDQ(,R4)                                                    
         CR    R4,RF                                                            
         BL    DISREC01                                                         
         GOTO1 GETNAME,APPARM,(R2),KWDDSCPH                                     
*                                                                               
         USING SCMELD,R1                                                        
*                                                                               
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,SCMELQ     LIST DATA ELEMENT X'3E'                      
         MVI   ELEMSEQ,C'H'        H = HELP COMMENT                             
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC05                                                         
         SR    RF,RF                                                            
         IC    RF,SCMLN            ELEMENT LENGTH                               
         SH    RF,=Y(SCMLN1Q+1)                                                 
         BM    DISREC05                                                         
         CH    RF,=Y(L'KWDHLPD-1)                                               
         BL    *+8                                                              
         LA    RF,L'KWDHLPD-1                                                   
         EXMVC RF,KWDHLPD,SCMNARR  HELP COMMENT                                 
*                                                                               
         USING LIDELD,R2                                                        
*                                                                               
DISREC05 MVI   APELCODE,LIDELQ     LIST DATA ELEMENT X'1F'                      
         GOTO1 GETEL,(R2)                                                       
         LR    R2,R1               SAVE R1                                      
         BNE   DISREC30                                                         
         LA    R4,KWDFRSTH                                                      
*                                                                               
DISREC10 MVC   KYWDLDG,LIDDLEDG    MOVE IN UNIT/LEDGER                          
         MVC   KYWDCDE,LIDDACCS    MOVE IN ASSOCIATED KEYWORD                   
         MVC   APREPUL,LIDDLEDG                                                 
         MVI   KYWDCDEH+5,L'KYWDCDE     TRICK, PUT AN INPUT LENGTH              
         GOTO1 VALDEF,KYWDCDEH                                                  
         MVC   KYWDDSP,APKEYHD                                                  
         LA    R4,KYWDQ(,R4)                                                    
         GOTO1 NEXTEL,(R2)                                                      
         LR    R2,R1               SAVE R1                                      
         BE    DISREC10                                                         
*                                                                               
DISREC30 SR    RE,RE                                                            
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R1,R2,R4                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO VALIDATE SELECT RECORD                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING KWDRECD,R2                                                       
         SPACE 1                                                                
VALSEL   MVC   APRECKEY,SPACES                                                  
         MVC   LSTCDE,AC@KEYWD                                                  
         OI    LSTCDEH+6,FVOXMT                                                 
         LA    R0,LSTSEL1H                                                      
         ST    R0,APPARM           SAVE START ADDRESS                           
         LA    R1,LSTSEL2H                                                      
         SR    R1,R0                                                            
         STH   R1,APPARM+6         SAVE LENGTH OF LIST LINE                     
         MVI   APPARM+4,15         SAVE MAX NUMBER OF LINES                     
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO GET SELECT RECORD                                 *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING KWDRECD,R2                                                       
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   IOKEY,APRECKEY      SET IO KEY                                   
         CLI   APINDS,APILFLST     FIRST TIME IN?                               
         BE    GETSEL05            YES                                          
         CLI   APINDS,APILNLST     LAST TIME IN?                                
         BNE   *+10                NO                                           
*                                                                               
GETSEL05 MVC   SAVLSTKY,APRECKEY   SAVE CURRENT KEY                             
         MVC   KWDKEY,APRECKEY     MOVE KEY INTO IOKEY                          
         CLI   KWDKEY,KWDKTYPQ     X'2D'                                        
         BNE   GETSEL10                                                         
         CLI   KWDKSUB,KWDKSUBQ    X'06'                                        
         BNE   GETSEL10                                                         
         CLC   KWDKCPY,CUABIN      SAME COMPANY CODE?                           
         BE    GETSEL20                                                         
*                                                                               
GETSEL10 MVC   KWDKEY,SPACES                                                    
         MVI   KWDKTYP,KWDKTYPQ    X'2D'                                        
         MVI   KWDKSUB,KWDKSUBQ    X'06'                                        
         MVC   KWDKCPY,CUABIN      COMPANY CODE                                 
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD     RE-READ RECORD, IO OCCURED                   
         BZ    GETSEL40                                                         
         GOTO1 AIO,IORDD+IOACCFIL+IO1                                           
         BE    GETSEL80                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   GETSEL80                                                         
         B     GETSEL95                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ     CONTINUE WITH SEQUENTIAL READ?               
         BNZ   GETSEL80                                                         
*                                                                               
GETSEL60 LA    R1,IOHID+IOACCFIL+IO1                                            
         B     *+8                                                              
*                                                                               
GETSEL80 LA    R1,IOSQD+IOACCFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    GETSEL81                                                         
         TM    IOERR,IOEDEL                                                     
         BZ    GETSEL95                                                         
*                                                                               
GETSEL81 L     R2,AIOAREA1                                                      
         CLI   KWDKEY,KWDKTYPQ      X'2D'                                       
         BNE   GETSEL95                                                         
         CLI   KWDKSUB,KWDKSUBQ     X'06'                                       
         BNE   GETSEL95                                                         
         CLC   KWDKCPY,CUABIN      SAME COMPANY CODE?                           
         BNE   GETSEL95                                                         
*---------------------------------------------------------------------*         
*        REPORT TYPE FILTER                                           *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         L     R6,AIOAREA1                                                      
         MVI   APELCODE,FFNELQ     X'25' FREE FORM ELEM. (REPORT TYPE)          
         GOTO1 GETEL,(R6)                                                       
         BE    GETSEL82                                                         
         CLC   APREPCDE,AC@RCV                                                  
         BNE   GETSEL80                                                         
         B     GETSEL84                                                         
*                                                                               
GETSEL82 CLC   APREPCDE,FFNUMBER-FFNELD+1(R1)                                   
         BNE   GETSEL80                                                         
*---------------------------------------------------------------------*         
*        FILTER ON FORMAT                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GETSEL84 GOTO1 AFVAL,LSTCODEH                                                   
         BNE   GETSEL86            NO INPUT, SKIP                               
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         EXCLC R6,KWDKCODE,FVIFLD                                               
         BNE   GETSEL80                                                         
*---------------------------------------------------------------------*         
*        FILTER ON PERSON                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GETSEL86 GOTO1 AFVAL,LSTOWNH                                                    
         BNE   GETSEL90                                                         
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         GOTO1 GETPER,APPARM,(R2),(L'TEMPOWN,TEMPOWN)                           
         BNE   GETSEL80                                                         
         EXCLC R6,TEMPOWN,FVIFLD                                                
         BNE   GETSEL80                                                         
*                                                                               
GETSEL90 MVC   APRECKEY(L'KWDKEY),KWDKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSEL99                                                         
*                                                                               
GETSEL95 MVI   APMODE,APMEOFS      END OF SELECT RECORDS                        
*                                                                               
GETSEL99 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        ROUTINE TO DISPLAY SELECT RECORD                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING KWDRECD,R2                                                       
REDIS    DS    0H                                                               
         MVI   ACPFCUR,PFKNEXT     GO TO NEXT RECORD                            
*                                                                               
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           R4=A(LIST/SELECT LINE)                       
*                                                                               
         USING LISTD,R4                                                         
         TWAXC LISTDATH,LISTDATH                                                
         MVC   LISTKWD,KWDKCODE                                                 
         MVC   LISTTYP,SCRTYP                                                   
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         GOTO1 GETACT,APPARM,(R2),(L'LISTADTE,LISTADTE)                         
         TM    KWDRSTA,X'80'                                                    
         BZ    DISSEL10                                                         
         OI    LISTDATH+1,FVAHIGH                                               
*                                  RECORD DELETED                               
         GOTO1 TEXTGET,APPARM,1600,(L'LISTNME,LISTNME),0                        
         B     EXIT                                                             
*                                                                               
DISSEL10 GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT ,                                                                
         SPACE 1                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   SAVRPCDE,REPCODE                                                 
         MVC   SAVRPTYP,REPSTYPE                                                
         CLI   SAVRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('SAVRPLNQ',SAVRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
IVALKEYW MVC   FVMSGNO,=AL2(FVFKYWD)                                            
         MVC   FVXTRA(L'APKEYWRD),APKEYWRD                                      
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRPTY MVC   FVMSGNO,=AL2(ACERPTYP)     INVALID REPORT TYPE                   
         MVC   FVXTRA(3),FFNUMBER-FFNELD+1(R1)                                  
         LA    R1,SCRTYPH                                                       
         ST    R1,FVADDR                                                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALDUPK DS    0H                                                               
IVALEDUP MVC   FVMSGNO,=AL2(ACEDUPR)      DUPLICATE PARAMETER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRAE  MVC   FVMSGNO,=AL2(FVFERAE)      RECORD ALREADY EXIST                  
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCULR MVC   FVMSGNO,=AL2(ACEIVUL)      INVALID UNIT/LEDGER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  TABLES AND CONSTANTS *                                                       
***********************************************************************         
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(KWD006H-TWAD,5666)  HELP TEXT                                
         DC    X'FF'                                                            
*                                                                               
SCRTXT2  DS    0F                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
KYWDD    DSECT                                                                  
KYWDNUMH DS    CL8                                                              
KYWDNUM  DS    CL3                                                              
KYWDLDGH DS    CL8                                                              
KYWDLDG  DS    CL2                                                              
KYWDCDEH DS    CL8                                                              
KYWDCDE  DS    CL(KEYWDLN)                                                      
KYWDDSPH DS    CL8                                                              
KYWDDSP  DS    CL24                                                             
KYWDQ    EQU   *-KYWDD                                                          
         EJECT ,                                                                
LISTD    DSECT                                                                  
LISTACTH DS    CL8                 HEADER ACTION                                
LISTACT  DS    CL3                 ACTION                                       
LISTDATH DS    CL8                 HEADER FOR DATA                              
LISTDAT  DS    CL74                DATA                                         
         ORG   LISTDAT                                                          
         DS    CL2                                                              
LISTTYP  DS    CL3                                                              
         DS    CL2                                                              
LISTKWD  DS    CL8                                                              
         DS    CL2                                                              
LISTNME  DS    CL36                                                             
         DS    CL2                                                              
LISTOWN  DS    CL8                                                              
         DS    CL2                                                              
LISTADTE DS    CL9                                                              
         ORG                                                                    
LISTLNQ  EQU   *-LISTD                                                          
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
LWSD     DSECT                                                                  
MAXPARM  EQU   12                                                               
PENNY    EQU   C'P'                                                             
*                                                                               
SVREGS   DS    6A                                                               
SAVRPCDE DS    CL(L'REPCODE)                                                    
SAVRPTYP DS    CL(L'REPSTYPE)                                                   
SAVRPLNQ EQU   *-SAVRPCDE                                                       
SAVBLOCK DS    (MAXPARM)CL32                                                    
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEKEY1 DS    CL(L'KWDKEY)                                                     
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
TEMPOWN  DS    CL8                                                              
CURUL    DS    CL2                                                              
KEYWDTYP DS    AL1                                                              
MULTLDG  DS    AL4                                                              
LWSX     DS    0C                                                               
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                    DSECT FOR USER DEFINED KEYWORDS                  *         
*---------------------------------------------------------------------*         
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF0D                                                       
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRF9D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACSCR0D   08/27/15'                                      
         END                                                                    
