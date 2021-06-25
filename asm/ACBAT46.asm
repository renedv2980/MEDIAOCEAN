*          DATA SET ACBAT46    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T61B46A                                                                  
         TITLE 'OVERLAY FOR SALES/USE TAX FOR TYPE 62'                          
T61B46   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,T61B46,R8,CLEAR=YES                                    
         USING LWSD,RC                                                          
         L     R9,4(R1)                                                         
         USING WORKD,R9            R9=GLOBAL W/S                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=TWA                                       
         L     RE,0(R1)            A(SALES TAX BLOCK)                           
         ST    RE,ADTAXB                                                        
*                                                                               
         LA    R2,TAXACCTH         SET THE CURSOR                               
         CLI   CSACT,ACTINP        INPUT ?                                      
         BE    MAIN                YES                                          
*                                                                               
         ZAP   LASTBASE,=P'0'      CLEAR FIELDS                                 
         XC    LASTWKC,LASTWKC                                                  
         ZAP   TAXTOT,=P'0'                                                     
*                                                                               
         USING TXD,R4                                                           
         LA    R4,TAXLOCH                                                       
         LA    R5,MXLNES                                                        
         MVI   LINE,1                                                           
*                                                                               
CHGDIS   CLC   LINE,CSOLINE        IS THIS THE LINE TO CHANGE?                  
         BNE   CHGD20              NO                                           
         BAS   RE,TRANSMIT         TRANSMIT IT                                  
         LR    R2,R4               SET CURSOR                                   
         B     *+8                                                              
*                                                                               
CHGD20   BAS   RE,PROTECT          CLEAR AND PROTECT ALL OTHERS                 
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         LA    R4,TXLNQ(R4)        DO NEXT LINE                                 
         BCT   R5,CHGDIS                                                        
*                                                                               
         CLI   CSOMODE,CSOMPCHA    PREPARE FOR CHANGE?                          
         BE    EXIT                YES, EXIT                                    
         B     EDTDATA                                                          
*                                                                               
MAIN     MVI   CSOLINE,1           INIT FOR ADDITE                              
         CLI   0(RE),C'B'                                                       
         BE    INIT10              FIRST TIME - MUST INITIALIZE                 
         CLI   BCPFKEY,11                                                       
         BE    PF11                PF=11 NO MORE INPUT  - RETURN                
         CLI   BCPFKEY,10                                                       
         BE    PF10                PF=10 REFRESH SCREEN FOR NEXT                
         CLI   BCPFKEY,0                                                        
         BE    EDTDATA             PF=0 'ENTER' - OK TO EDIT                    
         B     BADPFK              INVALID PFKEY                                
         EJECT                                                                  
***********************************************************************         
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                   *         
***********************************************************************         
*                                                                               
INIT10   LA    RF,STXDATA                                                       
         LA    R1,STXLNQ                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         GOTO1 ANTRSES,0                                                        
*                                                                               
         GOTO1 AOVRSCR,BOPARM,(X'EE',BASOLY2H)                                  
         MVC   TAXPASS,ADTAXB                                                   
*                                                                               
         ZAP   LASTBASE,=P'0'                                                   
         XC    LASTWKC,LASTWKC                                                  
         ZAP   TAXTOT,=P'0'        CLEAR TAX TOTAL                              
*                                                                               
*                                                                               
         MVC   TAXACCT(12),STXACC+3                                             
         OC    TAXACCT,BCSPACES           JOB                                   
         MVC   TAXDOC,STXREF              REFERENCE                             
         MVC   TAXDATE,STXDTE             DATE                                  
         MVC   TAXLOC(L'STXLOC),STXLOC    LOCALITY                              
         MVC   TAXWKC,STXWKC              TAX WORKCODE                          
*                                                                               
         OC    STXNARR(L'BCSPACES),BCSPACES                                     
         CLC   STXNARR(L'BCSPACES),BCSPACES                                     
         BE    INIT40                                                           
         LA    R3,L'STXNARR        CHOP AND DISPLAY NARRATIVE                   
         LA    R4,L'TAXNAR1                                                     
*                                                                               
         GOTO1 VCHOPPER,BOPARM,((R3),STXNARR),((R4),IOAREA),2                   
         L     R5,BOPARM+8                                                      
         LTR   R5,R5                                                            
         BZ    INIT40                                                           
         MVC   TAXNAR1,IOAREA                                                   
         CH    R5,=H'1'                                                         
         BE    INIT40                                                           
         MVC   TAXNAR2,IOAREA+L'TAXNAR1                                         
*                                                                               
INIT40   OC    STXAMT,STXAMT                                                    
         BZ    INIT70                                                           
         CURED STXAMT,(11,BOWORK1),2,DMCB=BOPARM,ALIGN=LEFT,FLOAT=-             
*                                                                               
INIT70   MVC   TAXTXB,BOWORK1                                                   
*                                                                               
INIT71   LA    R2,TAXACCTH         GET CURSOR TO FIRST REQUIRED FIELD           
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         L     RE,ADTAXB          SCREEN LOADED- OK TO EDIT(NEXT TIME)          
         MVI   0(RE),C'E'                                                       
*                                                                               
         CLI   8(R2),C' '          ACCOUNT                                      
         BNH   INIT72                                                           
         LA    R2,TAXDOCH          DOCUMENT                                     
         CLI   8(R2),C' '                                                       
         BNH   INIT72                                                           
         LA    R2,TAXLOCH          LOCALITY                                     
*                                                                               
INIT72   OI    CSINDSL2,CSIACFRM                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              EDIT HEADER FIELDS                                     *         
***********************************************************************         
*                                                                               
EDTDATA  BAS   RE,VALJOB           VALIDATE THE JOB                             
         BAS   RE,VALREF           VALIDATE THE REFERENCE NUMBER                
         BAS   RE,VALDATE          VALIDATE THE DATE                            
*        BAS   RE,VALORD           VALIDATE THE ORDER                           
         EJECT                                                                  
***********************************************************************         
*              EDIT THE DETAIL                                        *         
***********************************************************************         
*                                                                               
         ZAP   SCRTOT,=P'0'        SCREEN TOTAL                                 
         MVI   GOTDATA,C'N'        INDICATE NO DATA YET                         
*                                                                               
         USING PSTD,R5                                                          
         LA    R5,PSTABLE                                                       
         LA    R0,4*MXLNES         INITIALIZE POSTING ENTRY                     
DET20    MVC   PSTACC,BCSPACES                                                  
         MVC   PSTACCN,BCSPACES                                                 
         XC    PSTPCT,PSTPCT                                                    
         ZAP   PSTAMT,=P'0'                                                     
         LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,DET20                                                         
*                                                                               
         TWAXC TAXACCNH,TAXACCNH   CLEAR NAME FIELD                             
*                                                                               
         USING TXD,R4                                                           
         USING PSTD,R5                                                          
         LA    R4,TAXLOCH          LOCALITY                                     
         LA    R5,PSTABLE                                                       
*                                                                               
DET40    BAS   RE,ANYDATA          SEE IF ANY DATA ON LINE                      
         BNE   DET100                                                           
*                                                                               
         BAS   RE,SETGET           GET OPTIONS                                  
         BAS   RE,VALLOC           VALIDATE THE LOCALITY                        
         BAS   RE,VALBAS           VALIDATE THE BASIS                           
         BAS   RE,VALTWC           VALIDATE THE TAX WORKCODE                    
         EJECT                                                                  
***********************************************************************         
*              COMPUTE THE TAX AMOUNT FOR EACH ENTRY                  *         
***********************************************************************         
*                                                                               
         LA    R0,4                MAX OF 4 POSTING PER LINE                    
*                                                                               
DET60    CLC   PSTACC,BCSPACES     NO POSTING ACCOUNT- OK TO SKIP               
         BNH   DET80                                                            
         ZAP   PSTBAS,LASTBASE                                                  
         ZAP   PL13,LASTBASE       AMOUNT                                       
         MP    PL13,PSTPCT         X PERCENT 4DP                                
         SRP   PL13,64-6,5                                                      
         ZAP   PSTAMT,PL13         TAX AMOUNT                                   
         MVC   PSTWKC,LASTWKC      WORKCODE                                     
*                                                                               
DET80    LA    R5,PSTLNQ(R5)                                                    
         BCT   R0,DET60                                                         
         MVI   GOTDATA,C'Y'                                                     
*                                                                               
DET100   CLI   CSACT,ACTINP        ACTION ITEM INPUT?                           
         BNE   DET120              NO                                           
         SR    R1,R1               YES, KEEP CSOLINE IN SYNCH                   
         IC    R1,CSOLINE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,CSOLINE                                                       
*                                                                               
DET120   LA    R4,TXLNQ(R4)        GET TO NEXT SCREEN LINE                      
         LA    R3,TAXNARRH                                                      
         CR    R4,R3               ARE WE AT END?                               
         BL    DET40               NO, PROCESS NEXT LINE                        
*                                                                               
         CLI   GOTDATA,C'Y'        YES, DO WE HAVE ANY INPUT?                   
         BNE   INIT71              NO                                           
         BAS   RE,POSTIT           YES, MAKE POSTINGS                           
*                                                                               
         CP    SCRTOT,=P'0'                                                     
         BE    DET140                                                           
         CURED SCRTOT,(11,TAXSTOT),2,DMCB=BOPARM,ALIGN=LEFT,FLOAT=-             
         OI    TAXSTOTH+6,X'80'                                                 
         MVI   TAXSTOTH+5,L'TAXSTOT                                             
*                                                                               
DET140   MVC   BOWORK1(L'TAXLOC),TAXLOC       PUT AN * IN FRONT OF              
         MVI   TAXLOC,C'*'                    FIRST UNIT TO AVOID               
         MVC   TAXLOC+1(L'TAXLOC-1),BOWORK1   DOUBLE INPUT                      
         OI    TAXLOCH+6,X'80'                                                  
*                                                                               
         CLI   CSACT,ACTCHA        ACTION ITEM/CHANGE                           
         BNE   DET160                                                           
         GOTO1 AXITSES                                                          
*                                                                               
DET160   LA    R2,TAXLOCH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$IOKNX)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE JOB                                       *         
***********************************************************************         
*                                                                               
VALJOB   DS    0H                                                               
         NTR1                                                                   
         LA    R2,TAXACCTH                                                      
         MVI   FVMINL,1                                                         
         MVI   BOFLAG1,ACIPRCLI+ACIPRPRO+ACIPRJOB                               
         XC    PSCLICOD,PSCLICOD   CLEAR OUT OLD CODE                           
         XC    PSPROCOD,PSPROCOD                                                
         XC    PSJOBCOD,PSJOBCOD                                                
         GOTO1 AVALCPJ,TAXACCTH                                                 
         BNE   ERRXIT                                                           
*                                                                               
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSCLSE    ACCOUNT CLOSED?                              
         BO    CLOSER              YES                                          
*                                                                               
         MVC   JOB,ACCODE                                                       
         MVC   JOBN,ACNAME                                                      
         MVC   TAXACCN,ACNAME                                                   
         OI    TAXACCNH+6,X'80'                                                 
*                                                                               
         LA    R4,JOB                                                           
         GOTO1 AGETJOB,BOPARM,(R4)                                              
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    XJOBER              YES, ERROR                                   
*                                                                               
         USING GOBLOCKD,R4                                                      
         USING ACTRECD,R6                                                       
         L     R4,AGOPBLK                                                       
         LA    R6,IOKEY                                                         
*                                                                               
         MVC   ACTKEY,BCSPACES     READ CLIENT RECORD                           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   ACTKACT(3),GOSELCLI                                              
         GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
*                                                                               
         MVC   CLI,ACCODE          SAVE CLIENT CODE, NAME AND OFFICE            
         MVC   CLIN,ACNAME                                                      
         MVC   OFFICE,GOEFFOFC                                                  
*                                                                               
         MVC   ACTKEY,BCSPACES     READ PRODUCT RECORD                          
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   ACTKACT(3),GOSELCLI                                              
         MVC   ACTKACT+3(3),GOSELPRO                                            
         GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
*                                                                               
         MVC   PRD,ACCODE          SAVE PRODUCT AND NAME                        
         MVC   PRNN,ACNAME                                                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        CALL GETOPT FOR TAX WORKCODE AND LOCALITY CODE               *         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
         USING GOXBLKD,R6                                                       
SETGET   NTR1                                                                   
         L     R6,AGOXBLK                                                       
         LA    R2,TXWKCH                                                        
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,TXWKCH                                                     
         CLI   FVILEN,0            DO WE HAVE A WORKCODE?                       
         BNE   SETG20              YES, USE IT                                  
*                                                                               
         MVC   TXWKC,GOTXWC                                                     
         OI    TXWKCH+6,X'80'                                                   
*                                                                               
SETG20   LA    R2,TXLOCH                                                        
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,TXLOCH                                                     
         CLI   FVILEN,0            DO WE HAVE A LOCALITY CODE?                  
         BNE   SETGX               YES, USE IT                                  
*                                                                               
         MVC   TXLOC(L'GOTXLOC),GOTXLOC                                         
         OI    TXLOCH+6,X'80'                                                   
*                                                                               
SETGX    B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE REFERENCE                                 *         
***********************************************************************         
*                                                                               
VALREF   DS    0H                                                               
         NTR1                                                                   
         MVC   DOCNO,BCSPACES      DOCUMENT(REFERENCE)                          
         LA    R2,TAXDOCH                                                       
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TAXDOCH                                                    
         BNE   ERRXIT                                                           
         MVC   DOCNO,FVIFLD                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE DATE                                      *         
***********************************************************************         
*                                                                               
VALDATE  DS    0H                                                               
         NTR1                                                                   
         LA    R2,TAXDATEH         DATE                                         
         GOTO1 AFVAL,TAXDATEH                                                   
         CLI   FVILEN,0            DO WE HAVE A DATE?                           
         BNE   VALD20              YES                                          
         GOTO1 VDATCON,BOPARM,(5,0),(5,BOWORK1)                                 
         MVC   TAXDATE,BOWORK1                                                  
         OI    TAXDATEH+6,X'80'                                                 
*                                                                               
VALD20   GOTO1 AVALDAT,TAXDATEH                                                 
         BNE   ERRXIT                                                           
         MVC   DATE,BCWORK+2                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE ORDER NUMBER                              *         
***********************************************************************         
*                                                                               
VALORD   DS    0H                                                               
         NTR1                                                                   
         MVC   ORDERNO,BCSPACES    ORDER NUMBER                                 
         LA    R2,TAXORDRH                                                      
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,TAXORDRH                                                   
         CLI   FVILEN,0                                                         
         BE    VALOX                                                            
         MVC   ORDERNO,FVIFLD                                                   
*                                                                               
VALOX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHECK FOR ANY DATA TO EDIT                             *         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
ANYDATA  DS    0H                                                               
         NTR1                                                                   
         GOTO1 AFVAL,TXLOCH        NO, CHECK FOR DATA IN ANY FIELD              
         CLI   FVILEN,0                                                         
         BNE   ANYYES                                                           
*                                                                               
         GOTO1 AFVAL,TXBASH                                                     
         CLI   FVILEN,0                                                         
         BNE   ANYYES                                                           
*                                                                               
         GOTO1 AFVAL,TXWKCH                                                     
         CLI   FVILEN,0                                                         
         BNE   ANYYES                                                           
*                                                                               
ANYNO    LTR   RB,RB                                                            
         B     ANYX                                                             
*                                                                               
ANYYES   CR    RB,RB                                                            
*                                                                               
ANYX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE 4 LEVELS OF LOCALITY                                *         
*              ON I/P R4 IS A(CURRENT LINE)                           *         
*                     R5 IS A(AREA TO BUILD POSTING INTERFACE)        *         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
         USING PSTD,R5                                                          
VALLOC   DS    0H                                                               
         NTR1                                                                   
         LA    R2,TXLOCH                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TXLOCH                                                     
         BNE   ERRXIT                                                           
*                                                                               
         USING SUTRECD,R6                                                       
         LA    R6,IOKEY            READ LEVEL ONE                               
         MVC   SUTKEY,BCSPACES                                                  
         MVI   SUTKTYP,SUTKTYPQ                                                 
         MVI   SUTKSUB,SUTKSUBQ                                                 
         MVC   SUTKCPY,CUABIN                                                   
         MVC   SUTKLOC(2),TXLOC                                                 
         MVC   LKEY,IOKEY          SAVE THE KEY                                 
*                                                                               
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BNE   BADLOC              MISSING LEVEL ONE                            
*                                                                               
         BAS   RE,GETTAX           POST ACCOUNT/NAME/PCT                        
         CLC   TXLOC+2(2),BCSPACES                                              
         BNH   EXIT                DONE IF POSTING TO LEVEL ONE                 
*                                                                               
         LA    R5,PSTLNQ(R5)       GET TO NEXT SAVE AREA                        
         MVC   IOKEY,LKEY          RESTORE THE PREVIOUS KEY                     
         MVC   SUTKLOC(4),TXLOC                                                 
         MVC   LKEY,IOKEY          SAVE THE NEW KEY                             
*                                                                               
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BNE   BADLOC              MISSING LEVEL TWO                            
*                                                                               
         BAS   RE,GETTAX           POST ACCOUNT/NAME/PCT                        
         CLC   TXLOC+4(2),BCSPACES                                              
         BNH   EXIT                DONE IF POSTING TO LEVEL TWO                 
*                                                                               
         LA    R5,PSTLNQ(R5)       GET TO NEXT SAVE AREA                        
         MVC   IOKEY,LKEY          RESTORE THE PREVIOUS KEY                     
         MVC   SUTKLOC(6),TXLOC                                                 
         MVC   LKEY,IOKEY          SAVE THE NEW KEY                             
*                                                                               
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BNE   BADLOC              MISSING LEVEL THREE                          
*                                                                               
         BAS   RE,GETTAX           POST ACCOUNT/NAME/PCT                        
         CLC   TXLOC+6(2),BCSPACES                                              
         BNH   EXIT                DONE IF POSTING TO LEVEL THREE               
*                                                                               
         LA    R5,PSTLNQ(R5)       GET TO NEXT SAVE AREA                        
         MVC   IOKEY,LKEY          RESTORE THE PREVIOUS KEY                     
         MVC   SUTKLOC,TXLOC                                                    
         MVC   LKEY,IOKEY          SAVE THE NEW KEY                             
*                                                                               
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BNE   BADLOC              MISSING LEVEL FOUR                           
*                                                                               
         BAS   RE,GETTAX           POST ACCOUNT/NAME/PCT                        
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*               POST ACCOUNT NAMES/CODES/ RATE TO TABLE                         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
         USING PSTD,R5                                                          
         USING ACTRECD,R6                                                       
GETTAX   NTR1                                                                   
         BAS   RE,GETNAME                                                       
         MVC   TXLOCN,BCWORK       DISPLAY LOCALITY NAME                        
         OI    TXLOCNH+6,X'80'                                                  
         MVC   PSTLOCN,TXLOCN      AND SAVE IN POSTING TABLE                    
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,ACTRFST                                                       
         SR    RE,RE                                                            
*                                                                               
GETT20   CLI   0(R6),0             FIND RATE RECORD                             
         BE    NORATE                                                           
         CLI   0(R6),SUTELQ                                                     
         BE    GETT40                                                           
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     GETT20                                                           
*                                                                               
         USING SUTELD,R6                                                        
GETT40   CLI   SUTLN,SUTLN2Q                                                    
         BL    *+16                                                             
         MVC   PSTACC(1),CUABIN                                                 
         MVC   PSTACC+1(14),SUTACC  SAVE THE POSTING ACCOUNT                    
         MVC   PSTLOC,IOKEY         SAVE LOCALITY KEY                           
         CLC   SUTEFF,DATE                                                      
         BH    *+16                 RATE NOT YET IN EFFECT                      
         MVC   PSTEFF,SUTEFF        SAVE EFFECTIVE DATE                         
         ZAP   PSTPCT,SUTRTE        AND RATE                                    
*                                                                               
         IC    RE,1(R6)             SEE IF ANY MORE RATES                       
         AR    R6,RE                                                            
         CLI   0(R6),SUTELQ                                                     
         BE    GETT40                                                           
*                                                                               
         OC    PSTPCT,PSTPCT       DID WE FIND A RATE?                          
         BZ    NORATE              NO                                           
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     VALIDATE THE CREDIT                          
         MVC   ACTKEY(L'PSTACC),PSTACC                                          
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
*                                                                               
         MVC   PSTACCN,ACNAME      SAVE THE NAME                                
         MVC   PSTLNNUM,CSOLINE    SAVE THIS LINE NUMBER                        
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*               GET LOCALITY ACCOUNT NAME                                       
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
GETNAME  NTR1                                                                   
         L     R6,AIO1                                                          
         LA    R6,ACTRFST                                                       
         SR    RE,RE                                                            
         MVC   BCWORK,BCSPACES                                                  
*                                                                               
GETN20   CLI   0(R6),0             FIND NAME ELEMENT                            
         BE    GETNX                                                            
         CLI   0(R6),NAMELQ                                                     
         BE    GETN40                                                           
         IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     GETN20                                                           
*                                                                               
         USING NAMELD,R6                                                        
GETN40   SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     GETNX                                                            
         MVC   BCWORK(0),NAMEREC                                                
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE BASIS                                     *         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
VALBAS   DS    0H                                                               
         NTR1                                                                   
         LA    R2,TXBASH                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TXBASH                                                     
         BNE   ERRXIT                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 AAMTVAL,BOPARM,8(R2),(R0)  VALID RATE BASIS                      
         MVC   FVMSGNO,=AL2(AE$INVAM)                                           
         CLI   0(R1),0                                                          
         BNE   ERRXIT                                                           
*                                                                               
         L     RF,BOPARM+4                                                      
         ZAP   LASTBASE,0(8,RF)                                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE TAX WORKCODE                                  *         
***********************************************************************         
*                                                                               
         USING TXD,R4                                                           
VALTWC   DS    0H                                                               
         NTR1                                                                   
         LA    R2,TXWKCH                                                        
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,TXWKCH                                                     
         BNE   ERRXIT                                                           
*                                                                               
         GOTO1 AGETWRK,TXWKC       VALIDATE WORK CODE                           
         BNE   ERRXIT                                                           
         MVC   LASTWKC,TXWKC                                                    
*                                                                               
         USING GOBLOCKD,R1                                                      
         L     R1,AGOPBLK                                                       
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         LA    R0,6                                                             
*                                                                               
VALT20   CLC   LASTWKC,0(R1)                                                    
         BE    WRKERR                                                           
         LA    R1,2(R1)                                                         
         BCT   R0,VALT20                                                        
         B     EXIT                                                             
         DROP  R1,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              BUILD POSTING RECORDS                                  *         
***********************************************************************         
*                                                                               
         USING DLDESCD,R2                                                       
POSTIT   NTR1                                                                   
         LA    R2,IOAREA+2                                                      
         USING DLDESCD,R2                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,DOCNO                                                    
         MVC   DLDSDATE,DATE                                                    
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         XC    DLDSNARR,DLDSNARR                                                
         GOTO1 AVALNAR,BOPARM,TAXNAR1H,DLDSNARR                                 
         LA    R1,DLDSNARR                                                      
         SR    R1,R2               R1 = ELEMENT - NARRATIVE                     
         AR    R1,R6               R6 = L'NARRATIVE                             
         STC   R1,DLDSLEN                                                       
         AR    R2,R1               R2 TO NEXT ELEMENT                           
*                                                                               
         USING PSTD,R5                                                          
         LA    R5,PSTABLE                                                       
         LA    R3,MXLNES           MAXIMUM NUMBER OF LINES                      
*                                                                               
POST10   LA    R0,4                NUMBER PER LINE                              
         MVI   ACTIVITY,C'N'                                                    
         ZAP   BOPL61,=P'0'                                                     
         MVC   CSOLINE,PSTLNNUM                                                 
*                                                                               
POST20   CLC   PSTACC,BCSPACES                                                  
         BE    POST40              NO ENTRY                                     
         MVI   ACTIVITY,C'Y'                                                    
*                                                                               
         CLC   ORDERNO,BCSPACES                                                 
         BNH   POST23                   NO ORDER NUMBER                         
         USING ACNOD,R2                                                         
         MVC   ACNOEL(2),=X'2508'                                               
         MVC   ACNO(6),ORDERNO                                                  
         LA    R2,8(R2)                  R2 TO NEXT ELEMENT                     
*                                                                               
         USING ACTAXEL,R2                                                       
POST23   XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY KEY                              
         SR    R1,R1                                                            
         IC    R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         CLC   PSTACC+1(2),=C'SV'                                               
         BE    POST24                                                           
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   POST26                                                           
                                                                                
         USING PAKELD,R2                                                        
POST24   MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,PSTACC                                                    
         MVC   PAKOFF,OFFICE                                                    
         MVC   PAKCON,CLI                                                       
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   *+10                                                             
         MVC   PAKCON,JOB                                                       
         MVC   PAKDATE,DATE                                                     
         MVC   PAKREF,DOCNO                                                     
         ZIC   R1,PAKLN                                                         
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
POST26   MVI   DLPSEL,X'69'        DEBIT POSTING                                
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,JOB        JOB ACCOUNT                                  
         MVC   DLPSDBNM,JOBN       JOB NAME                                     
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT                               
         MVC   DLPSCRNM,PSTACCN    CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,PSTWKC     FOR PRODUCTION USE WORKCODE                  
         AP    BOPL61,PSTAMT       FOR ITEM TOTAL                               
         AP    SCRTOT,PSTAMT       SCREEN TOTAL                                 
         AP    TAXTOT,PSTAMT       TAX TOTAL                                    
         SR    R1,R1                                                            
         IC    R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         EJECT                                                                  
*                                     SET-UP FOR CREDIT POSTING                 
*                                                                               
*                                                                               
         USING ACOTHERD,R2                                                      
POST30   MVC   ACOTEL(2),=X'230F'       BUILD 'OTHERS' ELEMENT FOR              
         MVC   ACOTNUM(13),BCSPACES     PRODUCT AND JOB                         
         MVC   ACOTNUM(6),PRD+6                                                 
         MVC   ACOTNUM+6(6),JOB+9                                               
         SR    R1,R1                                                            
         IC    R1,ACOTLEN                                                       
         AR    R2,R1                                                            
*                                                                               
         USING ACTAXEL,R2                                                       
         XC    0(ACTAXLQ1,R2),0(R2)   ADD TAX ELEMENT                           
         MVI   ACTAXEL,X'5F'                                                    
         MVI   ACTAXLEN,ACTAXLQ2                                                
         MVC   ACTAXEFF,PSTEFF        EFFECTIVE DATE                            
         ZAP   ACTAXRTE,PSTPCT        RATE                                      
         ZAP   ACTAXBAS,PSTBAS        BASIS                                     
         MVC   ACTAXLOC,PSTLOC        LOCALITY                                  
         SR    R1,R1                                                            
         IC    R1,ACTAXLEN                                                      
         AR    R2,R1                                                            
*                                                                               
         USING DLPOSTD,R2                                                       
         MVI   DLPSEL,X'6A'        CREDIT POSTING                               
         MVI   DLPSLEN,X'71'       LENGTH                                       
         MVC   DLPSDBAC,CLI        CONTRA ACCOUNT IS CLIENT                     
         MVC   DLPSDBNM,CLIN                                                    
*                                                                               
         CLC   PSTACC+1(2),=C'SX'                                               
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOB        FOR SX IT'S CLI/PRD/JOB                      
         MVC   DLPSDBNM,JOBN                                                    
*                                                                               
         MVC   DLPSCRAC,PSTACC     CREDIT ACCOUNT                               
         MVC   DLPSCRNM,PSTACCN    CREDIT ACCOUNT NAME                          
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,PSTAMT     AMOUNT                                       
         MVC   DLPSANAL,OFFICE                                                  
         SR    R1,R1                                                            
         IC    R1,DLPSLEN                                                       
         AR    R2,R1                                                            
         MVI   0(R2),0                                                          
*                                                                               
POST40   LA    R5,PSTLNQ(R5)       UP TO 4 POSTINGS PER LINE                    
         BCT   R0,POST20                                                        
*                                                                               
         CLI   ACTIVITY,C'Y'                                                    
         BNE   POST50                                                           
         LA    RF,IOAREA-1         GET LENGTH OF IOAREA                         
         SR    R2,RF                                                            
         STH   R2,BOHALF1                                                       
         MVC   IOAREA(2),BOHALF1                                                
*                                                                               
         LA    R1,BCPARM           CLEAR DISK ADDRESS                           
         XC    0(4,R1),0(R1)                                                    
         ST    R1,BOPARM+8                                                      
*                                                                               
         MVC   BOWORK1(L'DLDSREF),IOAREA+2+(DLDSREF-DLDESCD)                    
         L     RE,BOPARM+8                                                      
         MVC   BOWORK1+10(4),0(RE) DISK ADDRESS                                 
         OI    BOWORK1+10,X'80'    INDICATE TAX                                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   TAXPASS,0                                                        
         BNE   POST45                                                           
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
POST45   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
*                                                                               
POST50   LA    R2,IOAREA+2         SET R2 FOR NEXT ITEM                         
         USING DLDESCD,R2                                                       
         SR    R1,R1                                                            
         IC    R1,DLDSLEN                                                       
         AR    R2,R1                                                            
         BCT   R3,POST10           UP TO MAX ON SCREEN                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              RESTORE SAVED SCREEN                                   *         
***********************************************************************         
*                                                                               
PF11     DS    0H                                                               
         L     RE,ADTAXB                                                        
         MVI   0(RE),C'X'          SET STXMODE TO END                           
         GOTO1 AXITSES                                                          
         MVI   CSSPROG,0           RESET THE PF KEYS                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REFRESH SCREEN FOR NEXT ENTRY                          *         
***********************************************************************         
*                                                                               
PF10     DS    0H                                                               
         TWAXC TAXACCTH,TAXACCNH,PROT=Y                                         
         TWAXC TAXDOCH,TAXDOCH            CLEAR THE SCREEN                      
         TWAXC TAXDATEH,TAXDATEH                                                
         TWAXC TAXLOCH,TAXLSTH,PROT=Y                                           
         TWAXC TAXNAR1H,TAXNAR2H                                                
         TWAXC TAXSTOTH,TAXSTOTH,PROT=Y                                         
         CURED LASTBASE,(11,BOWORK1),2,DMCB=BOPARM,ALIGN=LEFT,FLOAT=-           
         B     INIT71                       SET-UP FOR NEXT                     
         EJECT                                                                  
***********************************************************************         
*              TRANSMIT A LINE DURING ITEM/CHANGE                     *         
*              R4 = A(START OF LINE)                                  *         
***********************************************************************         
*                                                                               
TRANSMIT NTR1                                                                   
         LA    R0,NFIELDS          #FIELDS/LINE                                 
*                                                                               
TRAN100  NI    4(R4),X'FF'-X'20'   MARK AS NOT PREVIOUSLY VALIDATED             
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,TRAN100                                                       
*                                                                               
TRANSX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR & PROTECT LINE                                   *         
*              R4 = A(START OF LINE)                                  *         
***********************************************************************         
*                                                                               
PROTECT  NTR1                                                                   
         LA    R0,NFIELDS          #FIELDS/LINE                                 
*                                                                               
PROT100  TWAXC 0(R4),0(R4),PROT=Y                                               
         CLI   CSOMODE,CSOMPLIN    BATCH GENERATE                               
         BE    *+8                                                              
         OI    1(R4),X'20'         PROTECT FIELD                                
         OI    4(R4),X'20'         MARK PREVIOUSLY VALIDATED                    
         MVI   5(R4),0             CLEAR INPUT LENGTH                           
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   R1,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         BCT   R0,PROT100                                                       
*                                                                               
PROTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ERROR MESSAGES                                         *         
***********************************************************************         
*                                                                               
BADLOC   MVC   FVMSGNO,=AL2(AE$RCNOF)  LOCALITY RECORD NOT FOUND                
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(8),LKEY+3                                                 
         B     ERRXIT                                                           
*                                                                               
NORATE   MVC   FVMSGNO,=AL2(AE$NOBLR)  NO TAX RATE FOUND                        
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(8),IOKEY+3                                                
         B     ERRXIT                                                           
*                                                                               
LOCKER   MVC   FVMSGNO,=AL2(AE$ACTLK)    ACCOUNT IS LOCKED                      
         MVC   FVXTRA,BCSPACES                                                  
         B     ERRXIT                                                           
*                                                                               
CLOSER   MVC   FVMSGNO,=AL2(AE$JBCLO)    ACCOUNT IS CLOSED                      
         MVC   FVXTRA,BCSPACES                                                  
         B     ERRXIT                                                           
*                                                                               
POSTER   MVC   FVMSGNO,=AL2(AE$INACP)    NOT VALID ACCOUNT FOR POSTING          
         MVC   FVXTRA,BCSPACES                                                  
         B     ERRXIT                                                           
*                                                                               
WRKERR   MVC   FVMSGNO,=AL2(AE$INWRK)    NON-BILLABLE WORKCODE                  
         MVC   FVXTRA,BCSPACES                                                  
         B     ERRXIT                                                           
*                                                                               
XJOBER   MVC   FVMSGNO,=AL2(AE$BXJOB)    X-JOB ERROR                            
         MVC   FVXTRA,BCSPACES                                                  
         B     ERRXIT                                                           
*                                                                               
BADPFK   MVC   FVMSGNO,=AL2(AE$INVPF)    INVALID PKKEY                          
         MVC   FVXTRA,BCSPACES                                                  
*                                                                               
ERRXIT   L     RD,BCSVRD           RETURN TO BASE                               
         L     RD,8(RD)            AT POINT OF OVERLAY CALL                     
*                                                                               
EXIT     ST    R2,FVADDR                                                        
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LWSD     DSECT                                                                  
ADTAXB   DS    A                   A(USERS INPUT BLOCK)                         
ACTIVITY DS    CL1                                                              
SCRTOT   DS    PL6                                                              
PL13     DS    PL13                                                             
SAVERE   DS    A                                                                
LINE     DS    X                                                                
*                                                                               
OFFICE   DS    CL2                                                              
DOCNO    DS    CL6                                                              
DATE     DS    CL3                                                              
ORDERNO  DS    CL6                                                              
GOTDATA  DS    C                   DATA INDICATOR                               
*                                                                               
JOB      DS    CL15                JOB     CODE                                 
JOBN     DS    CL36                        NAME                                 
PRD      DS    CL15                PRODUCT CODE                                 
PRNN     DS    CL36                        NAME                                 
CLI      DS    CL15                CLIENT  CODE                                 
CLIN     DS    CL36                        NAME                                 
LKEY     DS    CL64                                                             
         EJECT                                                                  
       ++INCLUDE ACBATSTAX                                                      
         EJECT                                                                  
MXLNES   EQU   10                  MAXIMUM NUMBER OF LINES                      
         DS    0F                                                               
IOAREA   DS    2000C               BUILD POSTING RECORD                         
PSTABLE  DS    (MXLNES*PSTLINE)C  TABLE OF POSTING ACCOUNTS (14 LINES)          
LWSX     DS    0C                                                               
         EJECT                                                                  
*              DSECT TO COVER INPUT LINE                                        
TXD      DSECT                                                                  
TXLOCH   DS    CL(L'TAXLOCH)                                                    
TXLOC    DS    CL(L'TAXLOC)        LOCALITY                                     
TXLOCX   DS    CL(L'TAXLOCX)                                                    
TXBASH   DS    CL(L'TAXTXBH)                                                    
TXBAS    DS    CL(L'TAXTXB)        TAX BASIS                                    
TXBASX   DS    CL(L'TAXTXBX)                                                    
TXWKCH   DS    CL(L'TAXWKCH)                                                    
TXWKC    DS    CL(L'TAXWKC)        WORKCODE                                     
TXWKCX   DS    CL(L'TAXWKCX)                                                    
TXLOCNH  DS    CL(L'TAXDATAH)                                                   
TXLOCN   DS    CL(L'TAXDATA)       LOCALITY NAME                                
TXLOCNX  DS    CL(L'TAXDATAX)                                                   
TXLNQ    EQU   *-TXD                                                            
NFIELDS  EQU   4                                                                
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER POSTING DATA FOR A LINE                           
*                                                                               
PSTD     DSECT                                                                  
PSTLNNUM DS    CL1                 LINE NUMBER                                  
PSTACC   DS    CL15                CREDIT ACCOUNT                               
PSTACCN  DS    CL36                ACCOUNT NAME                                 
PSTLOC   DS    CL14                LOCALITY                                     
PSTLOCN  DS    CL36                LOCALITY NAME                                
PSTWKC   DS    CL2                 WORKCODE                                     
PSTEFF   DS    CL3                 EFFECTIVE DATE                               
PSTPCT   DS    PL4                 PERCENT                                      
PSTBAS   DS    PL6                 BASIS                                        
PSTAMT   DS    PL6                 POSTING AMOUNT                               
PSTLNQ   EQU   *-PSTD                                                           
PSTLINE  EQU   PSTLNQ*4            4 POSSIBLE ENTRIES PER LINE                  
         EJECT                                                                  
*ACBATDSECT                                                                     
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
       ++INCLUDE ACBATEED                                                       
         ORG   OSVALS+200                                                       
* CALLING OVERLAYS (ACBAT01, ACBAT03, ACBAT3E) USE FIRST 200 BYTES              
*                                                                               
USERL    EQU   OSVALSL-200                                                      
USERAREA DS    0F                                                               
LASTBASE DS    PL6                 BASIS                                        
LASTWKC  DS    CL2                 WORKCODE                                     
TAXTOT   DS    PL6                 TOTAL TAX                                    
TAXPASS  DS    XL1                                                              
         DS    CL(USERL-(*-USERAREA))   SPARE                                   
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBAT46   05/01/02'                                      
         END                                                                    
