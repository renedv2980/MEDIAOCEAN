*          DATA SET SPCSO14    AT LEVEL 105 AS OF 11/07/03                      
*PHASE T21814A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'T21814 - CHILD SPOT WEEK MOVE (FOR TRUE POL ESTIMATES)'         
T21814   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYSTORL,T21814,R8,RR=RE,CLEAR=YES                                
         ST    RE,RELO                                                          
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,MYSTOR                                                        
         USING WMTABD,R7                                                        
*                                                                               
         CLI   MYOVNUM,X'14'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    MOVMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'14'       STORE OVERLAY NUMBER                         
         B     MAIN                                                             
*                                                                               
XIT      XIT1                                                                   
RELO     DS    A                                                                
         EJECT                                                                  
* MAIN ROUTINE HANDLES GENCON MODES AND CALLS                                   
*                                                                               
MAIN     CLI   MODE,VALKEY         IF MODE VALKEY                               
         BNE   M50                                                              
         BAS   RE,VALIKEY          THEN PROCESS VALKEY                          
         B     MX                                                               
*                                                                               
M50      CLI   MODE,PRINTREP       IF MODE PRINTREP                             
         BNE   M60                                                              
         BAS   RE,DOPREP           THEN PROCESS PRINTREP                        
         B     MX                                                               
*                                                                               
M60      CLI   MODE,RUNLAST        IF MODE RUNLAST                              
         BNE   MX                                                               
         BAS   RE,DORLAST          THEN PROCESS RUNLAST                         
*                                                                               
MX       B     XIT                                                              
*                                                                               
* THIS ROUTINE HANDLES MODE PRINTREP.                                           
*                                                                               
DOPREP   NTR1                                                                   
*                                                                               
         L     RF,TWAMASTC                                                      
         L     RE,MCSSB-MASTD(RF)  GET SSB ADDRESS                              
         USING SSBD,RE                                                          
         OI    SSBSTAT2,SSBSROLC   SET TO RECOVER COPIES                        
         DROP  RE                                                               
*                                                                               
         CLI   CONREC,C'W'         IF WEEK MOVE THEN SKIP STUFF BELOW           
         BE    DP10                                                             
*                                                                               
         L     RF,VADUMMY          USE DUMMY FOR WMTAB                          
         ST    RF,MYSTOR                                                        
         LR    R0,RF               CLEAR WMTAB                                  
         L     R1,=A(128*1024)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,ATWA             GET DCB ADDRESS                              
         L     RE,TWADCONS-T218FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE)                                        
         ST    RE,AREQDSN                                                       
         OC    0(100,RE),0(RE)                                                  
         BNZ   DP10                                                             
         L     RF,=A(REQDSN)                                                    
         MVC   0(128,RE),0(RF)     MOVE REQDSN DCB TO CORE                      
*                                                                               
         L     R2,AREQDSN          OPEN THE CORE RES DCB                        
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DP10     BAS   RE,MOVBUYS          PROCESS BUY MOVE                             
         GOTO1 CREATREP,DMCB,(RC)  CREATE REPORT                                
*                                                                               
DPX      B     XIT                                                              
*                                                                               
* THIS ROUTINE HANDLES MODE RUNLAST WHICH IS ONLY CALLED WHEN                   
* WE ARE RUNNING OFFLINE.                                                       
*                                                                               
DORLAST  NTR1                                                                   
         CLI   CONREC,C'O'         SHOULDN'T HAPPEN SO DIE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ATWA             GET DCB ADDRESS                              
         L     RE,TWADCONS-T218FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE)                                        
         ST    RE,AREQDSN                                                       
*                                                                               
         L     R2,AREQDSN          CLOSE VIRTUAL REQUEST FILE                   
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AREQDSN          FREE POOL                                    
         FREEPOOL ((2))                                                         
*                                                                               
DRX      B     XIT                                                              
*                                                                               
* THIS ROUTINE CONTAINS THE MULTI-PASS LOGIC FOR MOVING BUYS                    
* FOR THE 'FROM' BRAND TO THE 'TO' BRD.                                         
*                                                                               
MOVBUYS  NTR1                                                                   
         BAS   RE,FROMBUYS         PROCESS 'FROM' BUY RECORDS                   
*                                                                               
         CLI   DATESAME,C'Y'       IF FROMDATE = TODATE THEN SKIP               
         BE    MB10                                                             
*                                                                               
         BAS   RE,PROGRECS         PROCESS PROGRAM RECORDS                      
         BAS   RE,TOBUYS           PROCESS 'TO' BUYS                            
         BAS   RE,TOLEFT           PROCESS LEFTOVER 'TO' BUYS                   
*                                                                               
MB10     GOTO1 BLDREQ,DMCB,(RC) CREATE U3 TURNAROUND REQUESTS                   
*                                                                               
MBX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VALIKEY  NTR1                                                                   
         LA    R2,MOVMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    MOVCLTH+4,X'DF'                                                  
         NI    MOVESTH+4,X'DF'                                                  
         NI    MOVMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,MOVCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    MOVESTH+4,X'DF'                                                  
         NI    MOVMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,MOVESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    MOVMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,MOVMKTH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKFBRD                                                           
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         IF NUMERIC IT'S A MARKET NUMBER              
         BZ    VKALL                                                            
         GOTO1 VALIMKT                                                          
         B     VKMKTX                                                           
*                                                                               
VKALL    CLI   CONREC,C'O'         ONLY IF OVERNITE                             
         BNE   VKSTA                                                            
         CLC   8(3,R2),=C'ALL'     IF MARKET 'ALL' THEN SET BMKTSTA = 0         
         BNE   VKSTA                                                            
         XC    BMKT,BMKT                                                        
         XC    BSTA,BSTA                                                        
         B     VKMKTX                                                           
*                                                                               
VKSTA    GOTO1 VALISTA             OTHERWISE IT'S A STATION NAME                
*                                                                               
VKMKTX   OI    4(R2),X'20'                                                      
*                                                                               
VKFBRD   LA    R2,MOVFBRDH         VALIDATE 'FROM' BRAND FIELD                  
         GOTO1 ANY                                                              
         BAS   RE,VALBRD                                                        
         MVC   FROMQBRD(4),QBRD    SAVE CHAR AND BINARY FORMAT                  
*                                                                               
VKTBRD   LA    R2,MOVTBRDH         VALIDATE 'TO' BRAND FIELD                    
         GOTO1 ANY                                                              
         BAS   RE,VALBRD                                                        
         MVC   TOQBRD(4),QBRD      SAVE CHAR AND BINARY FORMAT                  
*                                                                               
VKFWK    LA    R2,MOVFWKH          VALIDATE 'FROM' WEEK FIELD                   
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   INVERR                                                           
         GOTO1 DATVAL,DMCB,(1,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         BAS   RE,VALDATE                                                       
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,INDATE)                              
         BAS   RE,VALSEST                                                       
         MVC   FROMDATE,INDATE                                                  
         MVC   FROMSEST,SUBEST                                                  
         MVC   FROMSTR,SUBSTART                                                 
         MVC   FROMEND,SUBEND                                                   
*                                  COMPUTE SUNDAY DATE                          
         GOTO1 ADDAY,DMCB,THISDATE,NEXTDATE,6                                   
         GOTO1 DATCON,DMCB,NEXTDATE,(2,FROMDATX)                                
*                                                                               
VKTWK    LA    R2,MOVTWKH          VALIDATE 'TO' WEEK FIELD                     
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   INVERR                                                           
         GOTO1 DATVAL,DMCB,(1,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         BAS   RE,VALDATE                                                       
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,INDATE)                              
         BAS   RE,VALSEST                                                       
         MVC   TODATE,INDATE                                                    
         MVC   TOSEST,SUBEST                                                    
         MVC   TOBOOK,SUBBOOK                                                   
         MVC   TOHADJ,SUBHADJ                                                   
         MVC   TOSTART,SUBSTART                                                 
         MVC   TOEND,SUBEND                                                     
*                                  COMPUTE SUNDAY DATE                          
         GOTO1 ADDAY,DMCB,THISDATE,NEXTDATE,6                                   
         GOTO1 DATCON,DMCB,NEXTDATE,(2,TODATX)                                  
*                                                                               
         MVI   DATESAME,C'N'       DETERMINE FLAG FOR WHETHER 'FROM'            
         CLC   FROMDATE,TODATE         AND 'TO' DATES ARE SAME                  
         BNE   VKOPT                                                            
         MVI   DATESAME,C'Y'                                                    
         CLC   FROMBBRD,TOBBRD     CAN'T HAVE BOTH BRANDS AND DATES             
         BE    ERRSAME                 BE THE SAME                              
*                                                                               
VKOPT    MVI   TRADEOPT,C'N'       SET OPTIONS TO DEFAULTS                      
         MVI   CASHOPT,C'N'                                                     
         MVI   NOWRTOPT,C'N'                                                    
         MVI   TRACEOPT,C'N'                                                    
*                                                                               
         LA    R2,MOVOPTH          IF NOTHING ENTERED THEN DONE                 
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         XC    BLOCK(256),BLOCK    SEND INPUT TO SCANNER FOR PARSING            
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROPTS                                                          
         ZIC   RF,4(R1)            SAVE NUMBER OF OPTIONS IN FULL               
         ST    RF,FULL                                                          
         LA    R4,BLOCK            R4 = CURRENT SCANNER BLOCK ENTRY             
         LA    R3,1                R3 = SCANNER BLOCK ENTRY NUMBER              
*                                                                               
VKOPT10  C     R3,FULL             IF PAST LAST OPTION THEN DONE                
         BH    VKX                                                              
*                                                                               
         CLI   1(R4),0             NEVER VALID TO HAVE EQUALS SIGN              
         BNE   ERROPTS                                                          
*                                                                               
         CLI   0(R4),4             IF 'CASH' ENTERED                            
         BNE   VKOPT20                                                          
         CLC   =C'CASH',12(R4)                                                  
         BNE   VKOPT20                                                          
         MVI   CASHOPT,C'Y'        THEN SET CASH OPTION                         
         B     VKOPT90                                                          
*                                                                               
VKOPT20  CLI   0(R4),5             IF 'TRADE' ENTERED                           
         BNE   VKOPT30                                                          
         CLC   =C'TRADE',12(R4)                                                 
         BNE   VKOPT30                                                          
         MVI   TRADEOPT,C'Y'       THEN SET TRADE OPTION                        
         B     VKOPT90                                                          
*                                                                               
VKOPT30  CLI   0(R4),4             IF 'TEST' ENTERED                            
         BNE   VKOPT40                                                          
         CLC   =C'TEST',12(R4)                                                  
         BNE   VKOPT40                                                          
         MVI   NOWRTOPT,C'Y'       THEN SET NO WRITES OPTION                    
         B     VKOPT90                                                          
*                                                                               
VKOPT40  CLI   0(R4),8             IF 'TRACEOPT' ENTERED                        
         BNE   ERROPTS                                                          
         CLC   =C'TRACEOPT',12(R4)                                              
         BNE   ERROPTS                                                          
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   ERROPTS                                                          
         MVI   TRACEOPT,C'Y'       THEN SET TRACE WRITES OPTION                 
*                                                                               
VKOPT90  LA    R3,1(R3)            BUMP TO NEXT SCANNER BLOCK ENTRY             
         LA    R4,32(R4)                                                        
         B     VKOPT10             AND LOOP BACK                                
*                                                                               
VKX      B     XIT                                                              
*                                                                               
* THIS ROUTINE MAKES SURE THE DATE IS A VALID START OF WEEK FOR THE             
* GIVEN ESTIMATE.                                                               
*                                                                               
VALDATE  NTR1                                                                   
         MVC   THISDATE(2),QSTART  FILL IN YEAR                                 
         CLC   THISDATE+2(4),QSTART+2                                           
         BNL   VD10                                                             
         MVC   THISDATE(2),QEND                                                 
*                                                                               
VD10     MVC   NEXTDATE,QSTART     START AT BEGIN OF EST                        
*                                                                               
VD20     CLC   NEXTDATE,THISDATE   IF THIS MONDAY DATE MATCHES INPUT            
         BE    VDX                     THEN INPUT IS VALID                      
*                                                                               
*                                  BUMP TO NEXT WEEK                            
         GOTO1 ADDAY,DMCB,NEXTDATE,NEXTDATE,F'7'                                
*                                                                               
         CLC   NEXTDATE,QEND       REPEAT UNTIL END OF ESIMATE                  
         BL    VD20                                                             
*                                                                               
         B     ERRDATE             ERROR - INVALID DATE                         
*                                                                               
VDX      B     XIT                 INPUT IS VALID - RETURN                      
*                                                                               
* THIS ROUTINE FINDS WHICH SUBEST CONTAINS THE INPUT DATE                       
*                                                                               
VALSEST  LA    R4,SVSUBEST         R4 = A(SUBEST LIST TABLE)                    
         USING SESTLSTD,R4                                                      
*                                                                               
VS10     CLI   SNUM,0              DIE IF WE REACH END OF SESTLST               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INDATE,SSTART       IF INPUT DATE >= SUBEST START                
         BL    VS20                                                             
         CLC   INDATE,SEND         AND <= SUBEST END THEN WE FOUND IT           
         BNH   VS30                                                             
*                                                                               
VS20     LA    R4,SESTLSTL(R4)     OTHERWISE BUMP TO NEXT SUBEST                
         B     VS10                AND LOOP BACK                                
*                                                                               
VS30     MVC   SUBEST,SNUM         SAVE SUBEST LIST ENTRY VALUES                
         MVC   SUBBOOK,SBOOK                                                    
         MVC   SUBHADJ,SHUTADJ                                                  
         MVC   SUBSTART,SSTART                                                  
         MVC   SUBEND,SEND                                                      
*                                                                               
VSX      BR    RE                                                               
         DROP  R4                                                               
*                                                                               
* THIS ROUTINE VALIDATES THE BRAND AND SAVES ITS CHAR AND BINARY FORMAT         
*                                                                               
VALBRD   LA    RF,SVCLIST          LOOP THROUGH LIST FOUND IN CLI REC           
*                                                                               
VB10     CLI   0(RF),0             IF END OF LIST THEN ERROR                    
         BE    ERRBRD                                                           
*                                                                               
         CLC   0(3,RF),WORK        IF MATCH FOUND THEN GO SAVE                  
         BE    VB20                                                             
*                                                                               
         LA    RF,4(RF)            OTHERWISE BUMP LIST AND LOOP BACK            
         B     VB10                                                             
*                                                                               
VB20     MVC   QBRD(4),0(RF)       SAVE CHAR AND BINARY FORM OF BRD             
*                                                                               
VBX      BR    RE                                                               
         EJECT                                                                  
* PASS 1: READ BUY RECORDS FOR THE 'FROM' SUB ESTIMATE AND DELETE BUY           
*         ELEMENTS FOR THE 'FROM' PRODUCT.  SAVE RESULTS IN WMTAB.              
*                                                                               
FROMBUYS NTR1                                                                   
         MVI   RECFOUND,C'N'       INITIALIZE FLAG                              
*                                                                               
         XC    KEY,KEY             BUILD BUY KEY WITH AGY/MD, CLT               
         LA    R4,KEY                                                           
         USING BUYKEY,R4                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'       ADD PRODUCT 'POL'                            
*                                                                               
         MVC   BUYMSTA,BMKT        PUT MKT, STA AND 'FROM' EST IN KEY           
         MVC   BUYKEST,FROMSEST                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
FB10     CLC   KEY(4),KEYSAVE      IF END OF CLIENT THEN DONE                   
         BNE   FB100                                                            
*                                                                               
         OC    BMKT,BMKT           IF MARKET GIVEN                              
         BZ    FB20                                                             
         CLC   BUYMSTA(2),BMKT     THEN DONE IF END OF MARKET                   
         BNE   FB100                                                            
*                                                                               
         OC    BSTA,BSTA           IF STATION GIVEN                             
         BZ    FB20                                                             
         CLC   BUYMSTA,BMKT        THEN DONE IF END OF STATION                  
         BNE   FB100                                                            
*                                                                               
FB20     CLC   BUYKEST,FROMSEST    IF EST IS LESS THAN 'FROM' SUBEST            
         BNL   FB30                                                             
         MVC   BUYKEST,FROMSEST    THEN READ HIGH FOR ESTIMATE                  
         B     FB40                                                             
*                                                                               
FB30     BNH   FB50                ELSE IF EST IS GREATER THEN 'FROM'           
         MVC   BUYKEST(4),=4X'FF'  THEN READ HIGH FOR NEXT STATION              
*                                                                               
FB40     MVI   RDUPDATE,C'N'       READ HIGH AND LOOP BACK                      
         GOTO1 HIGH                                                             
         B     FB10                                                             
*                                                                               
FB50     BAS   RE,PROCFROM         PROCESS 'FROM' BUY RECORD                    
*                                                                               
         MVI   RDUPDATE,C'N'       GET NEXT KEY AND LOOP BACK                   
         GOTO1 SEQ                                                              
         B     FB10                                                             
*                                                                               
FB100    CLI   RECFOUND,C'N'       IF NO RECORDS FOUND THEN ERROR               
         BE    ERRBNF                                                           
*                                                                               
FBX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE 'FROM' BUY RECORD TO SEE IF THERE ARE              
* ANY PRODUCTS TO REMOVE AND SAVES ITS RESULTS IN WMTAB.                        
*                                                                               
PROCFROM NTR1                                                                   
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         CLI   TRACEOPT,C'Y'       IF WE ARE TRACING                            
         BNE   PF5                                                              
         L     RE,AIO3             THEN SAVE COPY OF RECORD                     
         L     RF,=F'2000'                                                      
         L     R0,AIO                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
PF5      L     R6,AIO              R6 = A(BUY DESCRIPTION ELEMENT)              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                                                               
         TM    BDCIND,X'20'        IF TRADE BUY                                 
         BO    *+16                                                             
         CLI   CASHOPT,C'Y'        THEN SKIP IF CASH ONLY OPTION                
         BE    PFX                                                              
         B     *+12                                                             
         CLI   TRADEOPT,C'Y'       ELSE SKIP IF TRADE ONLY OPTION               
         BE    PFX                                                              
*                                                                               
         XC    SPOTS,SPOTS         INITIALIZE SPOTS ACCUM TO ZERO               
*                                                                               
         L     R6,AIO              R6 = A(FIRST BUY ELEMENT)                    
         BAS   RE,GETBYEL                                                       
         USING REGELEM,R6                                                       
         BNE   PFX                 DONE IF NONE FOUND                           
*                                                                               
PF10     CLI   RLEN,X'0E'          IF NOT ALLOCATED BUYEL THEN GET NEXT         
         BNE   PF20                                                             
         TM    RSTATUS,X'C0'       IF MINUS BUY THEN GET NEXT                   
         BNZ   PF20                                                             
         CLC   RDATE,FROMDATE      IF NOT IN FROM WEEK THEN GET NEXT            
         BL    PF20                                                             
         CLC   RDATE,FROMDATX                                                   
         BH    PF20                                                             
         OC    RPAY,RPAY           IF PAID BUY THEN GET NEXT                    
         BNZ   PF20                                                             
         CLC   RPPRD,FROMBBRD      IF SAME BRAND THEN WE FOUND ONE              
         BE    PF30                                                             
*                                                                               
PF20     BAS   RE,NXTBYEL          GET NEXT BUY ELEMENT                         
         BNE   PF100               IF NO MORE THEN DONE                         
         B     PF10                OTHERWISE TRY AGAIN FOR MATCH                
*                                                                               
PF30     L     RF,SPOTS            BUMP SPOTS ACCUM                             
         LA    RF,1(RF)                                                         
         ST    RF,SPOTS                                                         
*                                                                               
         CLI   DATESAME,C'Y'       IF FROMDATE = TODATE                         
         BNE   PF50                                                             
         MVC   RPPRD,TOBBRD        THEN CHANGE BRAND IN ELEM                    
         B     PF90                                                             
*                                  ELSE REMOVE BUYEL AND ALL SUBELS             
PF50     GOTO1 RECUP,DMCB,(0,AIO),(R6)                                          
         CLI   0(R6),0                                                          
         BE    PF100                                                            
         CLI   0(R6),X'10'                                                      
         BL    PF90                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   PF50                                                             
*                                                                               
PF90     BAS   RE,CHKBYEL          GET NEXT BUY ELEMENT                         
         BE    PF10                LOOP BACK IF FOUND ONE                       
*                                                                               
PF100    OC    SPOTS,SPOTS         IF NO SPOTS FOUND THEN DONE                  
         BZ    PFX                                                              
*                                                                               
         BAS   RE,TESTEMP          IF NO MORE BUYS LEFT THEN DEL                
         CLI   DELFLAG,C'Y'                                                     
         BE    PF107                                                            
*                                                                               
         XC    DMCB+20(4),DMCB+20  SET UP PRODUCT LIST FOR PUTREC               
         XC    DMPRDLST,DMPRDLST                                                
         CLI   DATESAME,C'Y'       TEST CHANGE OF PRODUCT                       
         BNE   PF105                                                            
         MVC   DMPRDLST(1),TOBBRD                                               
         LA    RF,DMPRDLST                                                      
         ST    RF,DMCB+20                                                       
*                                                                               
PF105    CLI   NOWRTOPT,C'Y'       WRITE RECORD BACK                            
         BE    PF107                                                            
         GOTO1 PUTREC                                                           
         XC    DMCB+20(4),DMCB+20                                               
*                                                                               
PF107    MVI   RECFOUND,C'Y'       FLAG THAT WE FOUND ONE                       
*                                                                               
         MVC   AIO,AIO3            TRACE COPY/CHANGE                            
         GOTO1 TRCREC,DMCB,1                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 TRCREC,DMCB,2                                                    
*                                                                               
         L     R6,AIO              R6 = A(BUY DESCRIPTION ELEMENT)              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                                                               
         MVC   LUMSTA,BUYMSTA      BUILD LOOKUP KEY FROM BUY KEY                
         MVC   LUDAY,BDDAY             AND DESCRIPTION ELEMENT                  
         MVC   LUTIME,BDTIMST                                                   
*                                                                               
         BAS   RE,LOOKUP           LOOKUP TABLE ENTRY FOR THIS KEY              
*                                                                               
         OC    WMKEY,WMKEY         IF TABLE ENTRY NOT FOUND                     
         BNZ   *+10                                                             
         MVC   WMKEY,LUKEY         CREATE IT                                    
*                                                                               
         TM    BDCIND,X'20'        IF TRADE BUY                                 
         BO    PF110                                                            
         ICM   RF,3,WMFTSPTS       THEN BUMP TRADE SPOTS ACCUM                  
         A     RF,SPOTS                                                         
         STCM  RF,3,WMFTSPTS                                                    
*                                                                               
         MVC   WMFTBLIN,BUYKBUY+1  SAVE BUY LINE                                
*                                                                               
         CLI   DELFLAG,C'Y'        SET DELETED FLAG FOR DELETED BUYRECS         
         BNE   *+8                                                              
         OI    WMFLAGS,WMFDLTRD                                                 
         B     PF120                                                            
*                                                                               
PF110    ICM   RF,3,WMFCSPTS       ELSE BUMP CASH SPOTS ACCUM                   
         A     RF,SPOTS                                                         
         STCM  RF,3,WMFCSPTS                                                    
*                                                                               
         MVC   WMFCBLIN,BUYKBUY+1  SAVE BUY LINE                                
*                                                                               
         CLI   DELFLAG,C'Y'        SET DELETED FLAG FOR DELETED BUYRECS         
         BNE   *+8                                                              
         OI    WMFLAGS,WMFDLCSH                                                 
*                                                                               
PF120    DS    0H                                                               
*                                                                               
PFX      B     XIT                                                              
*                                                                               
* THIS ROUTINE TESTS TO SEE IF THE BUY RECORD NO LONGER CONTAINS                
* ANY BUY LINES.  IF SO THE RECORD WILL BE PUT BACK DELETED.                    
*                                                                               
TESTEMP  NTR1                                                                   
         MVI   DELFLAG,C'N'        IF BUY ELEM FOUND THEN RETURN NO             
         L     R6,AIO                                                           
         BAS   RE,GETBYEL                                                       
         BE    TEX                                                              
*                                                                               
         MVI   DELFLAG,C'Y'        OTHERWISE RETURN YES                         
*                                                                               
         CLI   NOWRTOPT,C'Y'       SKIP IOS IF TEST OPTION                      
         BE    TEX                                                              
*                                                                               
         L     R6,AIO              OTHERWISE WRITE DELETED RECORD BACK          
         OI    BUYRCNTL-BUYREC(R6),X'80'                                        
         XC    DMCB+20(4),DMCB+20                                               
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   KEY(10),0(R6)       DELETE KEY                                   
         MVI   KEY+10,0                                                         
         MVC   KEY+11(2),10(R6)                                                 
         GOTO1 READ                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
TEX      B     XIT                                                              
         EJECT                                                                  
* PASS2 : READ PROGRAM RECORDS FOR THE MASTER ESTIMATE AND MOVE SPOTS           
*         FROM ONE WEEK TO THE OTHER, UPDATING WMTAB WITH RESULTS.              
*                                                                               
PROGRECS NTR1                                                                   
         MVI   RECFOUND,C'N'       INITIALIZE FLAGS/ACCUMS                      
         XC    CURRSTA,CURRSTA                                                  
         XC    LEFTOVER,LEFTOVER                                                
*                                                                               
         XC    KEY,KEY             FILL KEY WITH AGYMD/CLT                      
         LA    R4,KEY                                                           
         USING CSORECD,R4                                                       
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
*                                                                               
         MVC   CSOKMKT,BMKT        PUT MKT, STA AND EST IN KEY                  
         MVC   CSOKSTA,BSTA                                                     
         MVC   CSOKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
PR10     CLC   KEY(5),KEYSAVE      IF END OF CLIENT THEN DONE                   
         BNE   PR100                                                            
*                                                                               
         OC    BMKT,BMKT           IF MARKET GIVEN                              
         BZ    PR20                                                             
         CLC   CSOKMKT,BMKT        THEN DONE IF END OF MARKET                   
         BNE   PR100                                                            
*                                                                               
         OC    BSTA,BSTA           IF STATION GIVEN                             
         BZ    PR20                                                             
         CLC   CSOKSTA,BSTA        THEN DONE IF END OF STATION                  
         BNE   PR100                                                            
*                                                                               
PR20     OC    CSOKSTA,CSOKSTA     SKIP STATION PERCENTAGES RECORD              
         BNZ   PR30                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     PR10                                                             
*                                                                               
PR30     CLC   CSOKEST,BMEST       IF EST IS LESS THAN REQUESTED                
         BNL   PR40                                                             
         MVC   CSOKEST,BMEST       THEN READ HIGH FOR ESTIMATE                  
         B     PR50                                                             
*                                                                               
PR40     BNH   PR60                ELSE IF EST IS GREATER THEN REQ              
         MVC   CSOKEST(3),=3X'FF'  THEN READ HIGH FOR NEXT STATION              
*                                                                               
PR50     MVI   RDUPDATE,C'N'       READ HIGH AND LOOP BACK                      
         GOTO1 HIGH                                                             
         B     PR10                                                             
*                                                                               
PR60     BAS   RE,PROCPROG         PROCESS PROGRAM RECORD                       
*                                                                               
         MVI   RDUPDATE,C'N'       GET NEXT KEY AND LOOP BACK                   
         GOTO1 SEQ                                                              
         B     PR10                                                             
*                                                                               
PR100    CLI   RECFOUND,C'N'       IF NO RECORDS FOUND THEN ERROR               
         BE    ERRPNF                                                           
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE PROGRAM RECORD TO SEE IF THERE IS A                
* MATCHING WMTAB ENTRY, AND IF SO, MOVES SPOTS WITHIN THE RECORD.               
*                                                                               
PROCPROG NTR1                                                                   
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         CLI   TRACEOPT,C'Y'       IF WE ARE TRACING                            
         BNE   PP10                                                             
         L     RE,AIO3             THEN SAVE COPY OF RECORD                     
         L     RF,=F'2000'                                                      
         L     R0,AIO                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
PP10     L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         MVC   LUMSTA,CSOKMKT      BUILD LOOKUP KEY FROM PROG KEY               
         MVC   LUDAY,DSCDAY            AND DESCRIPTION ELEMENT                  
         MVC   LUTIME,DSCTIME                                                   
*                                                                               
         BAS   RE,LOOKUP           LOOKUP TABLE ENTRY FOR THIS KEY              
*                                                                               
         OC    WMKEY,WMKEY         IF TABLE ENTRY NOT FOUND THEN DONE           
         BZ    PPX                                                              
*                                                                               
         MVC   TSPOTS,WMFTSPTS     GET 'FROM' SPOTS FROM TABLE ENTRY            
         MVC   CSPOTS,WMFCSPTS                                                  
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         MVI   TRDONLY,C'N'        IF PROGRAM WEIGHT IS ZERO                    
         CLI   DSCWGT,0                                                         
         BNE   *+8                                                              
         MVI   TRDONLY,C'Y'        THEN SET TRADE ONLY FLAG                     
*                                                                               
         CLC   CSOKSTA,CURRSTA     IF STATION CHANGED                           
         BE    *+16                                                             
         MVC   CURRSTA,CSOKSTA                                                  
         XC    LEFTOVER,LEFTOVER   THEN CLEAR LEFTOVER DOLLARS                  
*                                                                               
         LA    R1,FROMDATE         POINT TO 'FROM' WEEK ELEMENT                 
         BAS   RE,GETWKEL                                                       
         USING CSOWKEL,R6                                                       
         MVC   WMFCOST,WKCOST      SAVE 'FROM' COST IN WMTAB                    
*                                                                               
         SR    RF,RF               SUBTRACT TRADE SPOTS FROM ELEMENT            
         ICM   RF,3,WKTSPOTS                                                    
         LH    RE,TSPOTS                                                        
         SR    RF,RE                                                            
         BNL   *+6                 DON'T GO NEGITIVE                            
         SR    RF,RF                                                            
         STCM  RF,3,WKTSPOTS                                                    
*                                                                               
         SR    RF,RF               SUBTRACT CASH SPOTS FROM ELEMENT             
         ICM   RF,3,WKCSPOTS                                                    
         LH    RE,CSPOTS                                                        
         SR    RF,RE                                                            
         BNL   *+6                 DON'T GO NEGITIVE                            
         SR    RF,RF                                                            
         STCM  RF,3,WKCSPOTS                                                    
*                                                                               
         MVC   RATE,WKCOST         SAVE RATE OF 'FROM' WEEK ELEMENT             
*                                                                               
         LA    R1,TODATE           POINT TO 'TO' WEEK ELEMENT                   
         BAS   RE,GETWKEL                                                       
         USING CSOWKEL,R6                                                       
         MVC   WMTCOST,WKCOST      SAVE 'TO' COST IN WMTAB                      
*                                                                               
         BAS   RE,ADJCSPTS         ADJUST 'FROM' CASH SPOTS FOR VALUE           
*                                                                               
         ICM   RF,3,WKCSPOTS       ADD TRADE/CASH SPOTS TO 'TO'                 
         AH    RF,CSPOTS               WEEK ELEMENT                             
         STCM  RF,3,WKCSPOTS                                                    
         ICM   RF,3,WKTSPOTS                                                    
         AH    RF,TSPOTS                                                        
         STCM  RF,3,WKTSPOTS                                                    
*                                                                               
         MVC   AIO,AIO3            TRACE COPY/CHANGE                            
         GOTO1 TRCREC,DMCB,1                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 TRCREC,DMCB,2                                                    
*                                                                               
         CLI   NOWRTOPT,C'Y'                                                    
         BE    PP90                                                             
         GOTO1 PUTREC              WRITE RECORD BACK                            
PP90     MVI   RECFOUND,C'Y'       FLAG THAT WE HAVE FOUND A PROGREC            
*                                                                               
         MVC   WMPDSKA,DMDSKADD    SAVE DISK ADDRESS IN WMTAB ENTRY             
         MVC   WMPREF,CSOKREF           PROGRAM REFERENCE NUMBER                
*                                                                               
         MVC   WMTTSPTS,TSPOTS     SAVE 'TO' SPOTS IN WMTAB ENTRY               
         MVC   WMTCSPTS,CSPOTS                                                  
*                                                                               
PPX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* THIS ROUTINE RETURNS IN R6 THE ADDRESS OF THE WEEK ELEMENT WHOSE DATE         
* IS POINTED TO BY R1                                                           
*                                                                               
GETWKEL  NTR1                                                                   
         L     R6,AIO              POINT TO FIRST WEEK ELEMENT                  
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
GW10     CLC   WKDATE,0(R1)        IF MATCH FOUND THEN RETURN R6                
         BE    GWX                                                              
         BAS   RE,NEXTEL           REPEAT UNTIL WEEKS EXAUSTED                  
         BE    GW10                                                             
         DC    H'0'                                                             
*                                                                               
GWX      XIT1  REGS=(R6)                                                        
*                                                                               
* THIS ROUTINE CHECKS TO SEE IF THE RATE OF THE 'TO' WEEK ELEMENT IS            
* DIFFERENT FROM THE RATE OF THE 'FROM' ELEMENT.  IF IT IS, IT ADJUSTS          
* THE NUMBER OF CASH SPOTS TO END UP WITH THE SAME CASH VALUE.                  
*                                                                               
ADJCSPTS NTR1                                                                   
         USING CSOWKEL,R6                                                       
         CLC   RATE,WKCOST         DO NOTHING IF RATES ARE THE SAME             
         BE    XIT                                                              
*                                                                               
         CLI   TRADEOPT,C'Y'       DO NOTHING IF TRADE ONLY OPTION              
         BE    XIT                                                              
*                                                                               
         CLI   TRDONLY,C'Y'        DO NOTHING IF TRADE ONLY PROGRAM             
         BNE   AS10                                                             
         OC    CSPOTS,CSPOTS       AND THERE AREN'T ANY CASH SPOTS              
         BZ    XIT                                                              
*                                                                               
AS10     SR    R1,R1               R1 = 'FROM' SPOTS * 'FROM' RATE              
         ICM   R1,3,CSPOTS              ---------------------------             
         M     R0,RATE                          'TO' RATE                       
         A     R1,LEFTOVER                                                      
         ICM   RF,15,WKCOST                                                     
         LTR   RF,RF               DON'T DIVIDE BY ZERO                         
         BZ    AS20                                                             
         DR    R0,RF                                                            
*                                                                               
         L     RF,RATE             IF REMAINDER (R0) > HALF OF RATE             
         SRL   RF,1                    THEN ADD ONE MORE SPOT                   
         CR    R0,RF                   (OVERSPENT BUT MORE ACCURATE)            
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         S     R0,RATE                                                          
         B     AS30                                                             
*                                                                               
AS20     LR    R0,R1               IF RATE IS ZERO THEN NUMBER OF SPOTS         
         SR    R1,R1                   IS ZERO, EVERYTHING IS LEFTOVER          
*                                                                               
AS30     STH   R1,CSPOTS           SAVE NEW NUMBER OF SPOTS                     
         ST    R0,LEFTOVER         SAVE LEFTOVER FOR NEXT TIME                  
*                                                                               
ASX      B     XIT                                                              
         EJECT                                                                  
* PASS 3: READ BUY RECORDS FOR THE 'TO' SUB ESTIMATE AND ADD BUY                
*         ELEMENTS FOR THE 'TO' PRODUCT.                                        
*                                                                               
TOBUYS   NTR1                                                                   
         XC    KEY,KEY             BUILD BUY KEY WITH AGY/MD, CLT               
         LA    R4,KEY                                                           
         USING BUYKEY,R4                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
*                                                                               
         MVC   BUYMSTA,BMKT        PUT MKT, STA AND 'TO' EST IN KEY             
         MVC   BUYKEST,TOSEST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RECFOUND,C'N'                                                    
         XC    CURRSTA,CURRSTA                                                  
*                                                                               
TB10     CLC   KEY(3),KEYSAVE      IF END OF CLIENT THEN DONE                   
         BNE   TBX                                                              
*                                                                               
         OC    BMKT,BMKT           IF MARKET GIVEN                              
         BZ    TB20                                                             
         CLC   BUYMSTA(2),BMKT     THEN DONE IF END OF MARKET                   
         BNE   TBX                                                              
*                                                                               
         OC    BSTA,BSTA           IF STATION GIVEN                             
         BZ    TB20                                                             
         CLC   BUYMSTA,BMKT        THEN DONE IF END OF STATION                  
         BNE   TBX                                                              
*                                                                               
TB20     CLC   BUYKEST,TOSEST      IF EST IS LESS THAN 'TO' SUBEST              
         BNL   TB30                                                             
         MVC   BUYKEST,TOSEST      THEN READ HIGH FOR ESTIMATE                  
         B     TB40                                                             
*                                                                               
TB30     BNH   TB50                ELSE IF EST IS GREATER THEN 'TO'             
         MVC   BUYKEST(4),=4X'FF'  THEN READ HIGH FOR NEXT STATION              
*                                                                               
TB40     MVI   RDUPDATE,C'N'       READ HIGH AND LOOP BACK                      
         GOTO1 HIGH                                                             
         B     TB10                                                             
*                                                                               
TB50     BAS   RE,PROCTO           PROCESS 'TO' BUY RECORD                      
*                                                                               
         MVI   RDUPDATE,C'N'       GET NEXT KEY AND LOOP BACK                   
         GOTO1 HIGH                RESTORE SEQUENCE                             
         GOTO1 SEQ                                                              
         B     TB10                                                             
*                                                                               
TBX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE 'TO' BUY RECORD TO SEE IF THERE ARE                
* ANY BUYS TO CREATE FOR THE 'TO' PRODUCT.                                      
*                                                                               
PROCTO   NTR1                                                                   
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         CLI   TRACEOPT,C'Y'       IF WE ARE TRACING                            
         BNE   PT3                                                              
         L     RE,AIO3             THEN SAVE COPY OF RECORD                     
         L     RF,=F'2000'                                                      
         L     R0,AIO                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
PT3      L     R6,AIO              R6 = A(BUY DESCRIPTION ELEMENT)              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                                                               
         MVC   LUMSTA,BUYMSTA      BUILD LOOKUP KEY FROM BUY KEY                
         MVC   LUDAY,BDDAY             AND DESCRIPTION ELEMENT                  
         MVC   LUTIME,BDTIMST                                                   
*                                                                               
         BAS   RE,LOOKUP           LOOKUP TABLE ENTRY FOR THIS KEY              
*                                                                               
         OC    WMKEY,WMKEY         IF TABLE ENTRY NOT FOUND THEN DONE           
         BZ    PTX                                                              
*                                                                               
         MVC   BYTE,BDDAY          CALC START/END DATES IN FORMAT 3             
         BAS   RE,CALCDAYS                                                      
         GOTO1 DATCON,DMCB,(2,TODATE),(0,THISDATE)                              
         L     R2,STARTDAY                                                      
         GOTO1 ADDAY,DMCB,THISDATE,NEXTDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,NEXTDATE),(3,STARTDTE)                            
         L     R2,ENDDAY                                                        
         GOTO1 ADDAY,DMCB,THISDATE,NEXTDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,NEXTDATE),(3,ENDDTE)                              
*                                                                               
         TM    BDWHY3,X'10'        IF RECORD WAS ADDED BY WEEK MOVE             
         BO    PT4                     THEN BDELEM DATES ARE NO PROBLEM         
*                                                                               
         CLC   STARTDTE,BDSTART    ELSE IF START DATE NOT WITHIN THIS           
         BL    PTX                     BUYREC THEN DONE                         
         CLC   STARTDTE,BDEND                                                   
         BH    PTX                                                              
*                                                                               
PT4      SR    RF,RF               CALCULATE NUMBER OF SPOTS TO ADD             
*                                                                               
         TM    BDCIND,X'20'        IF TRADE BUY                                 
         BO    PT5                                                              
         ICM   RF,3,WMTTSPTS       THEN USE TRADE SPOTS ACCUM                   
         MVI   TCFLAG,WMFTOTRD     SAVE FLAG FOR LATER                          
         B     PT7                                                              
*                                                                               
PT5      ICM   RF,3,WMTCSPTS       ELSE USE CASH SPOTS ACCUM                    
         MVI   TCFLAG,WMFTOCSH     SAVE FLAG FOR LATER                          
*                                                                               
PT7      ST    RF,SPOTS            SET NUMBER OF SPOTS TO ADD                   
*                                                                               
         LTR   RF,RF               IF NO SPOTS TO ADD THEN DONE                 
         BZ    PTX                                                              
         MVC   BYTE,TCFLAG         IF CREATED 'TO' BUYS THEN DONE               
         NC    BYTE,WMFLAGS                                                     
         BNZ   PTX                                                              
*                                                                               
         SR    RF,RF               SAVE COST FROM DESC ELEM                     
         ICM   RF,7,BDCOST                                                      
         ST    RF,NORMCOST                                                      
*                                                                               
         SR    R2,R2               COUNT BUY ELEMENTS ALREADY IN RECORD         
*                                                                               
         L     R6,AIO              R6 = A(FIRST BUY ELEMENT)                    
         BAS   RE,GETBYEL                                                       
         USING REGELEM,R6                                                       
         BNE   PT20                                                             
*                                                                               
PT10     TM    RSTATUS,X'C0'       IF NOT MINUS BUY                             
         BNZ   *+8                                                              
         LA    R2,1(R2)            THEN BUMP BUY ELEMENT COUNT                  
*                                                                               
         BAS   RE,NXTBYEL          GET NEXT BYEL AND LOOP BACK                  
         BE    PT10                                                             
*                                                                               
PT20     A     R2,SPOTS            IF NUMBER OF EXISTING BUYS + NUMBER          
         C     R2,=F'60'               OF BUYS TO ADD > 60 THEN SKIP            
         BH    PTX                     THIS RECORD                              
*                                                                               
         BAS   RE,BLDBYEL          BUILD BUY ELEMENT INTO ELEM                  
*                                                                               
         ICM   RF,15,WMTCOST       IF 'TO' WEEK'S COST DIFFERENT FROM           
         M     RE,=F'100'              DESC ELEM COST THEN SET OVERRIDE         
         C     RF,NORMCOST             COST IN BUY ELEM                         
         BE    PT30                                                             
         LA    R6,ELEM                                                          
         OI    RSTATUS,X'20'                                                    
         STCM  RF,7,RPCOST                                                      
*                                                                               
PT30     BAS   RE,GETINSPT         R6 = INSERTION POINT                         
         L     R3,SPOTS            R3 = NUMBER OF ELEMS TO ADD                  
*                                                                               
*                                  LOOP AND ADD ELEMS                           
PT50     GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
         BCT   R3,PT50                                                          
*                                                                               
         L     R6,AIO              R6 = A(BUY DESCRIPTION ELEMENT)              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                                                               
         TM    BDWHY3,X'10'        IF RECORD WAS ADDED BY WEEK MOVE             
         BZ    PT80                                                             
         CLC   STARTDTE,BDSTART    THEN ADJUST START OR END DATE IF             
         BNL   *+14                    NECESSARY                                
         MVC   BDSTART,STARTDTE                                                 
         B     PT80                                                             
         CLC   ENDDTE,BDEND                                                     
         BNH   PT80                                                             
         MVC   BDEND,ENDDTE                                                     
*                                                                               
PT80     MVC   AIO,AIO3            TRACE COPY/CHANGE                            
         GOTO1 TRCREC,DMCB,1                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 TRCREC,DMCB,2                                                    
*                                                                               
         CLI   NOWRTOPT,C'Y'                                                    
         BE    PT90                                                             
         XC    DMPRDLST,DMPRDLST   WRITE RECORD BACK                            
         MVC   DMPRDLST(1),TOBBRD                                               
         LA    RF,DMPRDLST                                                      
         ST    RF,DMCB+20                                                       
         GOTO1 PUTREC                                                           
         XC    DMCB+20(4),DMCB+20                                               
*                                                                               
PT90     OC    WMFLAGS,TCFLAG      FLAG THAT WE HANDLED THIS REC                
*                                                                               
         L     RF,AIO              SAVE 'TO' BUY LINE NUMBER IN WMTAB           
         MVC   BYTE,10(RF)                                                      
         CLI   TCFLAG,WMFTOTRD                                                  
         BNE   *+14                                                             
         MVC   WMTTBLIN,BYTE                                                    
         B     *+10                                                             
         MVC   WMTCBLIN,BYTE                                                    
*                                                                               
PTX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* PASS 4: RUN THROUGH WMTAB AND CREATE 'TO' BUY RECORDS FOR ANY TABLE           
*         ENTRY THAT HASN'T ALREADY BEEN HANDLED IN PASS 3.                     
*                                                                               
TOLEFT   NTR1                                                                   
         L     R7,MYSTOR           R7 = FIRST WMTAB ENTRY                       
*                                                                               
TL10     OC    WMKEY,WMKEY         WHILE NOT END OF WMTAB                       
         BZ    TLX                                                              
*                                                                               
         TM    WMFLAGS,WMFTOTRD    IF TRADE SPOTS ARE LEFT TO BE                
         BO    TL20                    ADDED THEN PROCESS THIS ENTRY            
         OC    WMTTSPTS,WMTTSPTS                                                
         BZ    TL20                                                             
         XC    SPOTS,SPOTS                                                      
         MVC   SPOTS+2(2),WMTTSPTS                                              
         MVI   TCFLAG,WMFTOTRD                                                  
         BAS   RE,ADDLEFT                                                       
*                                                                               
TL20     TM    WMFLAGS,WMFTOCSH    IF CASH SPOTS ARE LEFT TO BE                 
         BO    TL90                    ADDED THEN PROCESS THIS ENTRY            
         OC    WMTCSPTS,WMTCSPTS                                                
         BZ    TL90                                                             
         XC    SPOTS,SPOTS                                                      
         MVC   SPOTS+2(2),WMTCSPTS                                              
         MVI   TCFLAG,WMFTOCSH                                                  
         BAS   RE,ADDLEFT                                                       
*                                                                               
TL90     LA    R7,WMTABL(R7)       BUMP TO NEXT WMTAB ENTRY                     
         B     TL10                AND LOOP BACK                                
*                                                                               
TLX      B     XIT                                                              
*                                                                               
* THIS ROUTINE CREATES A BUY RECORD FROM THE CURRENT WMTAB ENTRY                
* USING TCFLAG TO DETERMINE IF IT IS A TRADE OR CASH BUY.                       
*                                                                               
ADDLEFT  NTR1                                                                   
         LA    R3,KEY              GET PROGRAM RECORD FOR THIS WMTAB            
         AH    R3,LKEY                 ENTRY INTO AIO2                          
         AH    R3,LSTATUS                                                       
         MVC   0(4,R3),WMPDSKA                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         BAS   RE,BLDBKEY          BUILD BUY KEY INTO AIO1                      
         BAS   RE,BLDDESC          ADD DESCRIPTION ELEMENT                      
         BAS   RE,GETDEMOV         GET DEMO OVERRIDES IF ANY                    
         BAS   RE,BLDDEMO          ADD DEMO ELEMENT                             
         BAS   RE,BLDBUYS          ADD BUY ELEMENTS                             
*                                                                               
         GOTO1 TRCREC,DMCB,0       ADD RECORD                                   
         CLI   NOWRTOPT,C'Y'                                                    
         BE    AL10                                                             
         XC    DMCB+20(4),DMCB+20  CLEAR ADDED PRDLIST                          
         GOTO1 ADDREC                                                           
*                                                                               
AL10     L     RF,AIO              SAVE 'TO' BUY LINE NUMBER IN WMTAB           
         MVC   BYTE,10(RF)                                                      
         CLI   TCFLAG,WMFTOTRD                                                  
         BNE   *+14                                                             
         MVC   WMTTBLIN,BYTE                                                    
         B     *+10                                                             
         MVC   WMTCBLIN,BYTE                                                    
*                                                                               
ALX      B     XIT                                                              
*                                                                               
* THIS ROUTINE INITIALIZES THE BUY RECORD WITH ITS KEY.                         
*                                                                               
BLDBKEY  NTR1                                                                   
         LA    R4,KEY              BUILD BUY KEY                                
         USING BUYKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'                                                    
         MVC   BUYMSTA,WMMSTA                                                   
         MVC   BUYKEST,TOSEST                                                   
*                                                                               
         XC    ELEM,ELEM           FILL IN TABLE OF USED BLINE NUMBERS          
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
BB10     CLC   KEY(10),KEYSAVE     WHILE STILL SAME EST                         
         BNE   BB20                                                             
*                                                                               
         ZIC   RF,BUYKBUY+1        SET THIS BLINE NUMBER USED                   
         LA    RE,ELEM                                                          
         AR    RF,RE                                                            
         MVI   0(RF),1                                                          
         MVC   BYTE,BUYKBUY+1                                                   
*                                                                               
         MVI   RDUPDATE,C'N'       READ NEXT BUY KEY AND LOOP BACK              
         GOTO1 SEQ                                                              
         B     BB10                                                             
         DROP  R4                                                               
*                                                                               
BB20     NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         LA    RF,ELEM+1           LOOP THROUGH USED BLIN TABLE LOOKING         
         LA    RE,256                  FOR A NON USED BLIN NUMBER               
         CLI   0(RF),0                                                          
         BE    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         DC    H'0'                                                             
         LA    RE,ELEM                                                          
         SR    RF,RE                                                            
         STC   RF,BYTE             SAVE BLIN NUMBER IN BYTE                     
*                                                                               
BB50     L     RE,AIO              INIT AIO TO ZEROS                            
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO              MOVE KEY TO AIO                              
         USING BUYREC,R6                                                        
         MVC   BUYKEY(10),KEYSAVE                                               
*                                                                               
         MVC   BUYKBUY,BYTE        SET BUYLINE, RECLEN, AGY CODE                
         MVI   BUYKBUY+1,X'01'                                                  
         MVI   BUYKBUY+2,0                                                      
         MVC   BUYRLEN,DATADISP                                                 
         MVC   BUYALPHA,AGENCY                                                  
*                                                                               
BBX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
* THIS ROUTINE ADDS THE DESCRIPTION ELEMENT TO THE BUY RECORD, USING            
* ANY INFORMATION IT NEEDS FROM THE CORRESPONDING PROGRAMS RECORD'S             
* DESCRIPTION ELEMENT.                                                          
*                                                                               
BLDDESC  NTR1                                                                   
         LA    R4,ELEM             BUILD BUY DESC ELEM                          
         USING BDELEM,R4                                                        
         XC    ELEM,ELEM                                                        
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,X'46'                                                      
         MVI   BDINPUT,2                                                        
         MVI   BDWKIND,C'O'                                                     
         MVC   BDSEC,MASTSPLN                                                   
         MVC   BDNTAX,SVTAX                                                     
         MVC   BDMASPRD(1),TOBBRD   ADDREC USES BDMASPRD                        
         GOTO1 DATCON,DMCB,(5,0),(3,BDCHG)                                      
         MVI   BDWHY,X'80'                                                      
         MVI   BDWHY3,X'10'                                                     
         MVI   BDSTAT,X'04'                                                     
*                                                                               
         MVC   BDDAY,WMDAY         ADD INFO FROM WMTAB ENTRY                    
         MVC   BDTIMST(4),WMTIME                                                
*                                                                               
         MVI   BDWKS,1             ADD OBVIOUS INFO                             
         MVC   BDNOWK,SPOTS+3                                                   
*                                                                               
         L     R6,AIO2             POINT TO PROGRAM DESCRIPTION ELEMENT         
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         MVC   BDDAYPT,DSCDPT      SET DAYPART/ADJ CODES IN BDELEM              
         MVC   BDPROGT,DSCADJ                                                   
*                                  SET PROGRAM NAME IN BDELEM                   
         MVC   BDPROGRM(17),DSCPROG                                             
         OC    BDPROGRM(17),=CL17' '                                            
         MVI   BDPROGRM+17,C' '                                                 
         TM    TCFLAG,WMFTOTRD                                                  
         BZ    BD10                                                             
         LA    RE,BDPROGRM+13      IF TRADE LINE THEN ADD '-T' TO NAME          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVC   1(2,RE),=C'-T'                                                   
*                                                                               
BD10     MVC   BYTE,DSCDAY         CALC START/END DAY NUMBERS                   
         BAS   RE,CALCDAYS                                                      
*                                                                               
         L     R2,STARTDAY         CALC START DAY INTO BDSTART                  
         GOTO1 DATCON,DMCB,(2,TODATE),(0,THISDATE)                              
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,BDSTART)                             
*                                                                               
         L     R2,ENDDAY           CALC END DAY INTO BDEND                      
         GOTO1 DATCON,DMCB,(2,TODATE),(0,THISDATE)                              
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,BDEND)                               
*                                                                               
         ICM   RF,15,WMTCOST       SPOT COST IN PENNIES                         
         M     RE,=F'100'                                                       
         STCM  RF,7,BDCOST                                                      
*                                                                               
         TM    TCFLAG,WMFTOTRD     COST INDICATOR = X'00' IF TRADE              
         BZ    *+12                                 X'20' IF CASH               
         MVI   BDCIND,X'00'                                                     
         B     *+8                                                              
         MVI   BDCIND,X'20'                                                     
*                                                                               
         GOTO1 ADDELEM             ADD DESC ELEMENT TO BUY REC                  
*                                                                               
BDX      B     XIT                                                              
*                                                                               
* READ DEMO OVERRIDES FOR THIS PROGRAM INTO AIO2 IF THE RECORD EXISTS           
*                                                                               
GETDEMOV NTR1                                                                   
         L     RE,AIO2                                                          
         MVI   0(RE),0                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DEMKEY,R4                                                        
         MVI   DEMKTYPE,DEMKTYPQ                                                
         MVI   DEMKSTYP,DEMKSTPQ                                                
         MVC   DEMKAM,BAGYMD                                                    
         MVC   DEMKCLT,BCLT                                                     
         MVC   DEMKMKT(5),WMMSTA                                                
         MVC   DEMKEST,BMEST                                                    
         MVC   DEMKREF,WMPREF                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
* BUILD DEMO ELEMENT FOR THIS PROGRAM/ESTIMATE                                  
*                                                                               
BLDDEMO  NTR1                                                                   
*                                                                               
         LA    R6,ELEM             PUT ELCODE AND BOOK IN DEMO ELEMENT          
         USING NDELEM,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   NDCODE,X'02'                                                     
         MVC   NDBOOK,TOBOOK                                                    
*                                                                               
BLDD30   LA    R5,BLOCK            FILL DEMO LOOKUP PARAMETER BLOCK             
         USING SPDEMLK,R5                                                       
         XC    0(SPDEMLKL,R5),0(R5)                                             
         LA    RE,DEMLKSV                                                       
         ST    RE,SPLKXTND                                                      
         MVC   SPLKAREC,AIO3       A(1000 BYTE IOAREA)                          
         L     RE,SYSPARMS         A(COMFACS)                                   
         L     RE,16(RE)                                                        
         ST    RE,SPLKAFAC                                                      
         LA    RE,QDEMOS           A(DEMO LIST)                                 
         ST    RE,SPLKALST                                                      
         L     RE,AIO3             A(DEMO VALUES)                               
         LA    RE,1000(RE)                                                      
         ST    RE,SPLKAVAL                                                      
         MVI   SPLKFIL,C'T'        FILE CODE                                    
         MVI   SPLKMED,C'T'        MEDIA CODE                                   
         CLI   SVCPROF+3,C'0'      SOURCE CODE                                  
         BNE   *+12                                                             
         MVI   SPLKSRC,C'N'                                                     
         B     *+8                                                              
         MVI   SPLKSRC,C'A'                                                     
         MVC   SPLKAGY,AGENCY      AGENCY ALPHA                                 
         MVC   SPLKCLI,QCLT        CLIENT CODE                                  
         MVC   SPLKDBK,TOBOOK      DEMO BOOK                                    
         MVC   SPLKSTA,QSTA        STATION CALL LETTERS                         
         MVC   SPLKUMK,BMKT        USER MARKET NUMBER                           
*                                                                               
         MVC   SPLKDAY,WMDAY       DAY CODE                                     
         MVC   SPLKTIM,WMTIME      MILITARY START AND END TIMES                 
*                                                                               
         MVC   SPLKSVI,TOHADJ      SVI CODE                                     
         OC    TOHADJ,TOHADJ       IF HUTADJ IS ZERO                            
         BNZ   BLDD40                                                           
         MVI   SPLKAUTF,C'O'       ADD INFO FOR AUTO SVI                        
         MVC   SPLKAUST,TOSTART                                                 
         MVC   SPLKAUND,TOEND                                                   
         DROP  R4                                                               
*                                                                               
BLDD40   GOTO1 GETDEM2,DMCB,BLOCK     GET DEMO VALUES                           
*                                                                               
         MVC   NDPROG,SPLKPRG                                                   
*                                                                               
         LA    R2,NDEMNO           PUT DEMO VALUES INTO ELEMENT                 
         L     R3,SPLKALST                                                      
         L     R4,SPLKAVAL                                                      
         DROP  R5                                                               
*                                                                               
BLDD50   CLI   0(R3),X'FF'         TEST END OF DEMO LIST                        
         BE    BLDD90                                                           
         MVC   0(3,R2),0(R3)                                                    
         MVC   3(1,R2),7(R4)                                                    
         MVC   4(4,R2),0(R4)                                                    
         LA    R2,8(R2)                                                         
         LA    R3,3(R3)                                                         
         LA    R4,8(R4)                                                         
         B     BLDD50                                                           
*                                                                               
BLDD90   LR    R4,R2               SAVE POINTER TO END OF ELEMENT               
         SR    R2,R6               CALCULATE ELEMENT LENGTH                     
         STC   R2,NDLEN            SAVE IN ELEMENT                              
*                                                                               
         L     RF,AIO2             IF DEMO OVERRIDE RECORD FOUND                
         CLI   0(RF),X'0D'                                                      
         BNE   BLDD120                                                          
*                                                                               
         LA    R2,NDEMNO           THEN USE IT TO OVERRIDE GIVEN DEMOS          
         MVC   AIO,AIO2                                                         
*                                                                               
BLDD100  CR    R2,R4               TEST END OF DEMO VALUE LIST                  
         BNL   BLDD120                                                          
         GOTO1 FINDOVR,DMCB,(R2),TOBOOK                                         
         CLC   2(2,R1),=X'FFFF'    TEST NO OVERRIDE FOR THIS DEMO               
         BE    BLDD110                                                          
         MVI   3(R2),100           SET SVI % TO 100                             
         MVI   4(R2),X'80'         SET STATUS BIT FOR OVERRIDE                  
         MVC   6(2,R2),2(R1)       MOVE IN OVERRIDING VALUE                     
*                                                                               
BLDD110  LA    R2,8(R2)            BUMP TO NEXT DEMO                            
         B     BLDD100                                                          
*                                                                               
BLDD120  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         GOTO1 ADDELEM                                                          
*                                                                               
BLDDX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
* THIS ROUTINE ADDS 'TO' BUY ELEMENTS TO THE BUY RECORD.                        
*                                                                               
BLDBUYS  NTR1                                                                   
         BAS   RE,BLDBYEL          BUILD BUY ELEM                               
*                                                                               
         BAS   RE,GETINSPT         R6 = INSERTION POINT                         
         L     R3,SPOTS            R3 = NUMBER OF ELEMS TO ADD                  
*                                                                               
*                                  LOOP AND ADD ELEMS                           
BBU10    GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
         BCT   R3,BBU10                                                         
*                                                                               
BBUX     B     XIT                                                              
         EJECT                                                                  
* GETBYEL RETURNS BUY ELEMENTS.                                                 
*                                                                               
GETBYEL  DS    0H                                                               
         AH    R6,DATADISP         R6 = A(FIRST ELEMENT IN RECORD)              
         USING REGELEM,R6                                                       
*                                                                               
CHKBYEL  CLI   RCODE,0             IF REACHED E-O-R THEN RETURN NE              
         BE    GBNE                                                             
         CLI   RCODE,X'0B'         IF ELCODE = X'0B' THEN RETURN EQ             
         BE    GBEQ                                                             
         CLI   RCODE,X'0C'         IF ELCODE = X'0C' THEN RETURN EQ             
         BE    GBEQ                                                             
*                                                                               
NXTBYEL  ZIC   R0,1(R6)            OTHERWISE BUMP TO NEXT ELEM                  
         AR    R6,R0                                                            
         B     CHKBYEL             AND LOOP BACK                                
*                                                                               
GBEQ     CR    RC,RC               RETURN EQ                                    
         BR    RE                                                               
GBNE     LTR   RC,RC               RETURN NE                                    
         BR    RE                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
* THIS ROUTINE BUILDS THE BUY ELEMENT FOR THE 'TO' BUY RECORD.                  
*                                                                               
BLDBYEL  NTR1                                                                   
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RCODE,X'0B'                                                      
         MVI   RLEN,X'0E'                                                       
         MVC   RPPRD,TOBBRD                                                     
         MVC   RPTIME,MASTSPLN                                                  
*                                                                               
         MVC   BYTE,WMDAY          CALC RDATE FROM 'TO' DATE AND                
         BAS   RE,CALCDAYS             DISP TO START OF WEEK                    
         GOTO1 DATCON,DMCB,(2,TODATE),(0,THISDATE)                              
         L     R2,STARTDAY                                                      
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,RDATE)                               
         B     XIT                                                              
*                                                                               
* THIS ROUTINE POINTS R6 TO THE INSERTION POINT IN THE 'TO' BUY RECORD          
* TO ADD THE 'TO' BUY ELEMENTS.                                                 
*                                                                               
GETINSPT NTR1                                                                   
         L     R6,AIO              POINT R6 TO INSERTION POINT                  
         BAS   RE,GETBYEL                                                       
         BNE   GIX                 IF EOR THEN RETURN                           
*                                                                               
GI10     CLC   RDATE,TODATX        IF BUY ELEM DATE > 'TO' DATE THEN            
         BH    GIX                     WE FOUND IT                              
*                                                                               
         BAS   RE,NXTBYEL          OTHERWISE BUMP TO NEXT BUY ELEM              
         BE    GI10                AND LOOP BACK IF NOT EOR                     
*                                                                               
GIX      XIT1  REGS=(R6)                                                        
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE RECEIVES A DAYS PARAMETER IN BYTE AND CALCULATES                 
* THE DISPLACEMENT FROM MONDAY FOR THE START AND END DAYS OF THE                
* WEEK.  THE TWO VALUES ARE RETURNED IN STARTDAY AND ENDDAY.                    
*                                                                               
CALCDAYS NTR1                                                                   
*                                  CONVERT DAYS TO EXPANDED FORMAT              
         GOTO1 UNDAY,DMCB,BYTE,(X'07',WORK)                                     
*                                                                               
         LA    R2,WORK             CALC START DAY OF WEEK                       
         CLI   0(R2),C'.'                                                       
         BNE   *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    RF,WORK                                                          
         SR    R2,RF                                                            
         ST    R2,STARTDAY         SAVE IN STARTDAY                             
*                                                                               
         LA    R2,WORK+6           CALC END DAY OF WEEK                         
         CLI   0(R2),C'.'                                                       
         BNE   *+10                                                             
         BCTR  R2,0                                                             
         B     *-10                                                             
         LA    RF,WORK                                                          
         SR    R2,RF                                                            
         ST    R2,ENDDAY           SAVE IN ENDDAY                               
*                                                                               
CDX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOKS FOR A WMTAB ENTRY THAT MATCHES THE LOOKUP KEY              
* (LUKEY).  IT RETURNS ITS ADDRESS IN R7.  IF THERE IS NO MATCH R7              
* WILL POINT TO THE END OF THE TABLE WHERE A NEW ENTRY CAN BE ADDED.            
* THE NEW ENTRY WILL BE INITIALIZED TO CONTAIN ALL ZEROS.                       
*                                                                               
LOOKUP   NTR1                                                                   
         L     R7,MYSTOR           R7 = A(WMTAB)                                
*                                                                               
LU10     OC    WMKEY,WMKEY         IF END OF TABLE THEN RETURN NULL             
         BZ    LUX                     ENTRY                                    
*                                                                               
         CLC   WMKEY,LUKEY         IF MATCH FOUND THEN RETURN IT                
         BE    LUX                                                              
*                                                                               
         LA    R7,WMTABL(R7)       BUMP R7 AND LOOP BACK                        
         B     LU10                                                             
*                                                                               
LUX      XIT1  REGS=(R7)                                                        
         EJECT                                                                  
* ROUTINE TO TRACE RECORDS                                                      
*                                                                               
TRCREC   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
*        CLI   CONREC,C'O'                                                      
*        BNE   TRX                                                              
         CLI   TRACEOPT,C'Y'                                                    
         BNE   TRX                                                              
         L     R2,0(R1)            0 - ADD, 1 - COPY, 2 -CHANGE                 
         L     R3,AIO                                                           
         SR    R4,R4               R3 = AIO, R4 = RECORD LENGTH                 
         ICM   R4,3,CSOLEN-CSORECD(R3)                                          
         MVI   P,C' '                                                           
         GOTO1 SPOOL,DMCB,(R5)                                                  
         MVC   P(3),=C'ADD'                                                     
         LTR   R2,R2                                                            
         BZ    TR10                                                             
         MVC   P(4),=C'COPY'                                                    
         C     R2,=F'1'                                                         
         BE    TR10                                                             
         MVC   P(6),=C'CHANGE'                                                  
         TM    CSOCNTRL-CSORECD(R3),X'80'                                       
         BZ    TR10                                                             
         MVC   P(6),=C'DEL   '                                                  
TR10     GOTO1 SPOOL,DMCB,(R5)                                                  
         L     RF,=V(PRNTBL)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,0,AIO,C'DUMP',(R4),=C'1D00',(C'P',VPRINT)              
TRX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ERROR AND OTHER EXITS.                                                        
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERRDATE  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(39,R2),=C'** ERROR DATE IS NOT VALID FOR THIS EST'             
         GOTO1 ERREX2                                                           
*                                                                               
ERRBNF   OI    MOVMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(30,R2),=C'** ERROR BUY RECORDS NOT FOUND'                      
         GOTO1 ERREX2                                                           
*                                                                               
ERRPNF   OI    MOVMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(34,R2),=C'** ERROR PROGRAM RECORDS NOT FOUND'                  
         GOTO1 ERREX2                                                           
*                                                                               
ERRBRD   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(38,R2),=C'** ERROR INVALID BRAND FOR THIS CLIENT'              
         GOTO1 ERREX2                                                           
*                                                                               
ERRSAME  OI    MOVFBRD+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(40,R2),=C'** ERROR BRAND OR WEEK MUST BE DIFFERENT'            
         GOTO1 ERREX2                                                           
*                                                                               
ERROPTS  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(L'OPTTXT,R2),OPTTXT                                            
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     OI    MOVMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(14,R2),=C'MOVE COMPLETED'                                      
         GOTO1 ERREX2                                                           
*                                                                               
MYSPACES DC    CL80' '                                                          
OPTTXT   DC    C'** ERROR INVALID OPTION ENTERED (CASH,TRADE,TEST)'             
*                                                                               
         DS    0D                                                               
AREQDSN  DS    A                   CORE RES ADDRESS OF DCB                      
REQDSN   DCB   DDNAME=REQDSN,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=2000,MACRF=PM                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* GENERATE U3 REQUESTS FOR EACH MKT/STA/SUB-ESTIMATE                            
*                                                                               
BLDREQ   NMOD1 0,**BLDR**                                                       
         L     RC,0(R1)                                                         
         CLI   NOWRTOPT,C'Y'       SKIP IF TEST RUN                             
         BE    BQX                                                              
*                                                                               
         L     R6,AIO              BUILD REQUEST RECORD EXCEPT FOR              
         XC    0(26,R6),0(R6)          MKT/STA/SUBEST                           
         MVI   14(R6),106                                                       
         LA    R6,26(R6)                                                        
         MVC   0(80,R6),MYSPACES                                                
         MVC   0(2,R6),=C'U3'                                                   
         MVC   2(2,R6),AGENCY                                                   
         MVC   4(1,R6),QMED                                                     
         MVC   5(3,R6),QCLT                                                     
         MVC   8(2,R6),=C'NN'                                                   
         MVC   11(3,R6),=C'POL'                                                 
         MVC   68(12,R6),=CL12'JOSEPHINE'                                       
*                                                                               
         L     R7,MYSTOR           R7 = BEGIN OF WMTAB                          
         XC    BMKT(5),BMKT                                                     
*                                                                               
BQ10     OC    WMKEY,WMKEY         IF END OF WMTAB THEN DONE                    
         BZ    BQX                                                              
         CLC   WMMSTA,BMKT         IF SAME MKT/STA AS PREV THEN SKIP            
         BE    BQ90                                                             
         MVC   BMKT(5),WMMSTA      SAVE MKT/STA                                 
*                                                                               
*                                  ADD MKT/STA TO REQUEST                       
         EDIT  (2,WMMSTA),(4,14(R6)),FILL=0                                     
         GOTO1 MSUNPK,DMCB,WMMSTA,FULL,18(R6)                                   
*                                  ADD 'FROM' SUBEST INFO AND WRITE REQ         
         EDIT  (1,FROMSEST),(3,23(R6)),FILL=0                                   
         GOTO1 DATCON,DMCB,(2,FROMSTR),(0,37(R6))                               
         GOTO1 DATCON,DMCB,(2,FROMEND),(0,43(R6))                               
         BAS   RE,WRITEREQ                                                      
*                                                                               
         CLC   TOSEST,FROMSEST     IF 'TO' SUBEST DIFFERENT                     
         BE    BQ90                                                             
*                                  THEN ADD 'TO' SUBEST INFO AND WRITE          
         EDIT  (1,TOSEST),(3,23(R6)),FILL=0                                     
         GOTO1 DATCON,DMCB,(2,TOSTART),(0,37(R6))                               
         GOTO1 DATCON,DMCB,(2,TOEND),(0,43(R6))                                 
         BAS   RE,WRITEREQ                                                      
*                                                                               
BQ90     LA    R7,WMTABL(R7)                                                    
         B     BQ10                                                             
*                                                                               
BQX      XIT1                                                                   
*                                                                               
* THIS ROUTINE WRITE THE CURRENT REQUEST TO EITHER THE REQUEST FILE             
* OR THE REQUEST DSN DEPENDING UPON IF WE ARE RUNNING ONLINE OR OFF.            
*                                                                               
WRITEREQ NTR1                                                                   
         CLI   CONREC,C'O'         IF OVERNITE MOVE THEN PUT REQUEST            
         BNE   WR10                    TO VIRTUAL REQFILE                       
         L     R1,AREQDSN                                                       
         PUT   (R1),(R6)                                                        
         B     WRX                                                              
*                                                                               
WR10     CLI   TWAWRITE,C'N'       ELSE WRITE REQUEST TO REAL REQFILE           
         BE    WRX                                                              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
*                                                                               
WRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
CREATREP NMOD1 0,**CREA**                                                       
         L     RC,0(R1)                                                         
         L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R7,MYSTOR                                                        
*                                                                               
CR10     OC    WMKEY,WMKEY                                                      
         BZ    CRX                                                              
*                                                                               
         EDIT  (2,WMMSTA),(4,P),ALIGN=RIGHT,FILL=0                              
         GOTO1 MSUNPK,DMCB,WMMSTA,FULL,P+5                                      
         GOTO1 UNDAY,DMCB,WMDAY,P+11                                            
         GOTO1 UNTIME,DMCB,WMTIME,P+21                                          
         OC    P+21(11),MYSPACE2                                                
*                                                                               
         EDIT  (1,WMPREF),(3,P+31),ALIGN=RIGHT,ZERO=NOBLANK                     
*                                                                               
         EDIT  (4,WMFCOST),(4,P+39),ALIGN=RIGHT,ZERO=NOBLANK                    
         EDIT  (2,WMFCSPTS),(4,P+44),ALIGN=RIGHT,ZERO=NOBLANK                   
*                                                                               
         EDIT  (1,WMFCBLIN),(3,P+52),ALIGN=RIGHT,ZERO=NOBLANK                   
         TM    WMFLAGS,WMFDLCSH                                                 
         BZ    *+8                                                              
         MVI   P+55,C'-'                                                        
*                                                                               
         EDIT  (2,WMFTSPTS),(4,P+58),ALIGN=RIGHT,ZERO=NOBLANK                   
*                                                                               
         EDIT  (1,WMFTBLIN),(3,P+66),ALIGN=RIGHT,ZERO=NOBLANK                   
         TM    WMFLAGS,WMFDLTRD                                                 
         BZ    *+8                                                              
         MVI   P+69,C'-'                                                        
*                                                                               
         EDIT  (4,WMTCOST),(4,P+74),ALIGN=RIGHT,ZERO=NOBLANK                    
         EDIT  (2,WMTCSPTS),(4,P+79),ALIGN=RIGHT,ZERO=NOBLANK                   
*                                                                               
         EDIT  (1,WMTCBLIN),(3,P+87),ALIGN=RIGHT,ZERO=NOBLANK                   
         TM    WMFLAGS,WMFTOCSH                                                 
         BO    *+18                                                             
         OC    WMTCSPTS,WMTCSPTS                                                
         BZ    *+8                                                              
         MVI   P+90,C'+'                                                        
*                                                                               
         EDIT  (2,WMTTSPTS),(4,P+93),ALIGN=RIGHT,ZERO=NOBLANK                   
*                                                                               
         EDIT  (1,WMTTBLIN),(3,P+101),ALIGN=RIGHT,ZERO=NOBLANK                  
         TM    WMFLAGS,WMFTOTRD                                                 
         BO    *+18                                                             
         OC    WMTTSPTS,WMTTSPTS                                                
         BZ    *+8                                                              
         MVI   P+104,C'+'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R4)                                                  
*                                                                               
         LA    R7,WMTABL(R7)                                                    
         B     CR10                                                             
*                                                                               
CRX      XIT1                                                                   
*                                                                               
HEDSPECS SSPEC H1,40,C'---------- FROM BUYS -----------'                        
         SSPEC H1,75,C'----------- TO BUYS ------------'                        
         SSPEC H2,32,C'PROG'                                                    
         SSPEC H2,40,C'------ CASH ------'                                      
         SSPEC H2,59,C'--- TRADE ---'                                           
         SSPEC H2,75,C'------ CASH ------'                                      
         SSPEC H2,94,C'--- TRADE ---'                                           
         SSPEC H3,1,C'MKT'                                                      
         SSPEC H3,6,C'STA'                                                      
         SSPEC H3,12,C'DAYS'                                                    
         SSPEC H3,22,C'TIMES'                                                   
         SSPEC H3,32,C'REF'                                                     
         SSPEC H3,40,C'COST'                                                    
         SSPEC H3,45,C'SPOTS'                                                   
         SSPEC H3,51,C'BUYLINE'                                                 
         SSPEC H3,59,C'SPOTS'                                                   
         SSPEC H3,65,C'BUYLINE'                                                 
         SSPEC H3,75,C'COST'                                                    
         SSPEC H3,80,C'SPOTS'                                                   
         SSPEC H3,86,C'BUYLINE'                                                 
         SSPEC H3,94,C'SPOTS'                                                   
         SSPEC H3,100,C'BUYLINE'                                                
         DC    F'0'                                                             
         DROP  R4                                                               
*                                                                               
MYSPACE2 DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOE4D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MYSTOR   DS    A                                                                
MYSTORL  EQU   8000                                                             
*                                                                               
QBRD     DS    CL3                 VALIBRD RETURN VALUES                        
BBRD     DS    X                                                                
INDATE   DS    CL2                                                              
SUBEST   DS    X                                                                
SUBBOOK  DS    CL2                                                              
SUBHADJ  DS    CL1                                                              
SUBSTART DS    XL2                                                              
SUBEND   DS    XL2                                                              
*                                                                               
FROMQBRD DS    CL3                 FROM BRAND DISPLAYABLE                       
FROMBBRD DS    X                   FROM BRAND BINARY                            
FROMDATE DS    XL2                 FROM DATE (MONDAY)                           
FROMDATX DS    XL2                 FROM DATE (SUNDAY)                           
FROMSEST DS    X                   FROM SUB ESTIMATE NUMBER                     
FROMSTR  DS    XL2                 FROM SUBEST START DATE                       
FROMEND  DS    XL2                 FROM SUBEST END DATE                         
*                                                                               
TOQBRD   DS    CL3                 TO BRAND DISPLAYABLE                         
TOBBRD   DS    X                   TO BRAND BINARY                              
TODATE   DS    XL2                 TO DATE (MONDAY)                             
TODATX   DS    XL2                 TO DATE (SUNDAY)                             
TOSEST   DS    X                   TO SUB ESTIMATE NUMBER                       
TOBOOK   DS    CL2                 TO BOOK                                      
TOHADJ   DS    CL1                 TO HADJ                                      
TOSTART  DS    XL2                 TO SUBEST START DATE                         
TOEND    DS    XL2                 TO SUBEST END DATE                           
*                                                                               
DATESAME DS    C                   FROM DATE = TO DATE (Y/N)                    
*                                                                               
TRADEOPT DS    C                   OPTION TO MOVE TRADE ONLY (Y/N)              
CASHOPT  DS    C                   OPTION TO MOVE CASH ONLY (Y/N)               
NOWRTOPT DS    C                   OPTION TO SKIP FILE WRITES (Y/N)             
TRACEOPT DS    C                   OPTION TO TRACE FILE WRITES (Y/N)            
         DS    0D                                                               
BLK32    DS    XL32                                                             
*                                                                               
SPOTS    DS    F                                                                
TSPOTS   DS    H                                                                
CSPOTS   DS    H                                                                
RATE     DS    F                   FROM WEEK RATE                               
LEFTOVER DS    F                   CARRY OVER DOLLARS                           
RECFOUND DS    C                                                                
CURRSTA  DS    XL3                                                              
*                                                                               
TRDONLY  DS    C                   CURRENT PROGREC IS TRADE ONLY                
TCFLAG   DS    X                   CASH/TRADE (SEE WMFLAGS FOR BITS)            
DELFLAG  DS    X                   'FROM' BUY RECORD DELETED (Y/N)              
DMPRDLST DS    XL2                                                              
NORMCOST DS    F                   COST FROM BUY DESC ELEMENT                   
*                                                                               
STARTDAY DS    F                   START DATE DISP FROM MONDAY                  
ENDDAY   DS    F                   END DATE DISP FROM MONDAY                    
STARTDTE DS    XL3                 BDELEM START DATE LIMIT                      
ENDDTE   DS    XL3                 BDELEM END DATE LIMIT                        
*                                                                               
LUKEY    DS    0XL10               LOOKUP KEY:                                  
LUMSTA   DS    XL5                     MKT/STA                                  
LUDAY    DS    XL1                     DAYS                                     
LUTIME   DS    XL4                     START/END TIMES                          
         EJECT                                                                  
WMTABD   DSECT                     TABLE OF WEEK MOVES                          
WMKEY    DS    0XL10                                                            
WMMSTA   DS    XL5                 KEY:       MKT/STA                           
WMDAY    DS    XL1                            DAYS                              
WMTIME   DS    XL4                            START/END TIMES                   
*                                                                               
WMFTBLIN DS    XL1                 FROM BUY LINE TRADE                          
WMFCBLIN DS    XL1                 FROM BUY LINE CASH                           
WMTTBLIN DS    XL1                 TO BUY LINE TRADE                            
WMTCBLIN DS    XL1                 TO BUY LINE CASH                             
WMPREF   DS    XL1                 REFERENCE NUMBER OF PROGREC                  
*                                                                               
WMFLAGS  DS    X                   FLAGS:                                       
WMFDLTRD EQU   X'80'                   'FROM' BUY TRADE REC DELETED             
WMFDLCSH EQU   X'40'                   'FROM' BUY CASH REC DELETED              
WMFTOTRD EQU   X'20'                   'TO' BUY TRADE REC FOUND                 
WMFTOCSH EQU   X'10'                   'TO' BUY CASH REC FOUND                  
*                                                                               
WMPDSKA  DS    XL4                 DISK ADDR OF PROGRAM RECORD                  
*                                                                               
WMFCOST  DS    XL4                 FROM WEEK: COST                              
WMFCSPTS DS    XL2                            CASH SPOTS                        
WMFTSPTS DS    XL2                            TRADE SPOTS                       
*                                                                               
WMTCOST  DS    XL4                 TO WEEK:   COST                              
WMTCSPTS DS    XL2                            CASH SPOTS                        
WMTTSPTS DS    XL2                            TRADE SPOTS                       
WMTABL   EQU   *-WMTABD                                                         
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPDEMLK                                                        
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASSB                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105SPCSO14   11/07/03'                                      
         END                                                                    
