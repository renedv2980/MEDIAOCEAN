*          DATA SET GEPFM01    AT LEVEL 002 AS OF 06/14/13                      
*PHASE TF0101A                                                                  
         TITLE 'PFM01 - VALIDATE INPUT AND DISPLAY RECORD'                      
         PRINT NOGEN                                                            
PFM01    CSECT                                                                  
         NMOD1 032,**PF01**,RA,R8                                               
         USING PFMTEMPD,R9         R9=A(GLOBAL W/S)                             
         USING FLDHDRD,R4          R4=A(SCREEN FIELD HEADER)                    
         USING PFMSAVED,R3         R3=A(TWA)                                    
         XC    FERRS,FERRS                                                      
         XC    READCT,READCT                                                    
         XC    STIFINFO,STIFINFO   CLEAR FILE INFO                              
         XC    STIRINFO,STIRINFO   CLEAR RECORD INFO                            
         XC    STIEINFO,STIEINFO   CLEAR ELEMENT INFO                           
                                                                                
***********************************************************************         
* PFM INTERFACE CHECKS                                                *         
***********************************************************************         
         TM    STATFLAG,X'80'      INTERFACE ACTIVE?                            
         BZ    PFM01B                                                           
         BAS   RE,CALLINTF         CALL IT UNTIL IT'S NOT                       
         TM    STATFLAG,X'80'      STILL ACTIVE?                                
         BZ    *+12                NO                                           
         MVI   FERN,X'FF'          LET ERROR MESSAGE COME OUT                   
         B     RIDERR                                                           
         MVC   PFMRID1H+8(L'XKEY),XKEY                                          
         LA    R1,L'XKEY                                                        
         LA    R2,XKEY+L'XKEY-1    LAST BYTE OF KEY                             
PFM01A   CLI   0(R2),0             NULL?                                        
         BNE   PFM01A1             NO, FOUND LENGTH OF KEY                      
         BCTR  R1,0                                                             
         BCTR  R2,0                GO BACK UNTIL NOT NULL                       
         B     PFM01A                                                           
PFM01A1  STC   R1,PFMRID1H+5       LENGTH OF TRANSFER KEY                       
         OI    PFMRID1H+6,X'80'    TRANSMIT THE KEY LATER                       
                                                                                
***********************************************************************         
* START OF VALIDATION ROUTINE HERE                                    *         
***********************************************************************         
PFM01B   XC    SLNRECL,SLNRECL                                                  
         XC    SLIOAREA,SLIOAREA                                                
         XC    SLELINFO,SLELINFO                                                
         XC    PFMMSG(20),PFMMSG                                                
         XC    PFMMSG1(22),PFMMSG1                                              
         OI    PFMMSGH+6,X'80'                                                  
         OI    PFMMSG1H+6,X'80'                                                 
                                                                                
***********************************************************************         
* 1.SYSTEM VALIDATION ROUTINE                                         *         
***********************************************************************         
SYSVAL   MVI   FERN,1              NO FILE ENTERED                              
         LA    R4,PFMFILEH                                                      
         CLI   FLDILEN,0           INPUT?                                       
         BE    FILERR              NO - ERROR                                   
         CLC   =C'HEX',FLDDATA     TEST FOR SCREEN CHANGE REQUEST               
         BE    SYS070              CHANGE DISPLAY TO HEX                        
         CLC   =C'DEC',FLDDATA     DITTO                                        
         BE    SYS080              CHANGE DISPLAY TO DECIMAL                    
*                                                                               
         MVC   SAVENAME,PFMFILE    SAVE NAME IN CASE CHANGE NEXT IN             
         OC    SAVENAME,BLANKS     MAKE IT UPPERCASE                            
*                                                                               
SYS005   MVI   FERN,46             ERROR - INVALID INPUT FIELD                  
         GOTO1 ASCANNER,DMCBWS,PFMFILEH,(3,IOAREA),C',=/.'                      
         CLI   4(R1),2                                                          
         BE    *+12                TWO LINES ALLOWED                            
         CLI   4(R1),1             (BUT NOT CATERED FOR...)                     
         BNE   FILERR              NOT 1 LINE, 1 OR 2 FIELDS                    
*                                                                               
         LA    R2,IOAREA           SCANNED INPUT HERE                           
         USING SCANBLKD,R2                                                      
         MVI   FERN,44                                                          
         CLI   SC1STLEN,1          SCOPE FILENAME FOR SIZE                      
         BL    FILERR              TOO SMALL                                    
         CLI   SC1STLEN,7                                                       
         BH    FILERR              TOO BIG                                      
*                                                                               
         CLI   SC2NDLEN,0          SECOND FIELD?                                
         BNE   SYS010              YES                                          
*                                                                               
         MVC   SC2NDLEN,SC1STLEN   FILE ONLY GIVEN                              
         MVC   SC2NDFLD,SC1STFLD   MAKE COPY OF DATA TO 2ND FIELD               
         MVC   STIOV,CONOV         SET CONNECTED SYSTEM VALUES                  
         MVC   STISYS,CONSYS       ..                                           
         B     SYS040                                                           
*                                                                               
SYS010   ZIC   RE,SC1STLEN         VALIDATE REQUESTED SYSTEM                    
         BCTR  RE,0                                                             
         MVI   FERN,46                                                          
         LA    R1,SLSELIST                                                      
         USING SLSELIST,R1                                                      
*                                                                               
SYS020   CLI   SLSENAME,0          END OF TABLE?                                
         BE    FILERR              YES                                          
         EX    RE,SYSCOMP          COMPARE SYSTEM NAME AGAINST INPUT            
         BE    SYS030              MATCH                                        
         LA    R1,SLSELEN(R1)      BUMP..                                       
         B     SYS020              AND TRY NEXT                                 
*                                                                               
SYSCOMP  CLC   SLSENAME(0),SC1STFLD                                             
*                                                                               
SYS030   MVC   STIOV,SLSEOV        SET INPUT SYSTEM VALUES                      
         MVC   STISYS,SLSESYS                                                   
*        DROP  R1                                                               
SYS040   L     R1,ASYSTBL          FIND TABLE ENTRY FOR OVERLAY SYSTEM          
         USING SYSTBLD,R1                                                       
         MVI   FERN,46                                                          
*                                                                               
SYS050   CLI   SYSNUM,SYS_EOT      END OF TABLE?                                
         BE    FILERR              YES                                          
         CLC   SYSNUM,STIOV        MATCH?                                       
         BE    SYS060              YES                                          
         LA    R1,SYSTABLQ(R1)     BUMP..                                       
         B     SYS050              AND TRY NEXT                                 
*                                                                               
SYS060   ST    R1,ASYSTBL          SAVE THIS DISPLACEMENT                       
         MVC   STICASE,SYSCASE     SET DISPLAY CASE (UPPER/LOWER)               
         MVC   STIFDSP,SYSDFILE    SET DISP TO FILE TABLE                       
         MVC   STIPDSP,SYSDPERM    SET DISP TO PERM TABLE                       
         CLC   STISYS,CONSYS       SWITCH NECESSARY?                            
         BE    SYSX                NO                                           
         DROP  R1                                                               
*                                                                               
         L     RF,APARM            SWITCH SYSTEMS                               
         L     RF,16(RF)                                                        
         L     RF,CSWITCH-COMFACSD(RF)                                          
         MVC   DMCBWS(1),STISYS                                                 
         MVC   DMCBWS+1(3),=X'FFFFFF'                                           
         XC    DMCBWS+4(4),DMCBWS+4                                             
         MVI   FERN,45                                                          
         GOTO1 (RF),DMCBWS                                                      
         CLI   4(R1),0             OK?                                          
         BNE   FILERR              NO                                           
         B     SYSX                                                             
*                                                                               
SYS070   MVI   DISPDAT,C'H'        HEX SCREEN REQUIRED                          
         MVC   VWDISP,HWDISP       SCREEN SIZES                                 
         MVC   VNDISP,HNDISP                                                    
         GOTO1 ACALLOV,DUB,(X'FE',PFMLAST),0                                    
         CLI   4(R1),0             OVERLAY IN OK?                               
         BE    SYS090              YES                                          
         DC    H'0'                                                             
*                                                                               
SYS080   MVI   DISPDAT,C'D'        DECIMAL SCREEN REQUIRED                      
         MVC   VWDISP,DWDISP       SCREEN SIZES                                 
         MVC   VNDISP,DNDISP                                                    
         GOTO1 ACALLOV,DUB,(X'FD',PFMLAST),0                                    
         CLI   4(R1),0             SCREEN IN OK?                                
         BE    SYS090              YES                                          
         DC    H'0'                                                             
*                                                                               
SYS090   MVC   PFMFILE,SAVENAME    LAST FILE NAME                               
         LA    R4,PFMFILEH                                                      
         LA    R1,FLDDATA                                                       
         LA    R0,L'SAVENAME                                                    
         XR    RF,RF                                                            
         CLI   0(R1),C' '          PREVIOUS INPUT?                              
         BNH   SYS120              NO                                           
*                                                                               
SYS100   CLI   0(R1),C' '          GET LENGTH OF INPUT                          
         BE    SYS110                                                           
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SYS100                                                        
         LA    RF,L'SAVENAME                                                    
*                                                                               
SYS110   STC   RF,FLDILEN          SAVE IT                                      
         STC   RF,FLDOLEN          ..                                           
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPTHIS    TRANSMIT IT                                  
         B     SYS005              GO BACK AND VALIDATE IT                      
*                                                                               
SYS120   MVI   FERN,51             SCREEN CHANGED                               
         B     FILERR              EXIT                                         
*                                                                               
SYSX     B     FILEVAL                                                          
                                                                                
***********************************************************************         
* VALIDATE FILE NAME                                                  *         
***********************************************************************         
FILEVAL  EQU   *                                                                
         MVI   FERN,46                                                          
         CLI   SC2NDLEN,1          SCOPE FOR SIZE                               
         BL    FILERR              TOO SMALL                                    
         CLI   SC2NDLEN,7                                                       
         BH    FILERR              TOO BIG                                      
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,STIFDSP        DISP TO FILETABLE                            
         A     RF,AFILETBL         GET THE CORRECT FILETABLE                    
         USING FILTABD,RF                                                       
         MVI   FERN,02                                                          
*                                                                               
FIL010   CLI   FINAME,FI_EOT       END OF TABLE?                                
         BE    FILERR              YES                                          
         EX    R1,FILCOMP          COMPARE ON NAME FOR LENGTH INPUT             
         BE    FIL030              MATCH                                        
FIL020   LA    RF,FILTABLQ(RF)     BUMP..                                       
         B     FIL010              AND TRY NEXT                                 
*                                                                               
FILCOMP  CLC   SC2NDFLD(0),FINAME  COMPARES NAME AGAINST TABLE ENTRY            
*                                                                               
FIL030   TM    FINUMBER,FISYN      TEST IF SYNONYM ENTRY                        
         BO    FIL040                                                           
         CLI   SC2NDLEN,4          MIN 4 CHARS FOR NAME MATCH                   
         BL    FIL020              TOO FEW                                      
         B     FILEOK                                                           
*                                                                               
FIL040   IC    RE,FINUMBER         EXTRACT FILE NUMBER FOR SYNONYM              
         LA    RF,255-FISYN                                                     
         NR    RE,RF               TURN OFF SYNONYM BIT                         
         BCTR  RE,0                                                             
         MH    RE,HFILTABL         INDEX TO ACTUAL FILE ENTRY                   
         XR    RF,RF                                                            
         ICM   RF,3,STIFDSP                                                     
         A     RF,AFILETBL                                                      
         LA    RF,0(RE,RF)                                                      
         B     FILEOK                                                           
*                                                                               
FILERR   LA    RF,PFMFILEH         SAVE A(FIELD) WHICH HAS ERROR                
         ST    RF,FERRS                                                         
         OI    FIND,X'01'          SET ERROR ON                                 
         B     EXIT                                                             
*                                                                               
FILEOK   MVC   STIFN(STIFX-STIFN),FINUMBER SAVE FILE DETAILS                    
         MVC   STIF1,FINUMBER                                                   
         B     ULCASE                                                           
         DROP  R2                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* SET RECORD CASE TRANSLATE                                           *         
***********************************************************************         
ULCASE   EQU   *                                                                
         MVC   ADISPTBL,ADISPUPR   SET UPPER/LOWER CASE TRANSLATE               
         CLI   STICASE,C'U'        UPPERCASE?                                   
         BE    ULC010              YES                                          
         MVC   ADISPTBL,ADISPLWR   SAVE A(TRANSLATE TABLE)                      
*                                                                               
ULC010   MVI   FERN,0                                                           
         B     RIDVAL                                                           
                                                                                
***********************************************************************         
* RECORD ID MUST BE PRESENT & CORRECT                                 *         
***********************************************************************         
RIDVAL   EQU   *                                                                
         LA    R4,PFMRID1H                                                      
         XC    WORK,WORK           CLEAR WORK AREA                              
         CLI   FLDILEN,0           RECORD ID?                                   
         BNE   RID010              YES                                          
*                                                                               
         LA    R4,PFMRID2H                                                      
         CLI   FLDILEN,0           RECORD ON 2ND LINE TO PURGE?                 
         BNE   RID020              YES                                          
*                                                                               
RID0     MVI   FERN,03             ERROR - MISSING RECORD ID.                   
         B     RIDERR                                                           
*                                                                               
RID010   OC    PFMRID1(2),BLANKS   COMMAND                                      
         ZIC   RF,FLDILEN          GET LENGTH                                   
         LA    RE,WORK                                                          
         BCTR  RF,0                -1 FOR THE MOVE                              
         EX    RF,RIMOVE                                                        
         LA    RE,1(RF,RE)         NEXT FREE IN WORK AREA                       
*                                                                               
RID020   LA    R4,PFMRID2H         SECOND LINE                                  
         ICM   RF,1,FLDILEN        LOAD LENGTH OF THE 2ND LINE                  
         BZ    RID030              NO INPUT                                     
         BCTR  RF,0                -1 FOR THE MOVE                              
         EX    RF,*+8                                                           
         B     RID030                                                           
*                                                                               
RIMOVE   MVC   0(0,RE),FLDDATA     COPY THE DATA INTO THE WORK AREA             
*                                                                               
RID030   ZIC   RE,PFMRID1H+5       GET FIRST LINE`S LENGTH                      
         ZIC   RF,PFMRID2H+5       GET SECOND LINE`S LENGTH                     
         LA    RE,0(RF,RE)         TOTAL LENGTH OF LINES                        
         STC   RE,IRIDL            FOOL PROGRAM ABOUT THE LENGTH                
         CLC   WORK(2),=C'F='                                                   
         BE    RID035                                                           
         CLC   WORK(2),=C'G='                                                   
         BE    RID035                                                           
         L     RF,AUPPER           TURNS CONTENETS OF WORK INTO                 
         TR    WORK,0(RF)          UPPERCASE UNLESS FILTERING                   
*                                                                               
RID035   CLI   PFKEY,8             PAGE DOWN?                                   
         BNE   RID040              NO                                           
*                                                                               
         CLC   =C'NE',WORK         FROM A TEXT?                                 
         BNE   RID040              NO, DON`T NEED KEY, HAVE IT ALREADY          
         ZIC   R5,SLIKL            LOAD LENGTH                                  
         GOTO1 AHEXOUT,HEXWS,SLIK,WORK+2,(R5)                                   
         SLA   R5,1                2 BYTES FOR 1 HEX BYTE                       
         LA    R5,2(R5)            2 BYTES FOR 'K,'                             
         STC   R5,IRIDL            STORE THIS LENGTH                            
         MVC   WORK(2),=C'K,'                                                   
         CLI   STIKN,9                                                          
         BNE   *+10                                                             
         MVC   WORK(2),=C'G,'                                                   
*                                                                               
RID040   CLC   =C'ON',WORK         ACTIVATE 2ND RECORD ID LINE?                 
         BNE   RID050              NO                                           
         BAS   RE,L2ON                                                          
         MVI   FERN,48             SHOW THE ACTIVATED MESSAGE                   
         B     RIDERR              AND POSITION CURSOR BACK                     
*                                                                               
RID050   CLC   =C'OFF',WORK        DEACTIVATE 2ND RECORD ID LINE?               
         BNE   RID060              NO                                           
         BAS   RE,L2OFF                                                         
         MVI   FERN,50             SHOW THE DEACTIVATED MESSAGE                 
         B     RIDERR              AND POSITION CURSOR BACK                     
*                                                                               
RID060   CLI   WORK,C'='           BUILD KEY WITH PFM INTERFACE?                
         BNE   RID090              NO                                           
         BAS   RE,CALLINTF         CALL PFM INTERFACE                           
         TM    STATFLAG,X'80'      STILL ACTIVE?                                
         BZ    *+12                NO                                           
         MVI   FERN,X'FF'          LET ERROR MESSAGE COME OUT                   
         B     RIDERR                                                           
         LA    R4,PFMRID1H                                                      
         MVC   FLDDATA(L'XKEY),XKEY                                             
         LA    R1,L'XKEY                                                        
         LA    RF,XKEY+L'XKEY-1    LAST BYTE OF KEY                             
*                                                                               
RID070   CLI   0(RF),0             NULL?                                        
         BNE   RID080              NO, FOUND LENGTH OF KEY                      
         BCTR  R1,0                                                             
         BCTR  RF,0                GO BACK UNTIL IT`S NOT NULL                  
         B     RID070                                                           
*                                                                               
RID080   STC   R1,FLDILEN          LENGTH OF TRANSFER KEY                       
         OI    FLDOIND,FOUTTRN     TRANSMIT IT LATER                            
         B     PFM01B              BACK TO THE BEGINNING                        
*                                                                               
RID090   L     R2,AKEYTBL          COMPARE WITH RECORD ID TABLE                 
         CLI   STIFT,2             INDEX SEQUENTIAL FILE?                       
         BNE   RID100              NO                                           
         CLI   PFKEY,9             PAGE DOWN ON IS FILE?                        
         BNE   RID100              NO                                           
         CLC   =C'F=',PFMEID       FILTER BEING USED?                           
         BE    RID100              YES, CAN`T DO A NEXT ON KEY                  
*                                                                               
         MVC   WORK(2),=C'NE'      CODE FOR NE..                                
         MVC   PFMRID1(2),=C'NE'                                                
         OI    PFMRID1H+6,X'80'    TRANSMIT SO USER KNOWS IT`S NEXT             
*                                                                               
RID100   MVI   FERN,04             ERROR - INVALID RECORD ID                    
         CLI   0(R2),0             END OF SEARCH KEY TABLE?                     
         BE    RIDERR              YES                                          
         CLC   0(2,R2),WORK        SEE IF RECORD ID IS IN TABLE                 
         BE    RID110              IT IS                                        
         LA    R2,4(R2)            CHECK NEXT ENTRY IN TABLE                    
         B     RID100              UNTIL TABLE IS EXHAUSTED OR A MATCH          
*                                                                               
RID110   TM    3(R2),X'01'         IS RECORD ID VALID FOR A RECORD?             
         BZ    RIDERR              ERROR - N/D FOR RECORD ID                    
         MVC   STIKN,2(R2)         STORE THE RECORD ID CODE                     
         ZIC   RF,STIKN                                                         
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     RIDK                K,..  1                                      
         B     RIDA                A,..  2                                      
         B     RIDF                FI..  3                                      
         B     RIDN                NE..  4                                      
         B     RIDERR                    5                                      
         B     RIDERR                    6                                      
         B     RIDL                LA..  7                                      
         B     RIDFI               F=..  8                                      
         B     RIDK                G,..  9                                      
         B     RIDFI               G=..  A                                      
*                                                                               
RIDERR   LA    RF,PFMRID1H         BAD FIELD                                    
         ST    RF,FERRS            SAVE IT                                      
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* KEY FORMAT F=... F=.XX,.. G=.... G=XX,..                            *         
***********************************************************************         
RIDFI    EQU   *                                                                
         MVI   FERN,46             ERROR - INVALID INPUT FIELD                  
         GOTO1 ASCANNER,DMCBWS,PFMRID1H,(3,IOAREA),0                            
         CLI   4(R1),2             DISPLACEMENT INPUT                           
         BE    RIFI010             NOTE SCANNER WILL COCK UP THE                
         CLI   4(R1),1             LAST FIELD, SO DON'T RELY ON                 
         BNE   RIDERR              ANY INFORMATION FROM IT...                   
         XR    R6,R6                                                            
         XR    R1,R1                                                            
         B     RIFI040             WE`RE USING IT FOR THE NUMBER.               
*                                                                               
RIFI010  LA    R2,IOAREA                                                        
         USING SCANBLKD,R2                                                      
         MVI   FERN,12             ERROR - INVALID START                        
         XR    R6,R6                                                            
         XR    R1,R1                                                            
         CLI   SC2NDLEN,0                                                       
         BE    RIFI040                                                          
*                                                                               
         CLI   DISPDAT,C'H'        HEX OR DECIMAL?                              
         BNE   RIFI020                                                          
         MVI   FERN,7              INVALID HEX                                  
         TM    SC2NDVAL,SCHEXQ     VALID HEX?                                   
         BZ    RIDERR                                                           
         ZIC   R7,SC2NDLEN                                                      
         GOTO1 AHEXIN,HEXWS,SC2NDFLD,DUB,(R7)                                   
         LA    RF,4                                                             
         L     R7,12(R1)           LENGTH OF INPUT                              
         SR    RF,R7               DISP INTO WORD1                              
         LA    RF,SC2NDNUM(RF)                                                  
         BNP   RIDERR                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         ZIC   R7,SC2NDLEN         EVEN OR ODD LENGTH INPUT?                    
         LR    RF,R7                                                            
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    RIFI030             EVEN                                         
         L     R6,SC2NDNUM         ODD - NEED TO MOVE ALONG 4                   
         SRL   R6,4                                                             
         ST    R6,SC2NDNUM                                                      
         B     RIFI030                                                          
*                                                                               
RIFI020  TM    SC2NDVAL,SCNUMQ                                                  
         BO    RIFI030                                                          
         LA    R6,SC2NDFLD         R6=A(FIELD)                                  
         ZIC   R7,SC2NDLEN                                                      
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,13             ERROR - START NON-NUMERIC                    
         LTR   R1,R1                                                            
         BZ    RIDERR                                                           
         ST    R1,SC2NDNUM                                                      
*                                                                               
RIFI030  L     R6,SC2NDNUM                                                      
         ZIC   R1,SC2NDLEN                                                      
*                                                                               
RIFI040  STH   R6,DISPFI                                                        
         LA    R4,WORK+2           FOR F=                                       
         LA    R4,0(R1,R4)         R1 HOLDS L' ANY DISPLACEMENT I/P             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   FERN,3              ERROR - NO KEY DATA                          
         IC    R6,IRIDL            LENGTH OF RECORD ID                          
         CH    R6,=H'3'            -2 FOR F= -1 FOR MVC                         
         BNH   RIDERR                                                           
         LA    R6,WORK(R6)         PUT A SPACE AFTER THE KEY DATA               
         MVI   0(R6),C' '                                                       
                                                                                
***********************************************************************         
* DECODE ALL DATA TYPES TO HEX, C'A'X'11'B'00011010' -> X'C1111A'     *         
***********************************************************************         
         LA    R5,L'WORK1-2        LENGTH OF FREE WORK                          
         GOTO1 ADECODE,HEXWS,((R5),(R4)),(0,WORK1),0                            
*                                                                               
         CLI   8(R1),X'FF'         OK?                                          
         BNE   *+18                                                             
         MVC   FERN(4),8(R1)       KEY INVALID                                  
         MVI   FERN,0              SET SPECIAL ERROR CODE                       
         B     RIDERR                                                           
*                                                                               
         MVC   WORK2,WORK1                                                      
         L     R5,AUPPER                                                        
         TR    WORK2,0(R5)                                                      
         MVC   STIKL,SLIKL                                                      
         MVC   STIK,SLIK                                                        
         B     RIDOK                                                            
                                                                                
***********************************************************************         
* KEY FORMAT K,...                                                    *         
***********************************************************************         
RIDK     EQU   *                                                                
         CLI   WORK+2,C'*'         FOR K,*                                      
         BNE   RIDK100                                                          
         MVI   FERN,11             ERROR - NO PREVIOUS READ                     
         CLI   DISPDAT,C'H'                                                     
         BNE   *+16                                                             
         CLI   PFMHL1H,0                                                        
         BE    RIDERR                                                           
         B     *+12                                                             
         CLI   PFMDL1H,0           # IN FIRST OUTPUT LINE                       
         BE    RIDERR                                                           
*                                                                               
         L     R1,APARM            FAKPAK PLIST                                 
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         XR    R2,R2                                                            
         USING FLDHDRD,R2                                                       
         ICM   R2,3,TIOBCURD       DISP INTO TWA OF CURSOR                      
         AR    R2,R3               R3=A(TWA)                                    
         LR    R5,R2               MOVE TO R5, A(CURSOR LINE)                   
         MVI   FERN,47                                                          
         LA    RF,PFMLAST                                                       
         CR    R5,RF                                                            
         BNH   RIDERR                                                           
         DROP  R1                                                               
*                                                                               
         SH    R5,=H'11'           BACK TO START OF PREV FIELD                  
         MVI   FERN,19             ERROR - NOT FIRST LINE                       
         CLC   =C'00000',0(R5)     START DISPLACEMENT MUST BE 0                 
         BNE   RIDERR                                                           
         CLI   5(R5),C'-'                                                       
         BE    RIDK020                                                          
         CLI   5(R5),C'*'                                                       
         BE    RIDK020                                                          
         B     RIDERR                                                           
*                                                                               
RIDK020  XR    RF,RF                                                            
         ZIC   R0,STIFKL           FILE KEY LENGTH                              
         SLL   R0,1                DOUBLE FOR DISPLAYABLE CHARACTERS            
         LR    R1,R0                                                            
         LA    R1,2(R1)            +2 FOR K,                                    
         STC   R1,IRIDL            FOOL FOR LENGTH                              
         LA    R1,WORK             PFM W/S                                      
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   0(2,R1),=C'K,'                                                   
         CLI   STIKN,9                                                          
         BNE   *+10                                                             
         MVC   0(2,R1),=C'G,'                                                   
         LA    R1,2(R1)                                                         
*                                                                               
RIDK030  LR    RF,R0               FILE KEY LENGTH                              
         ZIC   RE,FLDLEN           L'DISPLAY FIELD                              
         SH    RE,=H'8'            FOR HEADER                                   
         CR    RF,RE               MORE THAN THIS LEFT?                         
         BL    *+6                                                              
         LR    RF,RE               ONLY MOVE WHAT`S LEFT                        
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE IN THE KEY DATA                         
         B     *+10                                                             
         MVC   0(0,R1),FLDDATA                                                  
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R1,0(RF,R1)         NEXT FREE IN WORK                            
         CLI   DISPDAT,C'H'        HEX/DECIMAL DISPLAYED?                       
         BNE   *+12                                                             
         LH    RE,HLINLEN          L'HEX DISPLAY LINE                           
         B     *+8                                                              
         LH    RE,DLINLEN          L'DECIMAL DISPLAY LINE                       
         AR    R2,RE               NEXT LINE                                    
         SR    R0,RF               REDUCE AMOUNT OF KEY                         
         BNP   RIDK040             NOTHING MORE TO MOVE                         
         B     RIDK030             KEEP GOING                                   
         DROP  R2                                                               
*                                                                               
RIDK040  XR    R0,R0               KEY IS NOW IN WORK                           
         XR    R1,R1                                                            
         LA    RE,WORK             START OF KEY IN RE                           
         ZIC   RF,STIFKL                                                        
         LA    RF,1(RF)            FOR K,                                       
         SLL   RF,1                SIGNIFICANCE IN RF                           
         LA    RF,0(RE,RF)         END OF KEY HERE                              
         LA    R1,WORK+L'WORK-1    NOTE -1 FOR EX BELOW                         
         SR    R1,RF               L' TO MOVE                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ZEROS                                                    
*                                                                               
         LA    RE,4(RE)            START SEARCH FROM K,XX                       
         LR    R0,RE               SAVE THIS                                    
         XR    R1,R1                                                            
RIDK050  CLC   =X'F0F0',0(RE)      HEX NULL BYTE IN EBCDIC                      
         BE    GOTNULL                                                          
         CLC   =F'0',0(RE)         END OF KEY?                                  
         BE    RIDK070             YES                                          
         LTR   R1,R1               # SAVED NULL BYTES                           
         BNZ   NOTNULL                                                          
*                                                                               
RIDK060  LA    RE,2(RE)            NEXT BYTE                                    
         B     RIDK050                                                          
*                                                                               
GOTNULL  LTR   R1,R1               PREVIOUS ZEROS?                              
         BNZ   *+6                 NO                                           
         LR    R0,RE               A(FIRST NULL IN THIS STRING)                 
         LA    R1,2(R1)            COUNT OF NULLS                               
         B     RIDK060                                                          
*                                                                               
NOTNULL  CH    R1,=H'12'           MIN NO OF F0 BEFORE TRUNCATION               
         BH    *+10                (MUST BE EVEN NUMBER)                        
         XR    R1,R1               CLEAR NULL COUNTER                           
         B     RIDK060                                                          
*                                                                               
         ZIC   RF,STIFKL           MORE THAN 12 F0 IN A ROW HERE                
         SLL   RF,1                X2 ON FILE KEY LENGTH                        
         SR    RF,R1               FILE KL-LENGTH TO MOVE                       
         LR    R1,RF               NEVER HAVE TO MOVE MORE THAN THIS            
         LA    RF,3(RF)            FOR C'/' & C'K,'                             
         STC   RF,IRIDL            NEW RECORD INPUT LENGTH                      
         LR    R2,R0               R2=A(1ST) RE=A(LAST)                         
         MVI   0(R2),C'/'          SEPARATOR                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(RE)                                                    
         LA    RE,L'WORK           LENGTH OF WORK                               
         SR    RE,RF               NON SIGNIFICANT CHARS AT END OF WORK         
         LA    R1,WORK                                                          
         AR    RF,R1               FIRST N-S-C ADDR                             
         BCTR  RE,0                -1 MVC                                       
         EX    RE,*+8                                                           
         B     RIDK070                                                          
         MVC   0(0,RF),ZEROS       CLEAR IT DOWN                                
*                                                                               
RIDK070  XR    R1,R1                                                            
         IC    R1,IRIDL            RECORD KEY INPUT LENGTH                      
*                                                                               
RIDK080  CH    R1,=H'60'           WILL KEY FIT ON 1 LINE?                      
         BNH   RIDK090             YES                                          
         USING FLDHDRD,R2                                                       
         LA    R2,PFMRID2H                                                      
         BAS   RE,L2ON             NO, TURN ON LINE 2                           
         SH    R1,=H'60'           FIRST 60 ONTO LINE 1                         
         BCTR  R1,0                1 OFF FOR MVC                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WORK+60  EXECUTE MOVE INTO LINE 2                     
         LA    R1,1(R1)                                                         
         OI    FLDOIND,FOUTTRN     TRANSMIT LINE                                
         STC   R1,FLDOLEN          SET LENGTH                                   
         STC   R1,FLDILEN                                                       
         LA    R1,60                                                            
         B     *+12                KEEP LINE 2 ON NOW                           
*                                                                               
RIDK090  LA    R2,PFMRID2H                                                      
         BAS   RE,L2OFF            TURN OFF LINE 2                              
         MVC   PFMRID1(L'PFMRID1),BLANKS CLEAR LINE 1                           
         LA    R2,PFMRID1H                                                      
         CH    R1,=H'60'                                                        
         BNH   *+8                                                              
         LA    R1,60               ONLY IF LINE 2 HOLDS ANYTHING                
         BCTR  R1,0                ONE OFF FOR MVC                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),WORK     EXECUTE MOVE INTO LINE 1                     
         LA    R1,1(R1)                                                         
         OI    FLDOIND,FOUTTRN     TURN ON LINE                                 
         STC   R1,FLDOLEN                                                       
         STC   R1,FLDILEN                                                       
*                                                                               
RIDK100  XR    R6,R6                                                            
         MVI   FERN,3              ERROR - NO KEY DATA                          
         IC    R6,IRIDL            LENGTH OF RECORD ID                          
         CH    R6,=H'3'            -2 FOR K, -1 FOR MVC                         
         BNH   RIDERR                                                           
         LA    R6,WORK(R6)         PUT A SPACE AFTER THE KEY DATA               
         MVI   0(R6),C' '                                                       
                                                                                
***********************************************************************         
* DECODE ALL DATA TYPES TO HEX, C'A'X'11'B'00011010' -> X'C1111A'     *         
***********************************************************************         
         IC    R5,STIFKL           KEY LENGTH SPECIFIC TO FILE                  
         IC    R6,STIFFKBV         GET KEY FILL CHAR                            
         GOTO1 ADECODE,HEXWS,((R5),WORK+2),((R6),STIK),0                        
*                                                                               
         CLI   8(R1),X'FF'         OK?                                          
         BNE   *+18                                                             
         MVC   FERN(4),8(R1)       KEY INVALID                                  
         MVI   FERN,0              SET SPECIAL ERROR CODE                       
         B     RIDERR                                                           
*&&US                                                                           
         CLI   STIOV,02            SPOT?                                        
         BNE   DK02                NO THEN THIS CHECK IS WRONG                  
         CLI   STIFN,6             DEMDIR?                                      
         BNE   *+20                                                             
         CLI   12(R1),3            YES: INPUT KEY MUST BE >= 3 BYTES            
         BNL   *+12                                                             
         MVI   FERN,53             KEY TOO SHORT!                               
         B     RIDERR                                                           
*&&                                                                             
DK02     MVC   STIKL,STIFKL        KEY VALID                                    
         TM    STIFTL,X'01'                                                     
         BZ    RIDOK                                                            
*                                                                               
         MVC   DUB(2),STIK         FIX REQUEST FILE KEY                         
         XC    STIK,STIK                                                        
         MVC   STIK(2),DUB                                                      
         MVI   STIK+3,X'FF'        FORMAT IS X'XXXX00FF'                        
         MVI   STIKL,4                                                          
         B     RIDOK                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* TURN SECOND KEYLINE ON/OFF                                          *         
***********************************************************************         
         USING FLDHDRD,RF          RF=A(KEY LINE 2)                             
L2ON     LA    RF,PFMRID2H                                                      
         NI    FLDATB,255-FATBPROT TURNS ON 2ND LINE FOR MOVE                   
         NI    FLDOIND,255-FOUTPRT                                              
         OI    FLDOIND,FOUTTRN                                                  
         BR    RE                                                               
*                                                                               
L2OFF    LA    RF,PFMRID2H                                                      
         OI    FLDATB,FATBPROT     TURNS OFF 2ND LINE FOR MOVE                  
         OI    FLDOIND,FATBPROT+FOUTPRT                                         
         XC    FLDDATA(60),FLDDATA                                              
         MVI   FLDILEN,0                                                        
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* KEY FORMAT A,...                                                    *         
***********************************************************************         
RIDA     EQU   *                                                                
         CLI   WORK+2,C'*'                                                      
         BNE   RIDA050                                                          
         MVI   FERN,11             ERROR - NO PREVIOUS READ                     
*                                                                               
         CLI   DISPDAT,C'H'        HEX SCREEN?                                  
         BNE   *+16                                                             
         CLI   PFMHL1H,0                                                        
         BE    RIDERR                                                           
         B     *+12                                                             
         CLI   PFMDL1H,0           # IN 1ST O/P LINE?                           
         BE    RIDERR                                                           
*                                                                               
         L     R1,APARM            FAKPAK PLIST                                 
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         XR    R2,R2                                                            
         USING FLDHDRD,R2                                                       
         ICM   R2,3,TIOBCURD       DISP INTO TWA OF CURSOR                      
         AR    R2,R3               R3=A(TWA)                                    
         LR    R5,R2               MOVE TO R5, A(CURSOR LINE)                   
         DROP  R1                                                               
*                                                                               
         SH    R5,=H'11'           BACK TO START OF PREVIOUS FIELD              
         MVI   FERN,19             ERROR - DISPLAY NOT AT 0                     
         CLC   =C'00000',0(R5)                                                  
         BNE   RIDERR                                                           
         CLI   5(R5),C'-'                                                       
         BE    RIDA020                                                          
         CLI   5(R5),C'*'                                                       
         BE    RIDA020                                                          
         B     RIDERR                                                           
                                                                                
***********************************************************************         
* THIS CODE MOVES THE ENTIRE KEY, CONTROL AND DISK ADDR FROM SCREEN   *         
* TO WORK BEFORE EXTRACTING THE D/A AND OUTPUTTING IT TO THE SCREEN   *         
* AND TO WORK+2 FOR HEXING IN                                         *         
***********************************************************************         
RIDA020  ZIC   RF,STIFCL           CONTROL BYTES                                
         ZIC   R1,STIFKL           FILE KEY LENGTH                              
         LA    R1,4(R1,RF)         4 BYTES FOR D/A                              
         SLL   R1,1                DOUBLE FOR DISPLAYABLE CHARS                 
         LR    R0,R1                                                            
         LA    R1,WORK             PFM W/S                                      
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   0(2,R1),=C'A,'                                                   
         LA    R1,2(R1)                                                         
*                                                                               
RIDA030  LR    RF,R0               FILE KEY LENGTH                              
         ZIC   RE,FLDLEN           L'DISPLAY FIELD                              
         SH    RE,=H'8'                                                         
         CR    RF,RE               MORE THAN THIS LINE LEFT?                    
         BL    *+6                                                              
         LR    RF,RE               ONLY MOVE WHAT`S LEFT                        
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE IN THIS KEY DATA                        
         B     *+10                                                             
         MVC   0(0,R1),FLDDATA                                                  
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R1,0(RF,R1)         NEXT FREE IN WORK                            
         CLI   DISPDAT,C'H'        HEX/DECIMAL DISPLAYED?                       
         BNE   *+12                                                             
         LH    RE,HLINLEN          L'HEX DISPLAY LINE                           
         B     *+8                                                              
         LH    RE,DLINLEN          L'DECIMAL DISPLAY LINE                       
         AR    R2,RE               NEXT LINE                                    
         SR    R0,RF               REDUCE AMOUNT OF KEY                         
         BNP   RIDA040             NOTHING MORE LEFT TO MOVE                    
         B     RIDA030             KEEP GOING                                   
         DROP  R2                                                               
*                                                                               
RIDA040  XR    R0,R0               KEY IS NOW IN WORK                           
         XR    R1,R1                                                            
         LA    RE,WORK             START IN RE                                  
         ZIC   RF,STIFKL                                                        
         ZIC   R6,STIFCL           FILE CONTROL LENGTH                          
         AR    RF,R6                                                            
         LA    RF,5(RF)            FOR A, AND F(DISK ADDRESS)                   
         SLL   RF,1                SIGNIFICANCE IN RF                           
         AR    RF,RE               END OF KEY HERE                              
         LA    R1,WORK+L'WORK-1    NOTE -1 FOR EX BELOW                         
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ZEROS                                                    
*                                                                               
         ZIC   R0,STIFKL           FILE KEY LENGTH                              
         AR    R6,R0               R6=FILE CTRL LENGTH                          
         TM    STIFT,X'02'         I/S?                                         
         BO    *+16                                                             
         CLI   STIFCL,X'00'        IS CONTROL LANGTH 0?                         
         BE    *+8                                                              
         SH    R6,=H'2'                                                         
         SLL   R6,1                X2 FOR CHARACTERS                            
         LA    R5,WORK+2(R6)       R5 HOLDS A(DISK ADDR START)                  
         MVC   WORK+2(8),0(R5)                                                  
*                                                                               
         LA    R2,PFMRID2H                                                      
         USING FLDHDRD,R2                                                       
         BAS   RE,L2OFF            TURN OFF LINE 2                              
*                                                                               
         LA    R2,PFMRID1H                                                      
         MVC   FLDDATA(60),ZEROS   CLEAR LINE 1                                 
         MVC   FLDDATA(10),WORK    EXECUTE MOVE INTO LINE 1                     
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
         LA    R0,8                                                             
         STC   R0,FLDOLEN          SAVE LENGTH                                  
         STC   R0,FLDILEN          ..                                           
         MVI   IRIDL,10                                                         
         MVC   WORK+10(109),ZEROS  CLEAR REST OF WORK                           
         ZIC   R0,STIFSL           DEBUG AID     *****                          
*                                                                               
RIDA050  XR    R6,R6                                                            
         IC    R6,IRIDL            MUST BE 10 (L'DISK ADDR + A,)                
         MVI   FERN,3                                                           
         SH    R6,=H'2'                                                         
         BZ    RIDERR                                                           
         MVI   FERN,7              ERROR - INVALID HEX                          
         GOTO1 AHEXIN,HEXWS,WORK+2,STIK,(R6)                                    
         OC    12(4,R1),12(R1)     OK?                                          
         BZ    RIDERR              NO                                           
*                                                                               
         MVI   FERN,9              ERROR - DISK ADDR NOT 8 HEX CHARS            
         SRL   R6,1                                                             
         STC   R6,STIKL            SET KEY LENGTH                               
         CLI   STIKL,4             8 HEX CHARACTERS (4 BYTES)                   
         BNE   RIDERR                                                           
*                                                                               
         MVI   FERN,10             ERROR - INVALID D/A FORMAT                   
         CLI   STIFT,4                                                          
         BNE   *+14                                                             
         CLC   STIK(4),=X'00000001' TEST FIRST RECORD ON DAL FILE               
         BE    RIDOK                                                            
         OC    STIK(2),STIK        TT CANT BE ZERO                              
         BNZ   RIDOK                                                            
         TM    STIK+2,X'FC'        ALLOW FOR 18/20/22 BIT TRACK FORMAT          
         BZ    RIDERR                                                           
         B     RIDOK                                                            
                                                                                
***********************************************************************         
* KEY FORMAT FI...                                                    *         
***********************************************************************         
RIDF     EQU   *                                                                
         MVI   FERN,4              ERROR - INVALID RECORD ID                    
         CLI   STIFFKBN,X'FF'                                                   
         BE    RIDERR              CAN`T HAVE FIRST FOR FILE                    
         XR    R6,R6                                                            
         IC    R6,STIFFKBN                                                      
         XR    R5,R5                                                            
         STC   R5,STIK(R6)         SET STIK TO ZEROS                            
         LA    R6,1(R6)                                                         
         STC   R6,STIKL                                                         
         B     RIDOK                                                            
                                                                                
***********************************************************************         
* KEY FORMAT NE...                                                    *         
***********************************************************************         
RIDN     EQU   *                                                                
         MVI   FERN,11             ERROR - NO PREVIOUS RECORD FOR FILE          
         CLC   STIFN,SLRF                                                       
         BNE   RIDERR                                                           
         CLI   SLRI,0                                                           
         BNE   RIDOK                                                            
         B     RIDERR                                                           
                                                                                
***********************************************************************         
* KEY FORMAT LA...                                                    *         
***********************************************************************         
RIDL     EQU   *                   KEY FORMAT LA..                              
         MVI   FERN,11             ERROR - NO PREVIOUS RECORD FOR FILE          
         CLC   STIFN,SLRF                                                       
         BNE   RIDERR              NOT SAME FILE                                
         CLI   SLRI,0                                                           
         BE    RIDERR              NO VALID I/O                                 
         CLI   SLIRA,1                                                          
         BNE   RIDERR              PREVIOUS ACTION MUST BE DIS                  
         MVC   STIKL,SLIKL                                                      
         MVC   STIK,SLIK                                                        
         B     RIDOK                                                            
*                                                                               
RIDOK    EQU   *                                                                
         MVI   FERN,0                                                           
         B     RACTN                                                            
                                                                                
***********************************************************************         
* RECORD ACTION,START,END ARE OPTIONALLY PRESENT                      *         
***********************************************************************         
RACTN    XC    DISPSB,DISPSB       SET DEFAULT VALUES                           
         XC    0(256,RC),0(RC)                                                  
         MVI   STIRA,1             ACTION=DISPLAY                               
         CLI   STIFT,2                                                          
         BNE   *+8                                                              
         MVI   STIRA,4             ACTION=BROWSE FOR I/S                        
*                                                                               
         CLI   PFMRACTH+5,0        ANY I/P?                                     
         BNE   RACN005                                                          
         BAS   RE,PERMS                                                         
         CLI   FERN,27                                                          
         BE    EXIT                                                             
         B     RACTNOK             NO                                           
*                                                                               
RACN005  MVI   FERN,5              ERROR IN SCANNER                             
         GOTO1 ASCANNER,HEXWS,(0,PFMRACTH),(3,(RC))                             
         CLI   4(R1),0                                                          
         BE    RACTERR                                                          
*                                                                               
         MVI   FERN,25             INVALID ACTION NAME                          
         LR    R5,RC               POINT TO ACTION LINE                         
         USING SCANBLKD,R5                                                      
         CLI   SC2NDLEN,0                                                       
         BNE   RACTERR                                                          
         CLI   SC1STLEN,0          SCOPE FOR SIZE                               
         BE    RACN020             TOO SMALL                                    
         CLI   SC1STLEN,7                                                       
         BH    RACTERR             TOO BIG                                      
*                                                                               
         ZIC   RF,SC1STLEN                                                      
         BCTR  RF,0                RF=L'ACTION NAME-1                           
         L     RE,AACTNTBL         SEARCH ACTION TABLE                          
RACN010  CLI   0(RE),0             END OF TABLE                                 
         BE    RACTERR                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SC1STFLD    MATCH?                                       
         BE    *+12                                                             
         LA    RE,9(RE)            BUMP..                                       
         B     RACN010             AND TRY AGAIN                                
*                                                                               
         TM    8(RE),X'01'         ACTION DEFINED?                              
         BZ    RACTERR             NO                                           
         MVC   STIRA,7(RE)         SAVE ACTION VALUE                            
*                                                                               
         BAS   RE,PERMS            CHECK IF A VALID PERMUTATION                 
         CLI   FERN,27                                                          
         BE    EXIT                                                             
*                                                                               
RACN020  LA    R5,SCBLKLQ(R5)      POINT TO START BYTE LINE                     
         MVI   FERN,12             ERROR - INVALID START                        
         CLI   SC2NDLEN,0                                                       
         BNE   RACTERR                                                          
         CLI   SC1STLEN,0                                                       
         BE    RACN050             MISSING START BYTE                           
*                                                                               
         CLI   DISPDAT,C'H'                                                     
         BNE   RACN030                                                          
         MVI   FERN,7              ERROR - INVALID HEX                          
         TM    SC1STVAL,SCHEXQ     VALID HEX?                                   
         BZ    RACTERR             NO                                           
         ZIC   R7,SC1STLEN                                                      
         GOTO1 AHEXIN,HEXWS,SC1STFLD,DUB,(R7)                                   
         LA    RF,4                                                             
         L     R7,12(R1)           LENGTH OF INPUT                              
         SR    RF,R7               DISP INTO WORD1                              
         LA    RF,SC1STNUM(RF)                                                  
         BNP   RACTERR                                                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         ZIC   R7,SC1STLEN         EVEN OR ODD LENGTH INPUT?                    
         LR    RF,R7                                                            
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    RACN040             EVEN                                         
         L     R6,SC1STNUM         ODD - NEED TO MOVE ALONG 4                   
         SRL   R6,4                                                             
         ST    R6,SC1STNUM                                                      
         B     RACN040                                                          
*                                                                               
RACN030  TM    SC1STVAL,SCNUMQ     DECIMAL?                                     
         BO    RACN040             YES                                          
         LA    R6,SC1STFLD         R6=A(FIELD)                                  
         ZIC   R7,SC1STLEN         R7=LEN                                       
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,13             ERROR - START NOT NUMERIC                    
         LTR   R1,R1                                                            
         BZ    RACTERR                                                          
         ST    R1,SC1STNUM         SAVE IT                                      
*                                                                               
RACN040  L     R6,SC1STNUM                                                      
         MVI   FERN,12                                                          
         C     R6,MAXLEN           TOO BIG?                                     
         BH    RACTERR             YES                                          
         STH   R6,STIB             SAVE IT                                      
         STH   R6,DISPSB                                                        
*                                                                               
RACN050  LA    R5,SCBLKLQ(R5)      POINT TO END BYTE LINE                       
         MVI   FERN,14             ERROR - INVALID END                          
         CLI   SC1STLEN,0                                                       
         BE    RACTNOK             NO END BYTE INFO                             
         CLI   SC2NDLEN,0                                                       
         BE    RACN110             NO SECOND HALF                               
         CLI   0(R5),3                                                          
         BNE   RACTERR                                                          
*                                                                               
         CLC   =C'END',SC1STFLD    END=DEC                                      
         BE    RACN080                                                          
         CLC   =C'LEN',SC1STFLD    LEN=HEX                                      
         BNE   RACTERR                                                          
*                                                                               
         MVI   FERN,7              ERROR - INVALID HEX                          
         TM    SC2NDVAL,SCHEXQ                                                  
         BZ    RACTERR                                                          
         CLI   SC2NDLEN,2          LEN=XX                                       
         BNE   RACN060                                                          
         LA    R6,2                                                             
         LA    R7,11(R5)           ALIGN IN SC2NDNUM                            
         B     RACN070                                                          
*                                                                               
RACN060  MVI   FERN,14                                                          
         CLI   SC2NDLEN,4          LEN=XXXX                                     
         BNE   RACTERR                                                          
         LA    R6,4                                                             
         LA    R7,10(R5)           ALIGN IN SC2NDNUM                            
*                                                                               
RACN070  XC    SC2NDNUM,SC2NDNUM                                                
         GOTO1 AHEXIN,HEXWS,SC2NDFLD,(R7),(R6)                                  
         L     R6,SC2NDNUM                                                      
         BCTR  R6,0                                                             
         B     RACN100                                                          
*                                                                               
RACN080  TM    3(R5),X'80'                                                      
         BO    RACN090                                                          
         LA    R6,SC2NDFLD         R6=A(FIELD)                                  
         ZIC   R7,1(R5)            R7=LEN                                       
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,15             ERROR - END NOT NUMERIC                      
         LTR   R1,R1                                                            
         BZ    RACTERR                                                          
         ST    R1,SC2NDNUM                                                      
RACN090  L     R6,SC2NDNUM                                                      
*                                                                               
RACN100  MVI   FERN,14                                                          
         LTR   R6,R6                                                            
         BNP   RACTERR                                                          
         LA    R6,1(R6)                                                         
         STH   R6,SLNRECL          SET NEW RECORD LEN                           
         MVI   STIRNEW,1           SET NEW RECORD LEN FLAG                      
         CLI   STIRA,2             NEW LEN ONLY FOR CHA/ADD                     
         BE    *+12                                                             
         CLI   STIRA,3                                                          
         BNE   RACTERR                                                          
         BCTR  R6,0                NEED TO REDUCE BY 1 FOR ADD/CHANGE           
         B     RACN140                                                          
*                                                                               
RACN110  CLI   DISPDAT,C'H'        1ST HALF ONLY - NUMERIC?                     
         BNE   RACN120                                                          
         MVI   FERN,7              ERROR - INVALID HEX                          
         TM    SC1STVAL,SCHEXQ     VALID HEX?                                   
         BZ    RACTERR                                                          
         ZIC   R7,SC1STLEN                                                      
         GOTO1 AHEXIN,HEXWS,SC1STFLD,DUB,(R7)                                   
         LA    RF,4                                                             
         L     R7,12(R1)           LENGTH OF I/P                                
         SR    RF,R7               DISP INTO WORD1                              
         LA    RF,SC1STNUM(RF)                                                  
         BNP   RACTERR                                                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         ZIC   R7,SC1STLEN         EVEN OR ODD LENGTH INPUT?                    
         LR    RF,R7                                                            
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    RACN130             EVEN                                         
         L     R6,SC1STNUM         ODD - NEED TO MOVE ALONG 4                   
         SRL   R6,4                                                             
         ST    R6,SC1STNUM                                                      
         B     RACN130                                                          
*                                                                               
RACN120  TM    SC1STVAL,SCNUMQ     1ST HALF ONLY - NUMERIC?                     
         BO    RACN130                                                          
         LA    R6,SC1STFLD         R6=A(FIELD)                                  
         ZIC   R7,SC1STLEN         R7=LEN                                       
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,15             ERROR - END NOT NUMERIC                      
         LTR   R1,R1                                                            
         BZ    RACTERR                                                          
         ST    R1,SC1STNUM                                                      
RACN130  L     R6,SC1STNUM                                                      
*                                                                               
RACN140  MVI   FERN,14                                                          
         C     R6,MAXLEN           CHECK END VALUE IN R6                        
         BH    RACTERR                                                          
         MVI   FERN,16             ERROR - START > END                          
         CH    R6,STIB                                                          
         BNH   RACTERR                                                          
         STH   R6,STIL                                                          
         B     RACTNOK                                                          
*                                                                               
RACTERR  LA    R6,PFMRACTH                                                      
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
RACTNOK  MVI   FERN,18             ERROR - START > MAX REC LEN                  
         CLC   STIB,STIFRL         MORE CHECKS ON START,END                     
         BH    RACTERR                                                          
         MVI   FERN,17             ERROR - END > MAX REC LEN                    
         CLC   STIL,STIFRL                                                      
         BH    RACTERR                                                          
         OC    STIL,STIL                                                        
         BNZ   RACTNOK1                                                         
         CLI   STIFRT,1                                                         
         BNE   RACTNOK1                                                         
         MVC   STIL,STIFRL         SET STIL FOR F/L FILES                       
*                                                                               
RACTNOK1 CLI   STIRNEW,1                                                        
         BNE   RACTNOK2                                                         
         CLI   STIFRT,1                                                         
         BNE   RACTNOK2                                                         
         CLC   STIL,STIFRL                                                      
         BE    RACTNOK2                                                         
         MVI   FERN,22                                                          
         B     RACTERR                                                          
*                                                                               
RACTNOK2 EQU   *                                                                
         B     EID                                                              
                                                                                
***********************************************************************         
* ELEMENT ID IS OPTIONALLY PRESENT                                    *         
***********************************************************************         
EID      LA    R2,PFMEIDH                                                       
         USING FLDHDRD,R2                                                       
         MVI   FERN,6              ERROR - INVALID ELEMENT ID                   
         CLI   FLDILEN,0                                                        
         BE    EIDOK               ELEMENT ID NOT INPUT                         
         CLI   STIRA,1             ONLY FOR REC DIS/CHA                         
         BE    EID010                                                           
         CLI   STIRA,2                                                          
         BE    EID010                                                           
         B     EIDERR                                                           
*                                                                               
EID010   CLI   STIFRT,3            ONLY FOR V/L/ELEMENT FILES                   
         BNE   EIDERR                                                           
         OC    STIB(4),STIB        ONLY IF NO REC START,END                     
         BNZ   EIDERR                                                           
*                                                                               
         L     R4,AKEYTBL                                                       
EID020   CLI   0(R4),0             SEARCH ID TABLE                              
         BE    EIDERR                                                           
         CLC   0(2,R4),FLDDATA                                                  
         BE    EID030                                                           
         LA    R4,4(R4)                                                         
         B     EID020                                                           
*                                                                               
EID030   TM    3(R4),X'02'                                                      
         BZ    EIDERR              ERROR - N/D FOR ELEMENTS                     
         MVC   STIEN,2(R4)                                                      
         CLI   STIEN,5             S,...                                        
         BE    EID040                                                           
         CLI   STIEN,6             I,...                                        
         BE    EID070                                                           
         CLI   STIEN,8             F=...                                        
         BE    EID080                                                           
         CLI   STIEN,3             FI...                                        
         BE    EIDOK                                                            
         CLI   STIEN,7             LA...                                        
         BE    EIDOK                                                            
         DC    H'0'                                                             
*                                                                               
EID040   EQU   *                   ELEMENT ID S,...                             
         CLI   DISPDAT,C'H'        1ST HALF ONLY - NUMERIC?                     
         BNE   EID050                                                           
         MVI   FERN,7              INVALID HEX                                  
         ZIC   R7,FLDILEN                                                       
         BCTR  R7,0                -2 FOR S,                                    
         BCTR  R7,0                                                             
         GOTO1 AHEXIN,HEXWS,FLDDATA+2,DUB,(R7)                                  
         OC    12(4,R1),12(R1)                                                  
         BZ    EIDERR                                                           
         LA    RF,4                                                             
         L     R6,12(R1)           LENGTH OF INPUT                              
         SR    RF,R6               DISP INTO WORD1                              
         LA    RF,WORD1(RF)                                                     
         BNP   EIDERR                                                           
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         LR    RF,R7               L'INPUT                                      
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    EID060              EVEN                                         
         L     R6,WORD1            ODD - NEED TO MOVE IT ALONG 4                
         SRL   R6,4                                                             
         ST    R6,WORD1                                                         
         B     EID060                                                           
*                                                                               
EID050   MVC   WORD1,=4C'0'                                                     
         ZIC   R6,FLDILEN                                                       
         MVI   FERN,12             ERROR - INVALID START                        
         SH    R6,=H'3'                                                         
         BM    EIDERR                                                           
         CH    R6,=H'3'                                                         
         BH    EIDERR                                                           
         LA    R7,WORD1+3                                                       
         SR    R7,R6                                                            
         EX    R6,*+8              RIGHT JUSTIFIED IN WORD1                     
         B     *+10                                                             
         MVC   0(0,R7),FLDDATA+2                                                
         MVC   DUB(4),=4C'0'                                                    
         MVZ   DUB(4),WORD1                                                     
         MVI   FERN,13             ERROR - START NON-NUMERIC                    
         CLC   DUB(4),=4C'0'                                                    
         BNE   EIDERR                                                           
         PACK  DUB,WORD1                                                        
         CVB   R6,DUB                                                           
*                                                                               
EID060   MVI   FERN,12                                                          
         C     R6,MAXLEN                                                        
         BH    EIDERR                                                           
         STH   R6,STIE                                                          
         B     EIDOK                                                            
*                                                                               
EID070   EQU   *                   ELEMENT ID I,...                             
         ZIC   R6,FLDILEN                                                       
         MVI   FERN,7              INVALID HEX                                  
         SH    R6,=H'2'                                                         
         BZ    EIDERR                                                           
         GOTO1 AHEXIN,HEXWS,FLDDATA+2,STIE,(R6)                                 
         L     R6,12(R1)                                                        
         LTR   R6,R6                                                            
         BZ    EIDERR                                                           
         STC   R6,STIEL            SAVE ELEMENT ID LENGTH                       
         B     EIDOK                                                            
*                                                                               
EID080   EQU   *                   ELEMENT ID F=...                             
         MVI   FERN,6                                                           
         CLI   STIRA,2             CHANGE WITH FILTER?                          
         BE    EIDERR              NOT POSSIBLE YET                             
         ZIC   R6,FLDILEN                                                       
         MVI   FERN,7                                                           
         SH    R6,=H'2'                                                         
         BZ    EIDERR                                                           
         GOTO1 AHEXIN,HEXWS,PFMEID+2,STIE,(R6)                                  
         L     R6,12(R1)                                                        
         LTR   R6,R6                                                            
         BZ    EIDERR                                                           
         STC   R6,STIEL            SAVE FILTER LENGTH                           
         B     EIDOK                                                            
*                                                                               
EIDERR   LA    R6,PFMEIDH                                                       
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
EIDOK    EQU   *                                                                
         B     EACTN                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ELEMENT ACTION,START,END ARE OPTIONALLY PRESENT                     *         
***********************************************************************         
EACTN    CLI   STIEN,0             MUST HAVE ELEMENT ID INPUT                   
         BNE   EACT010                                                          
         MVI   FERN,6                                                           
         CLI   PFMEACTH+5,0                                                     
         BNE   EIDERR                                                           
         B     EACTNOK                                                          
*                                                                               
EACT010  MVI   STIEA,1             DEFAULT ELEMENT ACTION IS DIS                
         CLI   PFMEACTH+5,0                                                     
         BE    EACT030                                                          
*                                                                               
         XC    0(256,RC),0(RC)                                                  
         MVI   FERN,05             ERROR IN SCANNER                             
         GOTO1 ASCANNER,HEXWS,(0,PFMEACTH),(3,(RC))                             
         CLI   4(R1),0                                                          
         BE    EACTNERR                                                         
*                                                                               
         LR    R5,RC               POINT TO ELEMENT ACTION LINE                 
         USING SCANBLKD,R5                                                      
         MVI   FERN,25             INVALID ELEMENT ACTION                       
         CLI   SC2NDLEN,0                                                       
         BNE   EACTNERR                                                         
         CLI   SC1STLEN,0          SCOPE FOR SIZE                               
         BE    EACT050             TOO SMALL                                    
         CLI   SC1STLEN,7                                                       
         BH    EACTNERR            TOO BIG                                      
         ZIC   R6,SC1STLEN                                                      
         BCTR  R6,0                R6=L'ELEMENT ACTION-1                        
         L     R7,AACTNTBL                                                      
*                                                                               
EACT020  CLI   0(R7),0                                                          
         BE    EACTNERR                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R7),SC1STFLD                                                 
         BE    *+12                                                             
         LA    R7,9(R7)                                                         
         B     EACT020                                                          
*                                                                               
         TM    8(R7),X'02'                                                      
         BZ    EACTNERR            ACTION N/D FOR ELEMENT                       
         MVC   STIEA,7(R7)         SAVE ELEMENT ACTION VALUE                    
*                                                                               
EACT030  CLI   STIRA,1             DIS/DIS VALID PAIR                           
         BNE   EACT040                                                          
         CLI   STIEA,1                                                          
         BNE   EACTNERR                                                         
         CLI   PFMEACTH+5,0                                                     
         BE    EACTNOK                                                          
         B     EACT050                                                          
*                                                                               
EACT040  CLI   STIEA,2             CHA/CHA & CHA/ADD VALID                      
         BE    EACT050             CHA/COPY VALID TOO...                        
         CLI   STIEA,3                                                          
         BE    EACT050                                                          
         CLI   STIEA,6                                                          
         BE    EACT050                                                          
         MVI   FERN,25                                                          
         B     EACTNERR                                                         
*                                                                               
EACT050  LA    R5,SCBLKLQ(R5)      POINT TO ELEMENT START BYTE                  
         MVI   FERN,12             ERROR - INVALID START                        
         CLI   SC2NDLEN,0                                                       
         BNE   EACTNERR                                                         
         CLI   SC1STLEN,0                                                       
         BE    EACT080             MISSING START BYTE                           
*                                                                               
         CLI   DISPDAT,C'H'        1ST HALF ONLY - NUMERIC?                     
         BNE   EACT060                                                          
         MVI   FERN,7              INVALID HEX                                  
         TM    SC1STVAL,SCHEXQ     VALID HEX?                                   
         BZ    EACTNERR                                                         
         ZIC   R7,SC1STLEN                                                      
         GOTO1 AHEXIN,HEXWS,SC1STFLD,DUB,(R7)                                   
         LA    RF,4                                                             
         L     R7,12(R1)           LENGTH OF INPUT                              
         SR    RF,R7               DISP INTO WORD1                              
         LA    RF,SC1STNUM(RF)                                                  
         BNP   EACTNERR                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         ZIC   R7,SC1STLEN         EVEN OR ODD LENGTH INPUT?                    
         LR    RF,R7                                                            
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    EACT070             EVEN                                         
         L     R6,SC1STNUM         ODD - NEED TO MOVE ALONG 4                   
         SRL   R6,4                                                             
         ST    R6,SC1STNUM                                                      
         B     EACT070                                                          
*                                                                               
EACT060  TM    SC1STVAL,SCNUMQ                                                  
         BO    EACT070                                                          
         LA    R6,SC1STFLD         R6=A(FIELD)                                  
         ZIC   R7,SC1STLEN                                                      
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,13             ERROR - START NON-NUMERIC                    
         LTR   R1,R1                                                            
         BZ    EACTNERR                                                         
         ST    R1,SC1STNUM                                                      
*                                                                               
EACT070  L     R6,SC1STNUM                                                      
         MVI   FERN,12                                                          
         CLI   STIEA,6             START NONZERO FOR COPY                       
         BE    *+16                                                             
         C     R6,=F'255'                                                       
         BH    EACTNERR                                                         
         B     *+12                                                             
         C     R6,=F'16000'        FOR NOW...                                   
         BH    EACTNERR                                                         
         STH   R6,STIBE                                                         
         LTR   R6,R6                                                            
         BZ    EACT080                                                          
         CLI   STIEA,1             START NON-ZERO FOR DIS                       
         BE    EACT080                                                          
         CLI   STIEA,6                                                          
         BE    EACT080                                                          
         B     EACTNERR                                                         
*                                                                               
EACT080  LA    R5,SCBLKLQ(R5)      POINT TO ELEMENT END BYTE                    
         MVI   FERN,14             ERROR - INVALID END                          
         OC    0(2,R5),0(R5)                                                    
         BNZ   EACT090                                                          
         CLI   STIEA,3             MUST HAVE END FOR ELEMENT ADD                
         BNE   EACTNOK                                                          
*                                                                               
EACT090  CLI   SC2NDLEN,0                                                       
         BE    EACT130             NO SECOND HALF                               
         CLI   SC1STLEN,3                                                       
         BNE   EACTNERR                                                         
         CLC   =C'END',SC1STFLD    END=DEC                                      
         BE    EACT100                                                          
         CLC   =C'LEN',SC1STFLD    LEN=HEX                                      
         BNE   EACTNERR                                                         
*                                                                               
         MVI   FERN,07             INVALID HEX                                  
         TM    SC2NDVAL,SCHEXQ                                                  
         BZ    EACTNERR                                                         
         MVI   FERN,14                                                          
         CLI   SC2NDLEN,2          LEN=XX                                       
         BNE   EACTNERR                                                         
         LA    R6,2                                                             
         LA    R7,11(R5)                                                        
         XC    SC2NDNUM,SC2NDNUM                                                
         GOTO1 AHEXIN,HEXWS,SC2NDFLD,(R7),(R6)                                  
         L     R6,SC2NDNUM                                                      
         BCTR  R6,0                                                             
         B     EACT120                                                          
*                                                                               
EACT100  TM    SC2NDVAL,SCNUMQ                                                  
         BO    EACT110                                                          
         LA    R6,SC2NDFLD         R6=A(FIELD)                                  
         ZIC   R7,SC2NDLEN         R7=LEN                                       
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,15             ERROR END NON-NUMERIC                        
         LTR   R1,R1                                                            
         BZ    EACTNERR                                                         
         ST    R1,SC2NDNUM                                                      
EACT110  L     R6,SC2NDNUM                                                      
*                                                                               
EACT120  MVI   FERN,14                                                          
         LTR   R6,R6                                                            
         BNP   EACTNERR                                                         
         MVI   STIENEW,1           SET NEW ELEMENT LENGTH FLAG                  
         CLI   STIEA,2             NEW LEN ONLY FOR CHA/ADD                     
         BE    *+12                                                             
         CLI   STIEA,3                                                          
         BNE   EACTNERR                                                         
         B     EACT160                                                          
*                                                                               
EACT130  CLI   DISPDAT,C'H'        1ST HALF ONLY - NUMERIC?                     
         BNE   EACT140                                                          
         MVI   FERN,7              INVALID HEX                                  
         TM    SC1STVAL,SCHEXQ     VALID HEX?                                   
         BZ    EACTNERR                                                         
         ZIC   R7,SC1STLEN                                                      
         GOTO1 AHEXIN,HEXWS,SC1STFLD,DUB,(R7)                                   
         LA    RF,4                                                             
         L     R7,12(R1)           LENGTH OF INPUT                              
         SR    RF,R7               DISP INTO WORD1                              
         LA    RF,SC1STNUM(RF)                                                  
         BNP   EACTNERR                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DUB                                                      
         ZIC   R7,SC1STLEN         EVEN OR ODD LENGTH INPUT?                    
         LR    RF,R7                                                            
         SRL   RF,1                                                             
         SLL   RF,1                                                             
         CR    R7,RF                                                            
         BE    EACT150             EVEN                                         
         L     R6,SC1STNUM         ODD - NEED TO MOVE ALONG 4                   
         SRL   R6,4                                                             
         ST    R6,SC1STNUM                                                      
         B     EACT150                                                          
*                                                                               
EACT140  TM    SC1STVAL,SCNUMQ     1ST HALF ONLY                                
         BO    EACT150                                                          
         LA    R6,SC1STFLD         R6=A(FIELD)                                  
         ZIC   R7,SC1STLEN         R7=A(LEN)                                    
         BAS   RE,HEXDX            CHECK FOR HEX                                
         MVI   FERN,15             ERROR - END NON-NUMERIC                      
         LTR   R1,R1                                                            
         BZ    EACTNERR                                                         
         ST    R1,SC1STNUM                                                      
EACT150  L     R6,SC1STNUM                                                      
*                                                                               
EACT160  MVI   FERN,14                                                          
         C     R6,=F'255'          CHECK END VALUE IN R6                        
         BH    EACTNERR                                                         
         MVI   FERN,16             ERROR - START > END                          
         CH    R6,STIBE                                                         
         BL    EACTNERR                                                         
         STH   R6,STILE                                                         
         B     EACTNOK                                                          
*                                                                               
EACTNERR LA    R6,PFMEACTH                                                      
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
EACTNOK  EQU   *                                                                
         B     REQOK                                                            
                                                                                
***********************************************************************         
* SEARCH PERMUTATION TABLE, AND IF VALID STORE I/O INFO.              *         
***********************************************************************         
PERMS    NTR1                                                                   
         MVC   STIPFKO+0(1),STIFN                                               
         MVC   STIPFKO+1(1),STIKN                                               
         MVC   STIPFKO+2(1),STIRA                                               
         SR    R4,R4                                                            
         ICM   R4,3,STIPDSP                                                     
         A     R4,APERMTBL                                                      
*                                                                               
PERM010  CLI   0(R4),0                                                          
         BE    PERMERR                                                          
         CLC   STIPFKO(3),0(R4)                                                 
         BE    PERM020                                                          
         LA    R4,16(R4)                                                        
         B     PERM010                                                          
*                                                                               
PERM020  MVC   STIPERM,0(R4)                                                    
         CLI   STISYS2,0                                                        
         BNE   PERM025                                                          
         MVC   STIF2,STIF1                                                      
         B     XIT                                                              
*                                                                               
PERM025  XR    RF,RF                                                            
         ICM   RF,3,STIFDSP                                                     
         A     RF,AFILETBL                                                      
         USING FILTABD,RF                                                       
*                                                                               
PERM030  CLC   FINUMBER,STISYS2                                                 
         BE    PERM040                                                          
         LA    RF,FILTABLQ(RF)                                                  
         CLI   FINAME,FI_EOT                                                    
         BNE   PERM030                                                          
         DC    H'0'                SHOULDN'T BE HERE                            
*                                                                               
PERM040  MVC   STIF2,FINUMBER                                                   
         MVC   STIFN(STIFX-STIFN),STIF2                                         
         B     XIT                                                              
*                                                                               
PERMERR  LA    R6,PFMFILEH                                                      
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         MVI   FERN,27             ERROR - INVALID REQUEST                      
         B     XIT                                                              
                                                                                
***********************************************************************         
* A VALID LOOKING REQUEST HAS BEEN ENTERED. ATTEMPT TO READ RECORD TO *         
* OBTAIN RECORD LENGTH FOR FURTHER CHECKING & IF OK DISPLAY DATA      *         
***********************************************************************         
REQOK    MVI   DISKIOOP,0          SET TO FIRST I/O                             
         MVI   HDRN,3                                                           
         MVC   STIFN(STIFX-STIFN),STIF1                                         
         CLI   STIP00,0                                                         
         BE    RECMISS             NO I/O REQUIRED                              
*                                                                               
REQ010   CLI   DISKIOOP,0                                                       
         BE    REQ020                                                           
         MVC   STIFN(STIFX-STIFN),STIF2                                         
*                                                                               
REQ020   GOTO1 ADISKIO                                                          
         CLI   SLRI,0                                                           
         BE    RECMISS                                                          
         CLI   FERN,46             RECORD # INPUT FOR BLKED FILES               
         BE    RIDERR              WAS TOO BIG...                               
         CLI   DISKIOOP,1                                                       
         BE    RECR020                                                          
         MVI   DISKIOOP,1          SET TO SECOND I/O                            
         CLI   STIP01,0                                                         
         BE    RECR020             NO SECOND I/O REQUIRED                       
         B     REQ010                                                           
*                                                                               
RECR020  CLI   STIRA,4             RECORD READ WITH LEN=SLRL                    
         BE    RECHIT              NO CHECKING FOR BROWSE                       
         MVC   PFMMSG(L'PFMMSG),WRK                                             
         MVC   PFMMSG1(L'PFMMSG1),WRK+30                                        
         LH    R4,SLRL                                                          
         BCTR  R4,0                R4=MAX VALUE FOR STIB/STIL                   
         MVI   FERN,18             ERROR - START > RECORD LEN                   
         CH    R4,STIB                                                          
         BL    RACTERR                                                          
*                                                                               
         OC    SLNRECL,SLNRECL     WAS NEW RECORD LEN INPUT?                    
         BNZ   RECR030             YES BYPASS LENGTH CHECK                      
         MVI   FERN,17             ERROR - END > REC LEN                        
         CH    R4,STIL                                                          
         BL    RACTERR                                                          
*                                                                               
RECR030  LH    R5,STIL             SET DISPLAY LENGTH                           
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         LR    R5,R4               NO END INPUT                                 
         SH    R5,STIB                                                          
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
         B     RECHIT                                                           
*                                                                               
RECMISS  CLI   HDRN,3                                                           
         BNE   RECERR                                                           
         CLI   STIRA,3                                                          
         BE    ADDREC              NOTFOUND OK FOR ADD                          
         CLI   STIRA,5                                                          
         BE    CPYREC              NOTFOUND OK FOR COPY                         
*                                                                               
RECERR   SR    R6,R6                                                            
         IC    R6,HDRN                                                          
         LA    R6,28(R6)                                                        
         STC   R6,FERN             ERROR - DISK/EOF/NOTFOUND                    
RECERR2  LA    R6,PFMFILEH                                                      
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO ADD A RECORD                                             *         
***********************************************************************         
ADDREC   MVI   FERN,19                                                          
         OC    STIB,STIB                                                        
         BNZ   RACTERR             ERROR - START <> ZERO                        
*                                                                               
         MVI   FERN,20                                                          
         OC    STIL,STIL                                                        
         BZ    RACTERR             ERROR - MISSING END                          
*                                                                               
         LH    R5,VWDISP                                                        
         MH    R5,VNDISP           R5=SIZE OF DISPLAY SCREEN                    
         BCTR  R5,0                                                             
         MVI   FERN,24             ERROR - CAN`T FIT ON SCREEN                  
*                                                                               
         CH    R5,STIL             IF NUMBER OF BYTES ON SCREEN LESS            
         BNH   RACTERR             RECORD SIZE, THEN ERROR                      
*                                  NOTE:STIL IS (LEN-1) - PFM ADDS AN           
         CLI   STIFKL,0            X'00' TO THE END OF THE RECORD FOR           
         BE    ADDR010             YOU... (SEE FMTNEW)                          
         SR    R6,R6                                                            
         IC    R6,STIFKL                                                        
         SR    R7,R7                                                            
         IC    R7,STIFCL                                                        
         AR    R6,R7                                                            
         IC    R7,STIFSL                                                        
         AR    R6,R7                                                            
         BCTR  R6,R0               R6=MIN VALUE FOR STIL                        
         MVI   FERN,23                                                          
         CH    R6,STIL                                                          
         BH    RACTERR             ERROR - END TOO SMALL                        
*                                                                               
ADDR010  GOTO1 ACLEAR              CLEAR TWA DISPLAY AREA                       
         GOTO1 FMTNEW              FORMAT IOAREA FOR NEW RECORD                 
         LH    R5,STIL             SET DISPLAY LEN                              
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
         XC    DISPSLN,DISPSLN                                                  
         MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHRS                    
         GOTO1 ADISP                                                            
         MVI   HDRN,1              HDR=ENTER NEW RECORD                         
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO COPY A RECORD                                            *         
********************************************8**************************         
CPYREC   MVI   FERN,12                                                          
         OC    STIB,STIB                                                        
         BNZ   RACTERR             ERROR - INVALID START                        
*                                                                               
         MVI   FERN,14                                                          
         OC    STIL,STIL                                                        
         BNZ   RACTERR             ERROR - INVALID END                          
*                                                                               
         MVC   SLIOAREA(L'STIK),STIK                                            
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* GOT RECORD(S) TO DISPLAY                                            *         
***********************************************************************         
RECHIT   GOTO1 ACLEAR              CLEAR TWA DISPLAY AREA                       
         XR    R6,R6                                                            
         STH   R6,DISPSLN          SET START LINE NUMBER TO 1ST                 
         CLI   STIRA,4                                                          
         BE    BROWSE              GO TO BROWSE ROUTINE                         
         CLI   STIEN,0                                                          
         BNE   ELEMENT             GO TO ELEMENT ROUTINE                        
         OC    SLNRECL,SLNRECL     CHANGE IN RECORD LENGTH                      
         BZ    RECH010             NO                                           
         GOTO1 FMTNEW              INSERT NEW LENGTH IN RECORD                  
         CLC   SLNRECL,SLRL        INCREASED RECORD LENGTH                      
         BNH   RECH010             NO                                           
         MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHARS                   
         GOTO1 ADISP                                                            
         MVI   FERN,24                                                          
         CLI   DISPRES,0           DATA ALL FITS ON SCREEN?                     
         BNE   RACTERR             ERROR - CAN`T FIT ON SCREEN                  
         B     RECH020                                                          
*                                                                               
RECH010  TM    STIFTL,X'40'        BLOCKED RECORDS                              
         BZ    RECH015                                                          
         LH    R0,DISPSB                                                        
         AH    R0,DISPBLK                                                       
         STH   R0,DISPSB                                                        
*                                                                               
RECH015  MVI   DISPOP,X'0F'        PART DISP HEX,BYTES,CHARS                    
         GOTO1 ADISP                                                            
RECH020  CLI   STIRA,1                                                          
         BNE   *+12                                                             
         LA    R7,0                HDR=ENTER NEXT REQUEST                       
         B     RECH030                                                          
         CLI   STIRA,2                                                          
         BNE   *+12                                                             
         LA    R7,2                HDR=ENTER UPDATE                             
         B     RECH030                                                          
*                                                                               
         MVI   FERN,33                                                          
         CLI   STIRA,3                                                          
         BE    RECERR2             ERROR - RECORD ALREADY EXISTS                
*                                                                               
         CLI   STIRA,5                                                          
         BE    RECERR2             ERROR - RECORD ALREADY EXISTS                
         DC    H'0'                                                             
*                                                                               
RECH030  STC   R7,HDRN             SET HDR MESSAGE NUMBER.                      
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE FOR BROWSES                                                 *         
***********************************************************************         
BROWSE   MVC   DISPSB,STIB         SET START BYTE                               
         CLI   STIKN,8             FILTERING?                                   
         BE    BROW004             NO                                           
         CLI   STIKN,10            FILTERING?                                   
         BNE   BROW005             NO                                           
*                                                                               
BROW004  CLC   READCT,=H'1000'                                                  
         BL    BROW005                                                          
         MVI   FERN,52                                                          
         B     RECERR2                                                          
*                                                                               
BROW005  MVC   DISPSB,STIB         SET START BYTE                               
         CLC   STIB,SLRL                                                        
         BL    BROW010                                                          
BROW007  LH    R5,STIB             SET OUT OF RANGE                             
         LA    R5,IOAREA(R5)                                                    
         MVC   0(20,R5),=C'** REC LEN * XXXX **'                                
         MVC   13(4,R5),WRK+3                                                   
         MVC   DISPDL,DWDISP                                                    
         CLI   DISPDAT,C'H'                                                     
         BNE   *+10                                                             
         MVC   DISPDL,HWDISP                                                    
         B     BROW020                                                          
*                                                                               
BROW010  CLI   STIKN,8             FILTERING?                                   
         BE    FILTER                                                           
         CLI   STIKN,10            FILTERING?                                   
         BE    FILTER                                                           
*                                                                               
BROW015  LH    R5,STIL             R5=INPUT END BYTE                            
         LH    R6,SLRL                                                          
         BCTR  R6,0                R6=RECORD END BYTE                           
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         LR    R5,R6               SET INPUT TO RECORD END                      
         CR    R5,R6                                                            
         BNH   *+6                                                              
         LR    R5,R6               RECORD SHORTER THAN INPUT                    
         SH    R5,STIB                                                          
         LA    R5,1(R5)                                                         
         STH   R5,DISPDL                                                        
*                                                                               
BROW020  MVI   DISPOP,X'0E'        WHOLE DISP HEX,BYTES,CHARS                   
         GOTO1 ADISP                                                            
         CLI   DISPRES,1                                                        
         BNE   BROW030                                                          
         OC    DISPSLN,DISPSLN                                                  
         BZ    *+14                                                             
         MVC   SLIK,0(RC)          RESTORE LAST                                 
         B     BROW040                                                          
         LH    R5,VWDISP           FIRST WON`T FIT                              
         MH    R5,VNDISP                                                        
         STH   R5,DISPDL           MAKE IT FIT                                  
         MVC   DISPSB,STIB                                                      
         XC    DISPSLN,DISPSLN                                                  
         B     BROW020                                                          
*                                                                               
BROW030  MVC   PFMMSG(20),WRK      BUMP TO NEXT DISPLAY LINE                    
         MVC   PFMMSG1(22),WRK+30                                               
         LH    R6,DISPSLN                                                       
         AH    R6,DISPNLR                                                       
         STH   R6,DISPSLN                                                       
*                                                                               
BROW035  MVC   0(L'SLIK,RC),SLIK   SAVE LAST                                    
         MVC   STIP00(3),=X'030202' SET TO READ SEQUENTIAL                      
         CLI   STIKN,8             NEW ACTION??                                 
         BH    *+10                YES, 2 I/OS REQUIRED                         
         MVC   STIP01(3),=X'000000' CLEAR THIS OTHERWISE                        
         CLI   STISYS2,0                                                        
         BE    *+8                                                              
         MVI   STIP00+2,X'04'                                                   
         CLI   STIFT,2             I/S FILE                                     
         BE    *+8                 YES                                          
         MVI   STIP00+1,X'01'                        ??                         
         MVI   DISKIOOP,0                                                       
         MVC   STIFN(STIFX-STIFN),STIF1                                         
*                                                                               
BROW036  CLI   DISKIOOP,0                                                       
         BE    BROW037                                                          
         MVC   STIFN(STIFX-STIFN),STIF2                                         
*                                                                               
BROW037  GOTO1 ADISKIO                                                          
         CLI   SLRI,0                                                           
         BE    BROW038                                                          
         CLI   DISKIOOP,1                                                       
         BE    BROWSE                                                           
         MVI   DISKIOOP,1          SET TO SECOND I/O                            
         CLI   STIP01,0                                                         
         BE    BROWSE              NO SECOND I/O REQUIRED                       
         B     BROW036                                                          
*                                                                               
BROW038  SR    R6,R6               BROWSE TERMINATED BY I/O ERROR               
         IC    R6,HDRN                                                          
         LA    R6,34(R6)                                                        
         STC   R6,FERN             ERROR - SIZE/DISK/EOF/NOTFOUND               
         B     RECERR2                                                          
*                                                                               
BROW040  MVI   HDRN,0                                                           
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ELEMENT ROUTINE                                                     *         
***********************************************************************         
ELEMENT  L     R4,ASYSTBL          SPOT SYSTEM ELEMENT CHECK                    
         MVI   FERN,08             ERROR - ELEMENT NOT FOUND                    
         USING SYSTBLD,R4                                                       
         CLI   SYSNUM,2                                                         
         BNE   *+12                                                             
         CLI   IOAREA,0                                                         
         BE    EIDERR                                                           
         DROP  R4                                                               
*                                                                               
         ZIC   RF,STIFKL           FIND FIRST ELEMENT WITH AN INPUT ID          
         ZIC   RE,STIFCL                                                        
         LA    RF,0(RE,RF)                                                      
         IC    RE,STIFSL                                                        
         LA    RF,0(RE,RF)         RF=L'KEY+L'CONTROL+L'SYS                     
         STH   RF,SLEFRST          SAVE START OF FIRST ELEMENT                  
         LR    R1,RF               R1=ELEMENT LENGTH COUNTER                    
         MVI   WORD1,X'FF'         SET ELEMENT NOT FOUND                        
         MVI   WORD2,0             SET RECORD OK.                               
         LA    R5,IOAREA(RF)       R5=A(FIRST ELEMENT)                          
         CLI   STIEN,3                                                          
         BE    ELM050              FI.. ELEMENT FOUND                           
*                                                                               
ELM010   CLI   STIEN,7             DID WE WANT LAST?                            
         BNE   ELM020                                                           
         CLI   0(R5),0                                                          
         BE    ELM050              LA.. ELEMENT FOUND                           
*                                                                               
ELM020   CLI   STIEN,5             DID WE WANT S,..                             
         BNE   ELM030                                                           
         LR    R7,R5                                                            
         LA    R0,IOAREA                                                        
         SR    R7,R0                                                            
         CH    R7,STIE                                                          
         BNL   ELM050              S,.. ELEMENT FOUND                           
*                                                                               
ELM030   CLI   STIEN,8             DID WE WANT F=..                             
         BNE   ELM040                                                           
         CLI   0(R5),0                                                          
         BNE   *+8                                                              
         MVI   1(R5),1                                                          
         CLC   STIEL,1(R5)         IGNORE IF ELEMENT < I/P LENGTH               
         BH    ELM060                                                           
         IC    R6,STIEL                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8              COMPARE FOR INPUT LENGTH                     
         B     *+10                                                             
         CLC   0(0,R5),STIE                                                     
         BE    ELM050              F=.. ELEMENT FOUND                           
*                                                                               
ELM040   CLI   STIEN,6             DID WE WANT I,..                             
         BNE   ELM060                                                           
         CLI   0(R5),0                                                          
         BNE   *+8                                                              
         MVI   1(R5),1                                                          
         CLC   STIEL,1(R5)         IGNORE IF ELEMENT < INPUT LENGTH             
         BH    ELM060                                                           
         IC    R6,STIEL                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8              COMPARE FOR INPUT LENGTH                     
         B     *+10                                                             
         CLC   0(0,R5),STIE                                                     
         BNE   ELM060              I,.. ELEMENT FOUND                           
*                                                                               
ELM050   ST    R5,WORD1            SAVE ADDR OF FOUND ELEMENT                   
ELM060   CLI   0(R5),0                                                          
         BE    ELM070                                                           
         CLI   1(R5),2                                                          
         BNL   *+12                                                             
         MVI   WORD2,1             SET INVALID ELEMENT LENGTH                   
         B     ELM080                                                           
         IC    R6,1(R5)                                                         
         AR    R1,R6               BUMP ELEMENT LENGTH SUM                      
         SR    R7,R7                                                            
         ICM   R7,3,STIFRL                                                      
         LA    R7,1(R7)                                                         
         CR    R1,R7                                                            
         BNH   *+12                                                             
         MVI   WORD2,2             SET ELEMENT LEN > MAX                        
         B     ELM080                                                           
         AR    R5,R6               NEXT ELEMENT                                 
         CLI   WORD1,X'FF'                                                      
         BE    ELM010              ELEMENT NOT FOUND YET                        
         B     ELM060                                                           
*                                                                               
ELM070   CH    R1,SLRL             END OF ELEMENTS FOUND                        
         BE    ELM080                                                           
         LA    R1,1(R1)                                                         
         CH    R1,SLRL                                                          
         BE    ELM080                                                           
         MVI   WORD2,3             SET ELEMENT LEN <> REC LEN                   
         BCTR  R1,0                                                             
         STH   R1,WORD2+2                                                       
*                                                                               
ELM080   L     R5,WORD1            ELEMENT NOT FOUND                            
         CLI   WORD1,X'FF'                                                      
         BNE   ELM090                                                           
         MVI   FERN,08             ERROR - ELEMENT NOT FOUND                    
         CLI   WORD2,0                                                          
         BE    EIDERR                                                           
*                                                                               
         MVI   SLEACTN,1           SET ACTION TO DISPLAY                        
         LH    R5,SLEFRST                                                       
         LA    R5,IOAREA(R5)       SET TO FIRST ELEMENT                         
         B     ELM110                                                           
*                                                                               
ELM090   CLI   STIEA,1             ELEMENT FOUND                                
         BE    ELM100                                                           
         CLI   WORD2,0                                                          
         BE    ELM100                                                           
         MVI   SLEACTN,1           SET ACTION TO DISPLAY IF FUNNY               
         B     ELM110                                                           
*                                                                               
ELM100   IC    R6,1(R5)            R6=OLD ELEMENT LENGTH                        
         MVC   SLEACTN,STIEA                                                    
*                                                                               
ELM110   MVI   SLENL,0                                                          
         MVC   SLEID,0(R5)                                                      
         LA    R0,IOAREA                                                        
         SR    R5,R0                                                            
         STH   R5,SLESTRT                                                       
         CLI   SLEACTN,1                                                        
         BNE   ELM120                                                           
         CLI   PFKEY,8             PAGE DOWN?                                   
         BNE   *+10                NO                                           
         MVC   SLESTRT,DISPLACE    NEW STARTING POINT                           
         CLI   STIEN,8             FILTER?                                      
         BNE   ELDIS               NO, REGULAR DISPLAY                          
         B     ELCHA               YES, CHANGE ONLY THOSE FILTERED              
*                                                                               
ELM120   CLI   SLEACTN,2           CHECK CHANGE PARAMETERS                      
         BNE   ELM140                                                           
         LH    R7,STILE                                                         
         LTR   R7,R7                                                            
         BZ    ELCHA               NO END INPUT                                 
         LA    R7,1(R7)            R7=INPUT ELEMENT LENGTH                      
         CLI   STIENEW,1                                                        
         BE    ELM130                                                           
         CR    R7,R6                                                            
         BNH   ELCHA                                                            
         MVI   FERN,21             ERROR - END > ELEMENT LENGTH                 
         B     EACTNERR                                                         
*                                                                               
ELM130   STC   R7,SLENL            NEW END INPUT                                
         SR    R7,R6               R7=CHANGE IN ELEMENT LENGTH                  
         BNZ   *+12                                                             
         MVI   FERN,14             ERROR - NO CHANGE                            
         B     EACTNERR                                                         
         AH    R7,SLRL                                                          
         BCTR  R7,0                R7=NEW END BYTE FOR RECORD                   
         CLM   R7,3,STIFRL                                                      
         BNH   ELCHA                                                            
         MVI   FERN,17             ERROR - RECORD LEN > MAX                     
         B     EACTNERR                                                         
*                                                                               
ELM140   CLI   SLEACTN,3           CHECK ADD PARAMETERS                         
         BNE   ELM150                                                           
         LH    R7,STILE                                                         
         LA    R7,1(R7)                                                         
         STC   R7,SLENL                                                         
         AH    R7,SLRL                                                          
         BCTR  R7,0                R7=NEW END BYTE FOR RECORD                   
         CLM   R7,3,STIFRL                                                      
         BNH   ELADD                                                            
         MVI   FERN,17             ERROR - RECORD LEN > MAX                     
         B     EACTNERR                                                         
*                                                                               
ELM150   CLI   SLEACTN,6           CHECK COPY PARAMETERS                        
         BNE   ELEDIE                                                           
         LH    R7,STILE                                                         
         LTR   R7,R7                                                            
         BZ    ELCOPY                                                           
         MVI   FERN,8              ERROR - NOT FOUND                            
         B     EACTNERR                                                         
*                                                                               
ELEDIE   DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE FOR ELEMENT ADDS                                            *         
***********************************************************************         
ELADD    LH    R5,SLESTRT                                                       
         LA    R5,IOAREA(R5)                                                    
         SR    R6,R6                                                            
         IC    R6,SLENL                                                         
         SH    R6,=H'2'                                                         
         BZ    ELAD010                                                          
         MVI   0(R5),C' '          SET ELEMENT TO SPACES                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),0(R5)                                                    
ELAD010  MVI   0(R5),X'FF'         SET ELEMENT ID CODE TO FF                    
         MVC   1(1,R5),SLENL       SET ELEMENT LENGTH                           
*                                                                               
ELAD020  XC    DISPSLN,DISPSLN     WHOLE DISP HEX,BYTES,CHARS                   
         MVC   DISPSB,SLESTRT                                                   
         MVI   DISPDL,0                                                         
         MVC   DISPDL+1(1),1(R5)                                                
         MVI   DISPOP,X'0E'                                                     
         GOTO1 ADISP                                                            
         MVI   HDRN,3              HDR=ENTER ELEMENT DATA                       
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO CHANGE AN ELEMENT                                        *         
***********************************************************************         
ELCHA    LH    R5,SLESTRT          CHANGE ELEMENT                               
         LA    R5,IOAREA(R5)                                                    
         MVI   FERN,8                                                           
         CLI   0(R5),0             END OF RECORD?                               
         BE    EACTNERR                                                         
*                                                                               
         SR    R7,R7                                                            
         IC    R7,SLEID+1          R7=OLD LENGTH                                
         SR    R6,R6                                                            
         IC    R6,SLENL            R6=NEW LENGTH                                
         LTR   R6,R6                                                            
         BZ    ELCH010                                                          
         STC   R6,1(R5)            SET NEW LENGTH IN ELEMENT                    
         SR    R6,R7                                                            
         BNP   ELCH010             NO INCREASE IN LENGTH                        
         AR    R7,R5               POINT TO END OF ELEMENT                      
         MVI   0(R7),C' '          EXTEND ELEMENT WITH SPACES                   
         SH    R6,=H'2'                                                         
         BM    ELCH010                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R7),0(R7)                                                    
*                                                                               
ELCH010  CLI   STIENEW,1           NEW ELEMENT LENGTH FLAGGED?                  
         BE    ELAD020             YES                                          
         XC    DISPSLN,DISPSLN     WHOLE DISP HEX,BYTES,CHARS                   
         MVC   DISPSB,SLESTRT                                                   
         MVC   DISPLACE,DISPSB                                                  
         MVI   DISPDL,0                                                         
         MVC   DISPDL+1(1),1(R5)                                                
         SR    R0,R0               SAVE THE ELEMENT LENGTH FOR LATER            
         MVI   DISPOP,X'0E'                                                     
         B     ELDI010                                                          
*                                                                               
ELCH020  STH   R0,DISPDL           GET THE ELEMENT LENGTH                       
         MVI   HDRN,3              HDR='ENTER ELEMENT DATA'                     
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO COPY AN ELEMENT                                          *         
***********************************************************************         
ELCOPY   LH    R5,STIBE            COPY ELEMENT                                 
         ZIC   RF,STIFRLBN                                                      
         LA    RF,IOAREA(RF)                                                    
         ICM   RF,3,0(RF)          GET LENGTH OF THIS RECORD                    
         MVI   FERN,8                                                           
         CR    R5,RF               END OF RECORD?                               
         BNL   EACTNERR                                                         
         LA    R5,IOAREA(R5)                                                    
         ZIC   RF,STIFKL           FIND FIRST ELEMENT WITH AN INPUT ID          
         ZIC   RE,STIFCL                                                        
         LA    RF,0(RE,RF)                                                      
         IC    RE,STIFSL                                                        
         LA    RF,0(RE,RF)         RF=L'KEY+L'CONTROL+L'SYS                     
         LA    RF,IOAREA(RF)                                                    
         CR    R5,RF                                                            
         BNH   ELCP010                                                          
ELCP005  ZIC   RE,1(RF)                                                         
         LA    RF,0(RE,RF)                                                      
         CLI   0(RF),0               ???                                        
         BE    EACTNERR            END OF RECORD                                
         CR    R5,RF                                                            
         BH    ELCP005                                                          
*                                                                               
         LH    R5,SLESTRT                                                       
         LA    R5,IOAREA(R5)                                                    
         ZIC   R1,1(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RF)                                                    
*                                                                               
ELCP010  XC    DISPSLN,DISPSLN     WHOLE DISP HEX,BYTES,CHARS                   
         MVC   DISPSB,SLESTRT                                                   
         MVC   DISPLACE,DISPSB                                                  
         MVI   DISPDL,0                                                         
         MVC   DISPDL+1(1),1(RF)                                                
         MVC   SLENL(1),1(RF)                                                   
         MVI   SLEACTN,3           FOOL IT THAT IT`S AN ADD                     
         MVI   DISPOP,X'0E'                                                     
         GOTO1 ADISP                                                            
         MVI   HDRN,4              HDR=ELEMENT DISPLAYED..                      
         XC    FERN,FERN                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ELEMENTS                                         *         
***********************************************************************         
ELDIS    MVI   DISPOP,X'0E'                                                     
         LA    R5,IOAREA                                                        
         AH    R5,SLESTRT                                                       
         XC    DISPSB,DISPSB       FIRST DISP KEY/CONT/SAVE                     
         MVC   DISPDL,SLEFRST                                                   
         OC    DISPDL,DISPDL                                                    
         BZ    ELDI030                                                          
         GOTO1 ADISP                                                            
         MVC   DISPSLN,DISPNLR                                                  
         MVI   DISPOP,X'0E'                                                     
         B     ELDI030                                                          
*                                                                               
ELDI010  GOTO1 ADISP               DISPLAY ELEMENT                              
         TM    DISPOP,X'01'                                                     
         BO    ELDI070             END DISP ON PART OF BIG ELEMENT              
         CLI   DISPRES,1                                                        
         BNE   *+20                                                             
         TM    DISPOP,X'80'                                                     
         BO    ELDI070             END DISP IF DON`T FIT                        
         OI    DISPOP,X'01'                                                     
         B     ELDI010             MAKE FIRST FIT                               
         LH    R6,DISPSLN                                                       
         AH    R6,DISPNLR                                                       
         STH   R6,DISPSLN          BUMP DISPLAY START LINE                      
         ZIC   R6,1(R5)            LOAD LENGTH OF ELEMENT                       
         AR    R0,R6               TOTAL LENGTH                                 
*                                                                               
ELDI020  CLI   0(R5),0                                                          
         BE    ELDI070             END DISPLAY IF LAST ELEMENT                  
         SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         CLI   1(R5),2                                                          
         BL    ELDI070             END DISPLAY IF FUNNY                         
         AR    R5,R6               BUMP TO NEXT ELEMENT                         
*                                                                               
ELDI030  LR    R7,R5                                                            
         LA    R6,1                                                             
         CLI   0(R5),0                                                          
         BE    ELDI050             DISPLAY 1 BYTE FOR LAST ELEMENT              
         LA    R6,2                DISPLAY 2 BYTES IF FUNNY                     
         CLI   1(R5),2                                                          
         BE    ELDI050                                                          
         CLI   STIEN,8             FILTER?                                      
         BNE   ELDI040             NO                                           
         CLC   STIEL,1(R5)         LESS THAN FILTER LENGTH?                     
         BH    ELDI020             DOESN`T MATCH                                
         IC    R6,STIEL            LOAD UP FILTER LENGTH                        
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),STIE        COMPARE WITH FILTER                          
         BNE   ELDI020             DOESN`T MATCH                                
*                                                                               
ELDI040  ZIC   R6,1(R5)            LOAD LENGTH OF ELEMENT                       
         AH    R7,STIBE            ADJUST BY START VALUE                        
         SH    R6,STIBE                                                         
         BNP   ELDI020             IGNORE ELEMENT IF > LENGTH                   
         LH    RE,STILE                                                         
         LTR   RE,RE                                                            
         BZ    ELDI050             USE ELEMENT LENGTH IF NO END VALUE           
         SH    RE,STIBE                                                         
         LA    RE,1(RE)                                                         
         CR    R6,RE               USE SMALLER OF ELEMENT & END                 
         BNH   ELDI050                                                          
         LR    R6,RE                                                            
*                                                                               
ELDI050  LA    RE,IOAREA           SET START & LENGTH OF DATA                   
         SR    R7,RE                                                            
         STH   R7,DISPSB                                                        
         CLI   0(R5),0             LAST ELEMENT?                                
         BE    ELDI060             YES                                          
         CLI   1(R5),2             FUNNY?                                       
         BE    ELDI060             YES                                          
         STH   R7,DISPLACE                                                      
*                                                                               
ELDI060  STH   R6,DISPDL                                                        
         B     ELDI010                                                          
*                                                                               
ELDI070  CLI   WORD2,0                                                          
         BNE   ELDI080                                                          
         CLI   STIEA,2             CHANGE OPTION?                               
         BE    ELCH020                                                          
         MVI   HDRN,0              END OF NORMAL DISPLAY                        
         XC    FERN,FERN                                                        
         B     EXIT                                                             
*                                                                               
ELDI080  LR    R6,RC               POINT TO SPECIAL MESSAGE AREA                
         XC    0(30,R6),0(R6)                                                   
         ST    R6,FERN                                                          
         MVI   FERN,0              SET SPECIAL ERROR NUMBER                     
         LA    R5,PFMFILEH                                                      
         MVC   0(14,R6),=C'**WARNING** - '                                      
         CLC   STIEA,SLEACTN                                                    
         BE    *+14                                                             
         LA    R5,PFMEACTH                                                      
         MVC   0(14,R6),=C'INVALID REC - '                                      
         ST    R5,FERRS                                                         
         OI    FIND,X'01'                                                       
*                                                                               
ELDI090  CLI   WORD2,1                                                          
         BNE   ELDI100                                                          
         MVC   14(15,R6),=C'ELEMENT INVALID'                                    
         B     EXIT                                                             
*                                                                               
ELDI100  CLI   WORD2,2                                                          
         BNE   ELDI110                                                          
         MVC   14(14,R6),=C'SUM ELMS > MAX'                                     
         B     EXIT                                                             
*                                                                               
ELDI110  CLI   WORD2,3                                                          
         BNE   ELDIDIE                                                          
         MVC   14(12,R6),=C'RL <> EL= NNNN'                                     
         LH    R1,WORD2+2                                                       
         CVD   R1,DUB                                                           
         UNPK  24(4,R6),DUB                                                     
         OI    27(R6),X'F0'                                                     
         B     EXIT                                                             
*                                                                               
ELDIDIE  DC    H'0'                                                             
                                                                                
***********************************************************************         
* FILTERS A RECORD FOR THE STRING IN WORK1                            *         
***********************************************************************         
FILTER   EQU   *                                                                
         LH    RE,READCT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,READCT                                                        
         LA    RE,IOAREA                                                        
         LA    RF,WORK1                                                         
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LA    R0,WORK1                                                         
         SR    RF,R0               RF=L'TO COMPARE ON                           
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
         ICM   R0,3,SLRL           RECORD LENGTH                                
         LH    R1,DISPFI                                                        
         LTR   R1,R1               CHECK ALL RECORD?                            
         BZ    FILT010             YES                                          
*                                                                               
         CR    R0,R1                                                            
         BNH   BROW007             ??                                           
         LA    RE,0(R1,RE)                                                      
         EX    RF,FILTC1                                                        
         BE    FILT020                                                          
         EX    RF,FILTC2                                                        
         BE    FILT020                                                          
         B     BROW035                                                          
*                                                                               
FILT010  EX    RF,FILTC1           LOOP TO CHECK WHOLE RECORD                   
         BE    FILT020             AGAINST DATA IN WORK1/WORK2                  
         EX    RF,FILTC2                                                        
         BE    FILT020                                                          
         LA    RE,1(RE)                                                         
         BCT   R0,FILT010                                                       
         B     BROW035                                                          
*                                                                               
FILT020  B     BROW015                                                          
*                                                                               
FILTC1   CLC   0(0,RE),WORK1                                                    
FILTC2   CLC   0(0,RE),WORK2                                                    
                                                                                
***********************************************************************         
* CONVERT HEX STRING X'....' TO VALUE IN R1                           *         
* R6=A(STRING)  R7=LENGTH                                             *         
***********************************************************************         
HEXDX    ST    RE,WORD2            SAVE RE                                      
         CLC   0(2,R6),=C'X'''                                                  
         BNE   HEXERR              1ST CHARS NOT (X')                           
         SH    R7,=H'3'                                                         
         BNP   HEXERR              L' CAN`T BE < 4                              
         LA    R1,2(R6,R7)                                                      
         CLI   0(R1),C''''                                                      
         BNE   HEXERR              LAST CHAR NOT (')                            
         GOTO1 AHEXIN,HEXWS,2(R6),WRK,(R7)                                      
         ICM   R1,15,12(R1)                                                     
         BZ    HEXERR              INVALID HEX                                  
         SLL   R7,2                                                             
         LA    R6,32               SHIFT RESULT                                 
         SR    R6,R7               32-(L'TXT * 4)                               
         L     R1,WRK                                                           
         SRL   R1,0(R6)            TO GET NUMBER IN R1                          
         L     RE,WORD2                                                         
         BR    RE                                                               
HEXERR   LA    R1,0                IF R1=0 ERROR                                
         L     RE,WORD2                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* THIS ROUTINE INITIALISES IOAREA TO SUITABLE FORMAT FOR A NEW RECORD *         
* TO BE ADDED TO A FILE OR FOR AN EXTENSION OF AN EXISTING RECORD     *         
***********************************************************************         
FMTNEW   NTR1                                                                   
         SR    R6,R6                                                            
         CLI   STIRA,2             AMEND?                                       
         BNE   *+8                                                              
         LH    R6,SLRL             YES                                          
         LH    R5,STIL                                                          
         LA    R5,1(R5)                                                         
         SR    R5,R6               R5=L'FORMAT AREA                             
         BNP   FMTN4                                                            
         LA    R6,IOAREA(R6)       R6=A(FORMAT AREA)                            
FMTN1    CH    R5,=H'80'           SET RECORD TO ALL BLANKS                     
         BNH   FMTN2                                                            
         MVC   0(80,R6),BLANKS                                                  
         LA    R6,80(R6)                                                        
         SH    R5,=H'80'                                                        
         B     FMTN1                                                            
FMTN2    SH    R5,=H'1'                                                         
         BM    FMTN4                                                            
         EX    R5,FMTN3                                                         
         B     FMTN4                                                            
FMTN3    MVC   0(0,R6),BLANKS                                                   
*                                                                               
FMTN4    CLI   STIFT,2                                                          
         BE    FMTIS                                                            
         CLI   STIFT,4                                                          
         BE    FMTDAL                                                           
         TM    STIFTL,X'01'                                                     
         BO    FMTREQ                                                           
         B     FMTLEN                                                           
*                                  INDEX SEQUENTIAL                             
FMTIS    CLI   STIFRT,2                                                         
         BNL   FMTDAL              V/L I/S SAME AS DAL                          
         SR    R5,R5                                                            
         ICM   R5,3,STIFRL         ZERO CONTROL & POINTER                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    IOAREA(0),IOAREA                                                 
         IC    R5,STIFKL           SET KEY TO INPUT VALUE                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     FMTNEWX                                                          
         MVC   IOAREA(0),STIK                                                   
*                                  DIRECT ACCESS LINKED                         
FMTDAL   CLI   STIRA,2             EXTENDING AMEND                              
         BE    FMTDAL1             YES                                          
         SR    R5,R5               ZERO CONTROL & SYSTEM                        
         SR    R6,R6                                                            
         IC    R5,STIFKL                                                        
         IC    R6,STIFCL                                                        
         AR    R5,R6                                                            
         IC    R6,STIFSL                                                        
         AR    R5,R6               R5=L'KEY+L'CONTROL+L'SYSTEM                  
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    IOAREA(0),IOAREA                                                 
         LA    R5,1(R5)                                                         
         LH    R6,STIL                                                          
         SR    R6,R5               R6=RECLEN-HDRLEN-1                           
         LA    R7,IOAREA(R5)                                                    
         MVI   0(R7),X'FF'         SET ELEMENT CODE TO X'FF'                    
         STC   R6,1(R7)            SET ELEMENT LENGTH TO RECORD LENGTH          
         IC    R5,STIFKL           SET KEY TO INPUT VALUE                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IOAREA(0),STIK                                                   
FMTDAL1  LH    R6,STIL             SET LAST BYTE TO ZERO                        
         LA    R6,IOAREA(R6)                                                    
         MVI   0(R6),0                                                          
*                                                                               
FMTLEN   CLI   STIFRLBN,X'FF'      RECORD LENGTH STORED IN RECORD?              
         BE    FMTNEWX             NO                                           
         SR    R5,R5               YES LOCATE POSITION IN RECORD                
         IC    R5,STIFRLBN                                                      
         LA    R5,IOAREA(R5)                                                    
         LH    R6,STIL                                                          
         LA    R6,1(R6)                                                         
         STH   R6,DUB                                                           
         MVC   0(2,R5),DUB         SET RECORD IN REC                            
         B     FMTNEWX                                                          
*                                  REQUEST FILE                                 
FMTREQ   XC    IOAREA(26),IOAREA   SET REQUEST FILE HEADER                      
         CLI   STIK+1,0                                                         
         BNE   FMTREQ1                                                          
         SR    R0,R0               CONVERT BINARY REQ # TO ALPHA                
         IC    R0,STIK                                                          
         CVD   R0,DUB                                                           
         UNPK  STIK(2),DUB                                                      
         OI    STIK+1,X'F0'                                                     
FMTREQ1  MVI   IOAREA+15,1                                                      
         MVC   IOAREA+26(2),STIK                                                
         B     FMTNEWX                                                          
*                                                                               
FMTNEWX  MVC   SLIOAREA,IOAREA     SAVE 1ST PART OF RECORD                      
         B     XIT                                                              
                                                                                
***********************************************************************         
* CALLINTF - DOES ALL THE FUNCTION TO THE PFM INTERFACE               *         
***********************************************************************         
CALLINTF NTR1                                                                   
         TM    STATFLAG,X'80'      INTERFACE ACTIVE?                            
         BO    INTERF1             YES                                          
         OI    STATFLAG,X'80'      INTERFACE NOW ACTIVE ?                       
         LA    R2,2                PAGE 2                                       
         BAS   RE,SAVETWA          SAVE PFM SCREEN                              
         XC    DISKADDR,DISKADDR                                                
         XC    XKEY,XKEY           NO TRANSFER KEY YET                          
         GOTO1 ASCANNER,DMCBWS,PFMFILEH,(3,IOAREA),C',=/.'                      
         CLI   4(R1),2                                                          
         BNE   INTERF0                                                          
         LA    R6,IOAREA+32        GO IMMEDIATELY TO THE 2ND                    
         CLI   0(R6),0             NO LENGTH = NO RECORD TYPE                   
         BE    INTERF0                                                          
         ZIC   R1,0(R6)            LEN(RECORD TYPE STRING)                      
         STC   R1,XKEY             STORE LENGTH AWAY                            
         MVC   XKEY+1(10),12(R6)   MOVE RECORD TYPE INTO XKEY                   
         TM    STATFLAG,X'40'      INTERFACE ACTIVE BEFORE?                     
         BZ    INTERF0             NO                                           
         CLI   IRIDL,1             JUST AN EQUAL SIGN                           
         BNE   INTERF0             SOME PARAMTERS                               
         LA    R2,3                                                             
         BAS   RE,RESTTWA          RESTORE FORMER INTERFACE SCREEN              
         LA    R6,XKEY                                                          
         USING TF05FFD,R3          R3=A(TWA)                                    
         LA    R2,CONRCRDH                                                      
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),1(R6)       NOT THE SAME RECORD TYPE                     
         BNE   INTERF0                                                          
         MVI   CONLAST+1,1         REWRITE TWA                                  
         MVI   CONLAST+2,1                                                      
         B     EXIT                                                             
*                                                                               
         USING PFMSAVED,R3         R3=A(TWA)                                    
INTERF0  MVC   DMCBWS+4(4),=X'D90F05FF' PFM INTERFACE MAIN SCREEN               
         L     RF,ACALLOV          NEED CALLOV TO DO THIS                       
         GOTO1 (RF),DMCBWS,(0,64(R3))                                           
         LA    R6,XKEY                                                          
         USING TF05FFD,R3          R3=A(TWA)                                    
         MVI   CONLAST+1,1         REWRITE TWA                                  
         MVI   CONLAST+2,1                                                      
         CLI   0(R6),0             NOTHING FOR RECORD                           
         BE    INTERF0A                                                         
         LA    R2,CONRCRDH                                                      
         MVC   5(1,R2),0(R6)       COPY THE LENGTH IF ANY                       
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),1(R6)       COPY RECORD TYPE INTO FIELD                  
         OI    6(R2),X'80'                                                      
         USING PFMSAVED,R3                                                      
         NI    STATFLAG,X'FF'-X'40'  MAKE IT LOOK LIKE FIRST TIME               
         USING TF05FFD,R3          R3=A(TWA)                                    
INTERF0A CLI   IRIDL,1             JUST AN EQUAL SIGN?                          
         BE    INTERF1                                                          
*                                                                               
         CLI   WORK+1,C','         IF '=,'                                      
         BE    INTERF1             THEN TREAT IT AS A '='                       
*                                                                               
         ZIC   R1,IRIDL            GET THE LENGTH                               
         LA    R2,80                                                            
         SR    R2,R1                                                            
         LA    R1,WORK(R1)         1 BEYOND LAST BYTE OF STRING                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),=80C' '                                                  
         GOTO1 ASCANNER,HEXWS,(C'C',WORK+1),(12,IOAREA)                         
         LA    R6,IOAREA                                                        
         LA    R2,CONI0H           1ST INPUT FIELD BESIDES RECORD TYPE          
         CLI   HEXWS+4,0                                                        
         BE    INTERF1                                                          
         ZIC   R4,HEXWS+4          # OF ITEMS                                   
SCANLOOP CLI   0(R6),0             LENGTH OF ITEM ZERO?                         
         BE    SCANNEXT                                                         
         ZIC   R1,2(R6)            GET VALIDITY BITS                            
         SRL   R1,4                PREPARE IT FOR TWA FORMAT                    
         STC   R1,4(R2)                                                         
         OI    4(R2),X'80'         INPUT THIS TIME                              
         ZIC   R1,0(R6)                                                         
         STC   R1,5(R2)                                                         
         OI    6(R2),X'80'                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R6)                                                   
SCANNEXT LA    R6,32(R6)           NEXT ITEM BLOCK                              
         ZIC   R1,0(R2)            GO TO THE PROTECTED FIELD                    
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            GO TO THE NEXT INPUT FIELD                   
         AR    R2,R1                                                            
         BCT   R4,SCANLOOP         CONTINUE PUTTING THEM OUT                    
*                                                                               
         USING PFMSAVED,R3         R3=A(TWA)                                    
INTERF1  DS    0H                                                               
         XR    R1,R1                                                            
         ICM   R1,3,STIFDSP                                                     
         A     R1,AFILETBL                                                      
         MVC   FILETYPE,STIFT-STIFN+8(R1)  PASS THROUGH FILE TYPE               
         MVC   DMCBWS+4(4),=X'D90F0500' PFM INTERFACE CONTROLLER                
         LA    RF,ENDOF01          POINT TO THE END OF THIS OVERLAY             
         ST    RF,DMCBWS                                                        
         L     RF,ACALLOV          NEED CALLOV TO DO THIS                       
         GOTO1 (RF),DMCBWS                                                      
         CLI   4(R1),X'FF'         DID IT LOAD PROPERLY?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         L     R1,APARM                                                         
         GOTO1 (RF),(R1)           DO THE INTERFACE                             
         CLC   =C'K,',XKEY         TEST A KEY OR DSK ADDR RETURNED              
         BE    INTERFOK                                                         
         CLC   =C'A,',XKEY                                                      
         BNE   EXIT                                                             
INTERFOK LA    R2,3                PAGE 3                                       
         BAS   RE,SAVETWA          SAVE INTERFACE SCREEN                        
         MVC   WORK(L'DISKADDR+L'XKEY),DISKADDR                                 
         LA    R2,2                PAGE 2                                       
         BAS   RE,RESTTWA          RESTORE THE PFM SCREEN                       
*NOP*    USING TF01FFD,R3                                                       
*NOP*    MVI   PFMLAST+1,1         THE 2 BYTES AFTER LAST X'00'                 
*NOP*    MVI   PFMLAST+2,1         TRANSMIT ENTIRE SCREEN                       
         USING PFMSAVED,R3                                                      
         NI    STATFLAG,X'FF'-X'80'  INTERFACE ISN'T ACTIVE ANYMORE             
         OI    STATFLAG,X'40'                                                   
         MVC   DISKADDR,WORK                                                    
         MVC   XKEY,WORK+L'DISKADDR                                             
         B     EXIT                                                             
                                                                                
***********************************************************************         
* SAVES THE TWA IN THE PAGE SPECIFIED IN R2                           *         
***********************************************************************         
SAVETWA  NTR1                                                                   
         XC    DMCBWS(24),DMCBWS                                                
         SLL   R2,32-8             MOVE PAGE NUMBER                             
         ICM   R2,3,2(R3)          TERMINAL NUMBER                              
         L     RF,ADATAMGR                                                      
         GOTO1 (RF),DMCBWS,(0,=C'DMWRT'),=C'TEMPSTR',(R2),64(R3)                
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* RESTORES THE TWA IN THE PAGE SPECIFIED IN R2                        *         
***********************************************************************         
RESTTWA  NTR1                                                                   
         XC    DMCBWS(24),DMCBWS                                                
         MVC   DMCBWS+20(2),=C'L='                                              
         MVC   DMCBWS+22(2),=X'0800' PARTIAL RESTORE                            
         SLL   R2,32-8             MOVE PAGE NUMBER                             
         ICM   R2,3,2(R3)          TERMINAL NUMBER                              
         L     RF,ADATAMGR                                                      
         GOTO1 (RF),DMCBWS,(0,=C'DMREAD'),=C'TEMPSTR',(R2),64(R3)               
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
EXIT     XMOD1 1                                                                
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
HFILTABL DC    Y(FILTABLQ)                                                      
DLINLEN  DC    Y(PFMDL2OH-PFMDL1OH)                                             
HLINLEN  DC    Y(PFMHL2OH-PFMHL1OH)                                             
DWDISP   DC    H'20'               DECIMAL DISPLAY CHAR WIDTH                   
DNDISP   DC    H'14'               DECIMAL NUMBER OF DISPLAY LINES              
HWDISP   DC    H'16'               HEX DISPLAY CHAR WIDTH                       
HNDISP   DC    H'16'               HEX NUMBER OF DISPLAY LINES                  
*                                                                               
DLWRHDR  DC    C'---BYTES---'      UPPER/LOWERCASE INDICATORS                   
DUPRHDR  DC    C'***BYTES***'                                                   
HLWRHDR  DC    C'----HEX----'                                                   
HUPRHDR  DC    C'****HEX****'                                                   
ZEROS    DS    0XL120                                                           
         DC    120X'00'                                                         
         SPACE 2                                                                
         LTORG                                                                  
MAXLEN   DC    F'18431'                                                         
ENDOF01  DS    0H                                                               
         EJECT                                                                  
* GEPFMSAVE                                                                     
       ++INCLUDE GEPFMSAVE                                                      
         EJECT                                                                  
* GEPFMTEMP                                                                     
       ++INCLUDE GEPFMTEMP                                                      
         EJECT                                                                  
* GEPFMDS                                                                       
       ++INCLUDE GEPFMDS                                                        
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
* FATIOB                                                                        
       ++INCLUDE FATIOB                                                         
* GEKEYFFD                                                                      
       ++INCLUDE GEKEYFFD                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEPFM01   06/14/13'                                      
         END                                                                    
