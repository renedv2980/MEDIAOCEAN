*          DATA SET RECNT6C    AT LEVEL 058 AS OF 11/14/03                      
*PHASE T8026CA,+0                                                               
         TITLE 'T8026C - GRAPHNET AND WPVI FORMATS'                             
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT6C (T8026C) --- GRAPHNET AND WPVI FORMATS               *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 22JAN03 HQ  CHANGE C/O FORMAT TO ADV NAME C/O AGY NAME              *         
* 03OCT02 SKU FLIGHTED BUY BUG FIX                                    *         
* 12JUL02 SKU COMBO PRINT BUG FIX                                     *         
* 10JAN01 RHV SPORTS BUYS                                             *         
* 19DEC97 JRD CARE OF AGENCY FOR GRAPHNET(RADIO)                      *         
* 11AUG97 RHV MARK 'REVISION' WORKSHEETS                              *         
* 17JUN97 RHV SHOW REP NAME                                           *         
* 21OCT96 RHV PRINT K ORD CMT AFTER BUYLINES & CONTROL PAGE BREAKING  *         
* 07OCT96 SKU SUPPORT LOW POWER STATION                               *         
* 06MAY96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 09APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV SUPPRESS DISPLAY OF EI FIELDS WHEN EMPTY                *         
*             NEW DISPLAY LAST VER# AND MOD#                          *         
* 08MAR96 SKU STATION AND ZIP BUG FIX                                 *         
* 01MAR96 RHV SUPPORT PETRY 34 BYTE ADDR AGY FIELDS                   *         
* 26FEB96 SKU KATZ CONVERTED ORDERS PRINT AGY ADDRESS IN X'71,72,73'  *         
* 06JAN96 SKU PROFILE 24 FOR TYPE D AND PROFILE 20 FOR TYPE N/X       *         
*             PRINT PTP NAME/PH# OVER SALESPERSON'S                   *         
* 12DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T8026C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8026C,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R7,4(R1)                                                         
         USING MYD,R7                                                           
         ST    RB,MYRB                                                          
         ST    R9,MYR9                                                          
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
         SPACE 2                                                                
         CLI   8(R1),1             ARE WE SETTING ADDRESSES ONLY                
         BE    EXXMOD                                                           
         SPACE 2                                                                
         CLI   FORMAT,C'G'         GRAPHNET FORMAT                              
         BE    GRAPH                                                            
         CLI   FORMAT,C'P'         WPVI FORMAT                                  
         BE    WPVI                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO GRAPHNET SPECIFICATIONS               
         SPACE 1                                                                
GRAPH    DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    G1G                 YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1D                                                              
         MVC   P(19),=C'*REP ORDER COMMENT*'                                    
         BAS   RE,GOSPOOL                                                       
*                                                                               
G1B      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1C                                                              
*                                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,G1B                                                           
*                                                                               
G1C      BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
G1D      LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1G                                                              
         MVC   P(23),=C'*STATION ORDER COMMENT*'                                
         BAS   RE,GOSPOOL                                                       
*                                                                               
G1E      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1F                                                              
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,G1E                                                           
*                                                                               
G1F      BAS   RE,GOSPOOL                                                       
*                                                                               
G1G      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PGRAFD,R3                                                        
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       SINGLE SPACING FOR GRAPHNET                  
         BE    G2                                                               
         SLL   R2,1                DOUBLE SPACING                               
G2       DS    0H                                                               
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
G5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    G5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    G5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,GRABLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
*                                                                               
G5A      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF          HAVE WE BEEN CALLED JUST TO FORCE            
         TM    TWAWSFLG,X'01'      HEADERS AND ORD CMT TO PRINT?                
         BO    XIT                 YES - WE'RE DONE FOR NOW                     
         DROP  RF                                                               
*                                                                               
         CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   G5D                                                              
         MVC   PGDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PGDAY,PDAY          DAY                                          
         MVC   PGTIM,PTIM          TIME                                         
         B     G50                                                              
         SPACE 1                                                                
G5D      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PGRAT-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'             
         B     G50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BNZ   G5G                                                              
         MVC   PGDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         MVC   PGDAT,PDAT          DATE                                         
         MVC   PGCLS,PCLS          CLASS                                        
         MVC   PGNPW,PNPW          NUMBER PER WEEK                              
         MVC   PGTSPOT,PTOT        TOTAL SPOTS                                  
         B     G50                                                              
         SPACE 1                                                                
G5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   G5H                                                              
         MVC   PGDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     G50                                                              
G5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   G6                                                               
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   G5S                                                              
         MVC   PGR(9),=C'*REP CMT-'                                             
         MVC   9(60,R3),PDAY+10                                                 
         B     G50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
G5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   G5T                                                              
         MVC   9(12,R3),=C'PROGRAMMING='                                        
         MVC   21(64,R3),PDAY+8                                                 
         B     G50                                                              
*                                                                               
G5T      MVC   9(66,R3),PDAY+6                                                  
         B     G50                                                              
         SPACE 1                                                                
G6       MVC   PGCHG,PCHG          REVISION CODE                                
         OC    PGCHG,PGCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PGLIN,PLIN          LINE NUMBER                                  
         MVC   PGDAT,PDAT          DATES                                        
         MVC   PGRAT,PRAT          RATE                                         
         MVC   PGTSPOT,PTOT        TOTAL SPOTS                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    G10                 NO                                           
         DROP  RF                                                               
         MVC   PGDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     G40                                                              
G10      DS    0H                                                               
         MVC   PGDAY,PDAY          DAY                                          
         MVC   PGTIM,PTIM          TIME                                         
         MVC   PGCLS,PCLS          CLASS                                        
         MVC   PGLEN,PLEN          LEN                                          
         MVC   PGDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         MVC   PGNPW,PNPW          NUMBER PER WEEK                              
         SPACE 1                                                                
         OC    PSEC(7),SPACES                                                   
         CLC   PSEC(7),SPACES      NO SECTION OR PLAN                           
         BE    G40                                                              
*                                  IF THERE'S SECTION OR PLAN                   
         BAS   RE,GOSPOOL          PRINT 1ST DATA LINE                          
         LA    R6,P                AND FILL P WITH 2ND DATA LINE                
         USING PGRAF2D,R6                                                       
         MVC   PGSEC,PSEC          SECTION                                      
         MVC   PGPLN,PPLN          PLAN                                         
         DROP  R6                                                               
G40      B     G50                                                              
         SPACE 1                                                                
G50      BAS   RE,GOSPOOL                                                       
         LA    R4,L'PRTLN(R4)                                                   
         LTR   R5,R5               CHECK FOR ZERO CONDITION                     
         BZ    EXXMOD                                                           
         BCT   R5,G5                                                            
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO WPVI SPECIFICATIONS                   
         SPACE 1                                                                
WPVI     DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    PVI1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    PVI1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
PVI1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    PVI1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,PVI1B                                                         
*                                                                               
PVI1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
PVI1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    PVI1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
PVI1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    PVI1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,PVI1E                                                         
*                                                                               
PVI1F    BAS   RE,GOSPOOL                                                       
*                                                                               
PVI1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
*                                                                               
         MVI   SVSTAOP2,C'Y'       FORCE TRIPLE SPACING FOR WPVI                
*                                                                               
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PWPVID,R3                                                        
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
P2L      MH    R2,=H'3'            TRIPLE SPACING                               
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
P5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    P5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    P5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,PVIBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
P5A      CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   P5D                                                              
         MVC   PPDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PPDAY,PDAY          DAY                                          
         MVC   PPTIM,PTIM          TIME                                         
         B     P50                                                              
         SPACE 1                                                                
P5D      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PPRAT-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'             
         B     P50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    P6                  USE EXITING CODE TO FILL IN                  
         SPACE 1                                                                
P5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   P5H                                                              
         MVC   PPDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     P50                                                              
P5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   P5V                                                              
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   P5L                                                              
         MVC   PP+6(13),=C'*REP ORD CMT*'                                       
         B     P5P                                                              
P5L      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   P5S                                                              
         MVC   PP+6(13),=C'*STA ORD CMT*'                                       
P5P      MVC   21(60,R3),PDAY+10                                                
         B     P50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
P5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   P5T                                                              
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     P50                                                              
*                                                                               
P5T      MVC   21(66,R3),PDAY+6                                                 
         B     P50                                                              
         SPACE 1                                                                
P5V      L     R6,AIO2      GET TOTAL BUYLINE COST-ONLY FOR MAIN LINE           
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'      IF DELETED                                   
         BO    P6                  DON'T SHOW TOTAL COST                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    P6                  DON'T SHOW TOTAL COST                        
         SPACE 1                                                                
         LA    R2,RBUYELEM                                                      
         USING RBUYELEM,R2                                                      
         EDIT  (4,RBUYTCOS),(12,PPTCOS),2,COMMAS=YES,FLOAT=-                    
         CLC   PPTCOS+10(2),=C'00'  DON'T PRINT ZERO CENTS                      
         BNE   P6                                                               
         CLC   PPTCOS+8(2),=C' .'  UNLESS THE WHOLE THING IS ZERO               
         BE    P6                                                               
         MVC   PPTCOS+10(2),SPACES                                              
         DROP  R2,R6                                                            
         SPACE 1                                                                
P6       MVC   PPCHG,PCHG          REVISION CODE                                
         OC    PPCHG,PPCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         MVC   PPLIN,PLIN          LINE NUMBER                                  
         SPACE 1                                                                
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    P7                  NO                                           
         DROP  RF                                                               
         MVC   PPDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     P7A                                                              
*                                                                               
P7       MVC   PPDAY,PDAY          DAY                                          
         MVC   PPTIM,PTIM          TIME                                         
         MVC   PPLEN,PLEN          LEN                                          
         MVC   PPNPW,PNPW          NUMBER PER WEEK                              
         MVC   PPTSPOT,PTOT        TOTAL SPOTS                                  
         MVC   PPCLS,PCLS          CLASS                                        
         MVC   PPSEC,PSEC          SECTION                                      
         MVC   PPPLN,PPLN          PLAN                                         
*                                                                               
P7A      LA    R6,PPDAT            DATES                                        
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK    BLANK OUT LEADING ZEROS             
         OC    DMCB(4),DMCB                                                     
         BZ    P10                                                              
         CLI   WORK+2,C'0'                                                      
         BE    P8                                                               
         MVC   0(1,R6),WORK+2                                                   
         LA    R6,1(R6)                                                         
P8       MVC   0(1,R6),WORK+3                                                   
         MVI   1(R6),C'/'                                                       
         CLI   WORK+4,C'0'                                                      
         BE    P8L                                                              
         MVC   2(1,R6),WORK+4                                                   
         LA    R6,1(R6)                                                         
P8L      MVC   2(1,R6),WORK+5                                                   
         SPACE 1                                                                
P10      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    P15                                                              
         MVI   3(R6),C'-'                                                       
         LA    R6,4(R6)                                                         
         CLI   WORK+2,C'0'                                                      
         BE    P12                                                              
         MVC   0(1,R6),WORK+2                                                   
         LA    R6,1(R6)                                                         
P12      MVC   0(1,R6),WORK+3                                                   
         MVI   1(R6),C'/'                                                       
         CLI   WORK+4,C'0'                                                      
         BE    P12L                                                             
         MVC   2(1,R6),WORK+4                                                   
         LA    R6,1(R6)                                                         
P12L     MVC   2(1,R6),WORK+5                                                   
         SPACE 1                                                                
*                                                                               
P15      CLI   PDAT-1,C'A'         ALTERNATE WEEKS                              
         BNE   *+14                                                             
         MVC   PPDAT-1(1),PDAT-1   * AND A ARE INDICATORS OF                    
         MVI   3(R6),C'A'          ALTERNATE WEEKS                              
*                                                                               
         MVC   PPRAT,PRAT          RATE                                         
         CLC   PPRAT+8(2),=C'00'   DON'T PRINT ZERO CENTS                       
         BNE   P50                                                              
         CLC   PPRAT+6(2),=C' .'   UNLESS THE WHOLE THING IS ZERO               
         BE    P50                                                              
         MVC   PPRAT+8(2),SPACES                                                
*                                                                               
P50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,4          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,P5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE SPOOL INTERFACE                                
         SPACE 1                                                                
GOSPOOL  NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVI   SPACING,3           FOR WPVI ALWAYS TRIPLE SPACE                 
         CLI   FORMAT,C'P'                                                      
         BE    GOSPL1                                                           
*                                                                               
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       FOR GRAPHNET, Y=SINGLE SPACE                 
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
GOSPL1   DS    0H                                                               
         TM    TWAWSFLG,X'20'                                                   
         BZ    GOSPL2                                                           
         MVI   SPACING,1                                                        
GOSPL2   GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  RF                                                               
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         LA    R4,H16                                                           
         ZIC   R3,XTRHED                                                        
GOSPL5   MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         MVI   SPACING,1           DON'T DOUBLE SPACE HEADLINES                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL5                                                        
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR SPACING                 
         SPACE 1                                                                
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         MVI   SPACING,3                                                        
         CLI   FORMAT,C'P'                                                      
         BE    GOSPL20                                                          
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'                                                    
         BNE   *+8                                                              
         MVI   SPACING,1           TRIPLE SPACING                               
GOSPL20  GOTO1 SPOOL,DMCB,(R8)                                                  
GOSPLX   MVI   MYHEDSW,C'N'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   NTR1                                                                   
         L     R3,0(R1)                                                         
*                                                                               
         MVC   WORK2X(L'KEY),KEY     SAVE OFF KEY FOR RESTORE                   
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(3,0(R3)),(R2),DATAMGR,RCONREC,GETTXT              
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         CLI   FORMAT,C'G'         FOR G FORMAT, CMTS GO FLUSH LEFT             
         BE    PSTCMT15                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         B     PSTCMT16                                                         
PSTCMT15 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R2)                                                       
PSTCMT16 BAS   RE,GOSPOOL                                                       
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(IOAREA-GENOLD+1001)                                      
         CR    R4,R2                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
         CLI   FORMAT,C'G'         FOR G FORMAT, CMTS GO FLUSH LEFT             
         BE    PSTCMT22                                                         
         MVC   P+30(60),0(R3)                                                   
         OC    P+30(60),SPACES                                                  
         B     PSTCMT25                                                         
PSTCMT22 DS    0H                                                               
         MVC   P(60),0(R3)                                                      
         OC    P(60),SPACES                                                     
*                                                                               
PSTCMT25 BAS   RE,GOSPOOL                                                       
*                                                                               
PSTCMTX  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'04'           NEED TO RESTORE SEQ LOOP?               
         BZ    XIT                                                              
         DROP  RF                                                               
         MVC   KEY,WORK2X               RESTORE BYREC SEQ LOOP                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GENERATE HEADLINES (HEADHOOK)                         
         SPACE 2                                                                
HOOK     NTR1  BASE=MYRB                                                        
         L     R9,MYR9                                                          
         CLI   FORMAT,C'G'         GRAPHNET FORMAT                              
         BE    HD10                                                             
         CLI   FORMAT,C'P'         WPVI FORMAT                                  
         BE    HD500                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
HD10     DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,6          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLI   RCSUBPRG,5                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,H1+6)                                       
         UNPK  DUB,SENDTIME        TIME                                         
         MVC   H1+20(2),DUB+1                                                   
         MVI   H1+22,C'.'                                                       
         MVC   H1+23(2),DUB+3                                                   
*                                                                               
         TM    PROFILES+CNTREVIB,CNTREVIA   MARK AS 'REVISION'?                 
         BZ    HD15                         NO                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASTAOP,X'40'      OVERRIDE W/STATION OPT?                      
         BO    HD15                                                             
         DROP  RF                                                               
         CLI   SVVER,1             VERSION > 1?                                 
         BNH   HD15                NO - SKIP MARKING AS REVISION                
         TM    SVCONF,X'80'        CONFIRMED?                                   
         BZ    HD15                YES - SKIP MARKING AS REVISION               
         MVC   H1+40(40),REVISION                                               
*                                                                               
HD15     MVC   H2+9(8),CONCNUM                                                  
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H2+25,GENOLD                                
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H3+9(L'TWAREPNM),TWAREPNM   REP NAME                             
*                                                                               
         CLC   PAGE,=X'0001'       FIRST PAGE?                                  
         BE    HD30                YES                                          
         MVC   H2+55(11),=C'(CONTINUED)'                                        
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H4,0                                                             
         LA    R6,H5                                                            
         BAS   RE,GRABLH                                                        
         B     XIT                                                              
*                                                                               
HD30     DS    0H                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+10                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H4+55(6),=C'RESENT'                                              
*                                                                               
***>     MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
***>     MVC   SVPRNT,P            SAVE PRINT LINE                              
***>     MVC   P,SPACES            BLANK OUT PRINT LINE                         
***>     MVI   XTRHED,0            MAXIMUM NUMBER OF EXTRA HEADLINES            
***>     OI    SPOOLIND,X'80'  DON'T SKIP LINE AFTER 1ST HEADS                  
*                                                                               
         MVC   H4+9(5),ACTSTAT    STATION                                       
         CLI   ACTSTAT+4,C' '                                                   
         BNE   *+10                                                             
         MVC   H4+13(3),=C'-TV'                                                 
*                                                                               
         CLI   ACTSTAT+4,C'L'                                                   
         BNE   *+10                                                             
         MVC   H4+13(3),=C'-L '                                                 
*                                                                               
         CLI   SVVER,1             NO RECAP IF VERSION ONE                      
         BE    HD60                                                             
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BZ    *+10                                                             
         MVC   H4+55(6),=CL6'RECAP'  IF RECAP, SAY SO                           
*                                                                               
HD60     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H5+9(20),TWASALNM                                                
         DROP  RF                                                               
*                                                                               
HD90     DS    0H                                                               
         TM    PROFILES+CNTSFONB,CNTSFONA PRINT SALESPERSON PHONE ON            
         BZ    HD110                      ALL VER??                             
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H5+30(12),TWASALTL  YES, PRINT PHONE                             
         DROP  RF                                                               
*                                                                               
HD100    DS    0H                                                               
         CLC   SVSASST,SPACES      FILL IN SALES ASST IF POSSIBLE               
         BE    HD120                                                            
         MVI   H5+43,C'/'                                                       
         MVC   H5+44(9),SVSASST                                                 
         B     HD120                                                            
*                                                                               
HD110    DS    0H                  NO, DON'T PRINT PHONE                        
         CLC   SVSASST,SPACES      FILL IN SALES ASST IF POSSIBLE               
         BE    HD120                                                            
         MVI   H5+30,C'/'                                                       
         MVC   H5+31(9),SVSASST                                                 
*                                                                               
HD120    MVC   H6+9(20),CONOFFN                                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD130                                                            
         MVC   H6+30(16),=C'SALESPERSON FAX#'                                   
         MVC   H6+47(L'TWASALFX),TWASALFX                                       
         B     HD140                                                            
*                                                                               
HD130    DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    HD140                                                            
         MVC   H6+30(12),=C'OFFICE FAX# '                                       
         MVC   H6+42(3),TWAOFFFX                                                
         MVI   H6+45,C'-'                                                       
         MVC   H6+46(3),TWAOFFFX+3                                              
         MVI   H6+49,C'-'                                                       
         MVC   H6+50(4),TWAOFFFX+6                                              
*                                                                               
HD140    DS    0H                                                               
         TM    TWAPRFW,X'20'               CONTYPE FORMAT OPTION #3?            
         BZ    HD144                       NO                                   
         MVC   H7(3),=C'AOR'               YES - REPL AGY W/AOR                 
         B     *+10                                                             
HD144    MVC   H7(3),=C'AGY'                                                    
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD146               NO                                           
*                                                                               
         LA    RE,H7+9                                                          
         MVC   0(L'TWAADVNM,RE),TWAADVNM                                        
         LA    RE,L'TWAADVNM+1(RE)                                              
*                                                                               
         CLI   0(RE),X'40'         CHOP SPACE                                   
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         LA    RE,2(RE)                                                         
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM2,RE),TWAAGNM2  AGENCY NAME FROM SCREEN               
         B     HD148                                                            
*                                                                               
HD146    DS    0H                                                               
         MVC   H7+9(33),TWAAGNM2   AGENCY NAME FOR CONTRACT                     
*                                                                               
HD148    DS    0H                                                               
         CLI   SVVER,1             ONLY SHOW ADD ON VER 1                       
         BE    HD150                                                            
         TM    PROFILES+CNTAADDB,CNTAADDA PRINT AGY ADDRESS ON ALL VER?         
         BZ    HD220                                                            
*                                                                               
HD150    DS    0H                                                               
         MVC   H8+9(34),TWAAGAD1   ADDRESS                                      
         MVC   H9+9(36),TWAAGAD2                                                
         MVC   H10+9(36),TWAAGAD3                                               
*                                                                               
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD160                                                            
         MVC   H8+42(4),=C'PH #'  AGENCY PHONE NUMBER                           
         MVC   H8+47(3),TWAAGYPH                                                
         MVI   H8+50,C'-'                                                       
         MVC   H8+51(3),TWAAGYPH+3                                              
         MVI   H8+54,C'-'                                                       
         MVC   H8+55(4),TWAAGYPH+6                                              
*                                                                               
HD160    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD170                                                            
         MVC   H9+42(4),=C'FAX#' AGENCY FAX NUMBER                              
         MVC   H9+47(3),TWAAFAX                                                 
         MVI   H9+50,C'-'                                                       
         MVC   H9+51(3),TWAAFAX+3                                               
         MVI   H9+54,C'-'                                                       
         MVC   H9+55(4),TWAAFAX+6                                               
*                                                                               
HD170    DS    0H                                                               
         MVC   H11+9(20),TWABUYER                                               
         MVC   H12+9(20),TWAADVNM                                               
         MVC   H13+9(20),CONPRD                                                 
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H13+9(20),TWAPRDNM                                               
         DROP  RF                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD200                                                            
         USING RCONIEL,R6                                                       
         MVC   H12+42(4),RCONIADV                                               
         MVC   H12+51(4),RCONIPRD                                               
         MVC   H12+60(10),RCONXEST                                              
         OC    H12+60(10),MYSPACES                                              
         CLC   H12+60(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H12+60(4),RCONIEST                                               
         MVC   H13+51(4),RCONIPR2                                               
         MVC   H12+34(2),=C'EI'                                                 
         MVC   H12+38(3),=C'ADV'                                                
         MVC   H12+47(3),=C'PRD'                                                
         MVC   H12+56(3),=C'EST'                                                
         MVC   H13+47(3),=C'PRD'                                                
         DROP  R6                                                               
         SPACE 1                                                                
HD200    GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H14+9)                               
         MVI   H14+18,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H14+20)                            
         SPACE 1                                                                
         B     XIT                                                              
*                                                                               
HD220    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H8+9(20),TWABUYER                                                
         MVC   H9+9(20),TWAADVNM                                                
         MVC   H11+9(20),CONPRD                                                 
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H11+9(20),TWAPRDNM                                               
         DROP  RF                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD230                                                            
         USING RCONIEL,R6                                                       
         MVC   H9+42(4),RCONIADV                                                
         MVC   H9+51(4),RCONIPRD                                                
         MVC   H9+60(10),RCONXEST                                               
         OC    H9+60(10),MYSPACES                                               
         CLC   H9+60(10),MYSPACES                                               
         BNE   *+10                                                             
         MVC   H9+60(4),RCONIEST                                                
         MVC   H11+51(4),RCONIPR2                                               
         MVC   H9+35(2),=C'EI'                                                  
         MVC   H9+39(3),=C'ADV'                                                 
         MVC   H9+48(3),=C'PRD'                                                 
         MVC   H9+57(3),=C'EST'                                                 
         MVC   H11+48(3),=C'PRD'                                                
         DROP  R6                                                               
         SPACE 1                                                                
HD230    GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H12+9)                               
         MVI   H12+18,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H12+20)                            
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*    WPVI HEADLINES                                                             
         SPACE 1                                                                
HD500    DC    0H'0'                                                            
         CLC   =C'MGS',CONACT                                                   
         BNE   HD510                                                            
         MVC   H1+41(24),=C'MAKEGOOD OFFER WORKSHEET'                           
         MVC   H2+41(24),=24C'-'                                                
*                                                                               
HD510    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H1(33),TWAREPNM     REP NAME                                     
         DROP  RF                                                               
         MVC   H2(5),ACTSTAT       STATION                                      
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-TV'                                                  
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-L '                                                  
         MVC   H2+9(20),SVSTAMKT   MARKET                                       
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H3,GENOLD                                   
*                                                                               
         MVC   H3+50(8),CONCNUM                                                 
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    HD560                                                            
         MVC   H4+45(9),=C'TRAFFIC #'                                           
         MVC   H4+55(10),SVTRAF                                                 
HD560    GOTO1 DATCON,DMCB,(5,0),(8,H2+89)                                      
         MVC   H2+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H2+103(2),DUB+1     TIME                                         
         MVI   H2+105,C'.'                                                      
         MVC   H2+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS DUPLICATE COPY                   
         MVC   H3+99(9),=C'DUPLICATE'                                           
         B     HD570                                                            
*                                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     HD570                                                            
*                                                                               
         MVC   H3+100(8),=C'FROM REP'                                           
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H3+96(12),=C'FROM STATION'                                       
         SPACE 1                                                                
HD570    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD580                                                            
         NI    SPOOLIND,X'7F'                                                   
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H4,0                                                             
         LA    R6,H5                                                            
         BAS   RE,PVIBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD580    CLC   SVAGYC,SPACES                                                    
         BE    *+16                                                             
         MVC   H4(8),=C'AGENCY #'                                               
         MVC   H4+9(10),SVAGYC     AGENCY CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD590               NO                                           
*                                                                               
         LA    RE,H5                                                            
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD592                                                            
*                                                                               
HD590    DS    0H                                                               
         MVC   H5(33),TWAAGNM2     AGENCY NAME FOR CONTRACT                     
*                                                                               
HD592    DS    0H                                                               
         MVC   H6(34),TWAAGAD1     ADDRESS                                      
         MVC   H7(36),TWAAGAD2                                                  
         MVC   H8(36),TWAAGAD3                                                  
         DROP  RF                                                               
*                                                                               
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD620                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD620                                                            
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BZ    HD610                                                            
         MVC   H5+46(5),=C'RECAP'  IF RECAP, SAY SO                             
         B     HD620                                                            
*                                                                               
HD610    DS    0H                                                               
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD620                                                            
         MVC   H5+46(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD620    DS    0H                                                               
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD630                                                            
         USING RCONIEL,R6                                                       
         MVC   H10+33(16),=C'EI  ADV      EST'                                  
         MVC   H11+37(12),=C'PRD      PRD'                                      
         MVC   H10+41(4),RCONIADV                                               
         MVC   H10+50(10),RCONXEST                                              
         OC    H10+50(10),MYSPACES                                              
         CLC   H10+50(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H10+50(4),RCONIEST                                               
         MVC   H11+41(4),RCONIPRD                                               
         MVC   H11+50(4),RCONIPR2                                               
         DROP  R6                                                               
*                                                                               
HD630    CLC   SVADVC,SPACES                                                    
         BE    *+16                                                             
         MVC   H4+66(12),=C'ADVERTISER #'                                       
         MVC   H4+79(10),SVADVC    ADVERTISER CODE                              
         MVC   H6+66(20),CONPRD    PRODUCT                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H9(20),TWABUYER     BUYER NAME                                   
         MVC   H5+66(20),TWAADVNM  ADVERTISER                                   
         MVC   H8+66(20),TWASALNM  SALESPERSON NAME                             
         CLC   SVCONPRD,SPACES                                                  
         BE    HD640                                                            
         MVC   H6+66(20),TWAPRDNM                                               
         DROP  RF                                                               
         MVC   H6+95(3),=C'CTG'                                                 
         MVC   H6+99(2),SVPRDCTG   CATEGORY                                     
         SPACE 1                                                                
HD640    GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H7+66)                               
         MVI   H7+75,C'-'                                                       
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H7+77)                             
*                                                                               
HD670    DS    0H                                                               
         MVC   H8+89(20),CONOFFN   OFFICE NAME                                  
         EDIT  (1,SVWKS),(2,H7+89)                                              
         MVC   H7+92(3),=C'WKS'                                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD680                                                            
         MVC   H10(7),=C'PHONE #'  AGENCY PHONE NUMBER                          
         MVC   H10+8(3),TWAAGYPH                                                
         MVI   H10+11,C'-'                                                      
         MVC   H10+12(3),TWAAGYPH+3                                             
         MVI   H10+15,C'-'                                                      
         MVC   H10+16(4),TWAAGYPH+6                                             
*                                                                               
HD680    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD690                                                            
         MVC   H11(7),=C'FAX   #'  AGENCY FAX NUMBER                            
         MVC   H11+8(3),TWAAFAX                                                 
         MVI   H11+11,C'-'                                                      
         MVC   H11+12(3),TWAAFAX+3                                              
         MVI   H11+15,C'-'                                                      
         MVC   H11+16(4),TWAAFAX+6                                              
*                                                                               
HD690    DS    0H                                                               
         MVC   H9+66(16),=C'SALESPERSON PH #'                                   
         MVC   H9+83(L'TWASALTL),TWASALTL                                       
*                                                                               
HD720    DS    0H                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD730                                                            
         MVC   H10+66(16),=C'SALESPERSON FAX#'                                  
         MVC   H10+83(L'TWASALFX),TWASALFX                                      
         B     HD740                                                            
*                                                                               
HD730    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD740                                                            
         MVC   H10+66(12),=C'OFFICE FAX# '                                      
         MVC   H10+78(3),TWAOFFFX                                               
         MVI   H10+81,C'-'                                                      
         MVC   H10+82(3),TWAOFFFX+3                                             
         MVI   H10+85,C'-'                                                      
         MVC   H10+86(4),TWAOFFFX+6                                             
*                                                                               
HD740    DS    0H                  SALES ASSISTANT                              
         MVC   H11+66(16),=C'SALES ASSISTANT:'                                  
         OC    TWASALAS,TWASALAS                                                
         BZ    HD750                                                            
         MVC   H11+83(L'TWASALAS),TWASALAS                                      
         DROP  RF                                                               
*                                                                               
HD750    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
GRABLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(24,R6),=C'MC LN DAYS         TIMES'                            
         MVC   30(13,R6),=C'LEN EFF DATES'                                      
         MVC   47(22,R6),=C'CLS NPW RATE       TOT'                             
         LA    R6,132(R6)                                                       
         MVC   47(22,R6),=C'SEC PLA            SPT'                             
         LA    R6,132(R6)                                                       
         MVC   0(29,R6),=C'-- -- ------------ ----------'                       
         MVC   30(16,R6),=C'--- ------------'                                   
         MVC   47(22,R6),=C'--- --- ---------- ---'                             
GRABLHX  B     XIT                                                              
*                                                                               
PVIBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(4,R6),=C'MOD/'                                                 
         MVC   37(23,R6),=C' EFFECTIVE    PER TOTAL'                            
         MVC   61(22,R6),=C'                FLIGHT'                             
         MVC   87(20,R6),=C'               PLAN '                               
         LA    R6,132(R6)                                                       
         SPACE 1                                                                
         MVC   0(35,R6),=C'LINE#     DAYS         TIME     LEN'                 
         MVC   37(23,R6),=C'   DATES       WK SPOTS'                            
         MVC   61(21,R6),=C' RATE/SPOT      TOTAL'                              
         MVC   87(20,R6),=C'CLASS SEC PLAN PRICE'                               
         LA    R6,132(R6)                                                       
         SPACE 1                                                                
         MVC   0(37,R6),=C'------ -----------  ----------- ---- '               
         MVC   37(23,R6),=C'------------  --- -----'                            
         MVC   61(25,R6),=C'------------ ------------'                          
         MVC   87(20,R6),=C'----- --- ---- -----'                               
PVIBLHX  B     XIT                                                              
*        CONSTANTS, LITERAL POOL, ETC.                                          
         SPACE 1                                                                
DASH     DC    51C'-'                                                           
REVISION DC    C'*************** REVISION ***************'                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 2                                                                
HEDSPECS DS    0D                                                               
         SPROG 1,5,6          ***GRAPHNET - ALL PAGES***                        
         PSPEC H2,1,C'CON #'                                                    
         PSPEC H3,1,C'REP'                                                      
         SPACE 1                                                                
         SPROG 1              ***GRAPHNET - PAGE 1 ONLY***                      
         PSPEC H4,1,C'TO'                                                       
         PSPEC H5,1,C'FM'                                                       
         PSPEC H6,1,C'OFF'                                                      
         PSPEC H8,1,C'BYR'                                                      
         PSPEC H9,1,C'ADV'                                                      
         PSPEC H11,1,C'PDT'                                                     
         PSPEC H12,1,C'FLT'                                                     
         EJECT                                                                  
         SPROG 5              ***GRAPHNET - PAGE 1 VERSION 1 ONLY ***           
         PSPEC H4,1,C'TO'                                                       
         PSPEC H5,1,C'FM'                                                       
         PSPEC H6,1,C'OFF'                                                      
         PSPEC H8,1,C'ADDR'                                                     
         PSPEC H11,1,C'BYR'                                                     
         PSPEC H12,1,C'ADV'                                                     
         PSPEC H13,1,C'PDT'                                                     
         PSPEC H14,1,C'FLT'                                                     
         EJECT                                                                  
         SPROG 2,3        **WPVI WORKSHEET - ALL PAGES***                       
         SPACE 1                                                                
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H2,46,C'---------------'                                         
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
*    FORMAT FOR GRAPHNET WORKSHEET BUYLINES                                     
         SPACE 1                                                                
PGRAFD   DSECT                                                                  
PGR      DS    0CL110                                                           
PGCHG    DS    CL2                 CHANGE CODE                                  
PGLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PGDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PGTIM    DS    CL10                TIME                                         
         DS    CL1                                                              
PGLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PGDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PGCLS    DS    CL3                 CLASS                                        
         DS    CL1                                                              
PGNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL1                                                              
PGRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PGTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL41                                                             
         SPACE 2                                                                
PGRAF2D  DSECT                                                                  
         DS    CL47                                                             
PGSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PGPLN    DS    CL3                 PLAN                                         
         DS    CL56                                                             
         EJECT                                                                  
*    FORMAT FOR WPVI WORKSHEET BUYLINES                                         
         SPACE 1                                                                
PWPVID   DSECT                                                                  
PP       DS    0CL110                                                           
PPCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PPLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PPDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PPTIM    DS    CL10                TIME                                         
         DS    CL2                                                              
PPLEN    DS    CL4                 LENGTH                                       
         DS    CL1                                                              
PPDAT    DS    CL12                DATE                                         
         DS    CL2                                                              
PPNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL2                                                              
PPTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL4                                                              
PPRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PPTCOS   DS    CL12                TOTAL COST                                   
         DS    CL2                                                              
PPCLS    DS    CL3                 CLASS                                        
         DS    CL2                                                              
PPSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PPPLN    DS    CL3                 PLAN                                         
         DS    CL2                                                              
PPPRATE  DS    CL5                 PLAN PRICE                                   
         DS    CL5                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058RECNT6C   11/14/03'                                      
         END                                                                    
