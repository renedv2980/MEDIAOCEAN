*          DATA SET RECNT67    AT LEVEL 173 AS OF 03/15/05                      
*PHASE T80267A,+0                                                               
         TITLE 'T80267 - REP/JEFFERSON FORMATS'                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT67 (T80267) --- REP/JEFERSON FORMATS                    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 15MAR05 BU  VSS/IBS -> WORKSHEET FOR FORMAT 'J'                     *         
* 22JAN01 RHV PGM=                                                    *         
* 12NOV00 RHV SPORTS BUYS                                             *         
* 27SEP00 BU  TRADE PROCESSING                                        *         
* 29MAR00 RHV POINT PERSON                                            *         
* 19DEC97 JRD CARE OF AGENCY ON REP FORMAT                            *         
* 14JUL97 RHV AGY CMT ON CFC CONF ORD WORKSHEET                       *         
* 03JUN97 RHV CREATE CFC CONFRIMED ORDER WORKSHEET                    *         
* 12NOV96 SKU REMOVE 'ALTERNATE WEEKS' LITERAL                        *         
* 18JUL96 RHV PRINT K ORD CMT AFTER BUYLINES                          *         
* 08OCT96 SKU LOW POWER STATION                                       *         
* 07JUN96 SKU ADD EOP CODES                                           *         
* 29APR96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 08APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV NEW MOD/VERSION DISPLAY                                 *         
* 02APR96 RHV SUPPRESSION OF EI FIELDS WHEN EMPTY                     *         
* 22MAR96 RHV RECAP/CHANGES ONLY PER NEW RULES & PROFILES           *           
* 29FEB96 RHV SUPPORT PETRY 34 BYTE AGY ADDRESS FIELDS                *         
* 26FEB96 SKU KATZ CONVERTED ORDERS PRINT AGY ADDRESS IN X'71,72,73'  *         
* 06JAN96 SKU PROFILE 24 TO PRINT PTP OVER SALERSPERSON FOR TYPE D    *         
* 13DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80267   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80267,R9,RR=R8                                                
         ST    R8,MYRELO                                                        
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
*                                                                               
         MVC   WORK2X(27),KEY      SAVE KEY FOR RESTORE                         
*                                                                               
         CLI   FORMAT,X'FF'        BLAIR REP FORMAT                             
         BE    REPFMT                                                           
         CLI   FORMAT,C'R'         STATION REP FORMAT                           
         BE    REPFMT                                                           
         CLI   FORMAT,C'O'         WIDE ORBIT USES REP FMT                      
         BE    REPFMT                                                           
         CLI   FORMAT,C'J'         JEFFERSON FORMAT                             
         BE    JEFFMT                                                           
         CLI   FORMAT,C'X'         VSS/IBS   FORMAT                             
         BE    JEFFMT                                                           
         DC    H'0'                                                             
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      B     EXXMOD                                                           
         EJECT                                                                  
*  ROUTINE TO FORMAT BUY LINE TO REP WORKSHEET SPECIFICATIONS                   
         SPACE 1                                                                
REPFMT   DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    REP1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         TM    TWACFFLG,X'08'      CFC CONFIRMATION?                            
         BZ    REP1A               NO - CONTINUE                                
         BAS   RE,PRTCFC           PRINT CFC COMMENT AND K COMMENT              
         B     REP1G               YES - SKIP ORD COMMENTS                      
*                                                                               
REP1A    L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    REP1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
         DROP  RF                                                               
*                                                                               
REP1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    REP1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,REP1B                                                         
*                                                                               
REP1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
REP1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    REP1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
REP1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    REP1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,REP1E                                                         
*                                                                               
REP1F    BAS   RE,GOSPOOL                                                       
*                                                                               
REP1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PREPD,R3                                                         
         CLI   FORMAT,C'R'         TRIPLE SPACING IS FOR STATION                
         BE    *+8                 ONLY                                         
         MVI   SVSTAOP2,C'N'                                                    
*                                                                               
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'      TRIPLE SPACING                                
         BE    REP2                                                             
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
REP2     MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
*                                                                               
REP5     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    REP5AA              NO - SKIP                                    
         TM    TWACFFLG,X'08'      CFC CONFIRMATION?                            
         BO    *+14                YES - PRINT BUY HEADER                       
         CLC   CONACT,=C'CF  '     FOR CONFIRM, NEVER PRINT HEADER              
         BE    REP5AA                                                           
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,REPBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
*                                                                               
REP5AA   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF          HAVE WE BEEN CALLED JUST TO FORCE            
         TM    TWAWSFLG,X'01'      HEADERS AND ORD CMT TO PRINT?                
         BO    XIT                 YES - WE'RE DONE FOR NOW                     
         DROP  RF                                                               
*                                                                               
*        CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
*        BNE   REP5A                                                            
*        MVC   PRLEN+4(13),=C'ALTERNATE WKS'                                    
*        MVC   PRDAY,PDAY          DAY                                          
*        MVC   PRTIM,PTIM          TIME                                         
*        B     REP50                                                            
         SPACE 1                                                                
REP5A    CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PRTCOS-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'            
         B     REP50                                                            
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BNZ   REP5G                                                            
         MVC   PRDAT-1(1),35(R4)   * FOR ALTERNATE WEEKS                        
         MVC   PRDAT,PDAT          DATE                                         
         MVC   PRNPW,PNPW          NUMBER PER WEEK                              
         MVC   PRTRADE,PNPW+3      POSSIBLE TRADE FLAG                          
         MVC   PRTSPOT,PTOT        TOTAL SPOTS                                  
         MVC   PRCLA,PCLS          CLASS                                        
         B     REP50                                                            
         SPACE 1                                                                
REP5G    DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   REP5H                                                            
         MVC   PRDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     REP50                                                            
REP5H    DS    0H                                                               
         OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   REP6                                                             
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   REP5L                                                            
         MVC   PR+6(13),=C'*REP ORD CMT*'                                       
         B     REP5P                                                            
REP5L    CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   REP5S                                                            
         MVC   PR+6(13),=C'*STA ORD CMT*'                                       
REP5P    MVC   21(60,R3),PDAY+10                                                
         B     REP50                                                            
         SPACE 1                                                                
*    IT'S A BUY COMMENT, BUT THEY DON'T WANT THE LABEL TO PRINT                 
REP5S    CLC   =C'P=',PDAY+6                                                    
         BNE   REP5T                                                            
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     REP50                                                            
*                                                                               
REP5T    MVC   21(66,R3),PDAY+6                                                 
         B     REP50                                                            
         SPACE 1                                                                
REP6     MVC   PRCHG,PCHG          REVISION CODE                                
         OC    PRCHG,PRCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PRLIN,PLIN          LINE NUMBER                                  
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    REP10               NO                                           
         DROP  RF                                                               
         MVC   PRDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     REP15                                                            
REP10    DS    0H                                                               
         MVC   PRDAY,PDAY          DAY                                          
         MVC   PRTYP,PTYP          TYPE                                         
         MVC   PRTIM,PTIM          TIME                                         
         MVC   PRLEN,PLEN          LEN                                          
REP15    MVC   PRDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         MVC   PRDAT,PDAT          DATES                                        
         MVC   PRNPW,PNPW          NUMBER PER WEEK                              
         MVC   PRTRADE,PNPW+3      POSSIBLE TRADE FLAG                          
         MVC   PRRAT,PRAT          RATE                                         
         MVC   PRTSPOT,PTOT        TOTAL SPOTS                                  
         MVC   PRCLA,PCLS          CLASS                                        
         MVC   PRSEC,PSEC          SECTION                                      
         MVC   PRPLA,PPLN          PLAN                                         
         SPACE 1                                                                
         L     R6,AIO2             GET TOTAL BUYLINE COST                       
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'      IF DELETED                                   
         BO    REP40               DON'T SHOW TOTAL COST                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    REP40               DON'T SHOW TOTAL COST                        
         SPACE 1                                                                
         LA    R2,RBUYELEM                                                      
         USING RBUYELEM,R2                                                      
         EDIT  (4,RBUYTCOS),(12,PRTCOS),2,COMMAS=YES,FLOAT=-                    
         DROP  R2,R6                                                            
         SPACE 1                                                                
REP40    B     REP50                                                            
         SPACE 1                                                                
REP50    BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,4          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,REP5                                                          
         SPACE 1                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*  ROUTINE TO FORMAT BUY LINE TO JEFFERSON WORKSHEET SPECIFICATIONS             
         SPACE 1                                                                
JEFFMT   DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    JEF1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    JEF1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
JEF1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    JEF1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,JEF1B                                                         
*                                                                               
JEF1C    BAS   RE,GOSPOOL                                                       
*                                                                               
JEF1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    JEF1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
JEF1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    JEF1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,JEF1E                                                         
*                                                                               
JEF1F    BAS   RE,GOSPOOL                                                       
*                                                                               
JEF1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PJEFD,R3                                                         
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    JEF2                                                             
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
JEF2     MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
         SPACE 1                                                                
JEF5     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    JEF5AA                                                           
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    JEF5AA              NO - SKIP                                    
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,JEFBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
JEF5AA   CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   JEF5A                                                            
         MVC   PJDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PJDAY,PDAY          DAY                                          
         MVC   PJTIM,PTIM          TIME                                         
         OC    PDAY,PDAY           ONLY PRINT SLASHES IF                        
         BZ    *+10                ADDITIONAL DAY/TIME INFO                     
         MVC   PJCAL,=C'/ / / / / /'  FOR BRDCST PATTERN                        
         B     JEF50                                                            
         SPACE 1                                                                
         SPACE 1                                                                
JEF5A    CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PJRAT-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'             
         B     JEF50                                                            
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    JEF6                USE EXISTING CODE TO FILL IN                 
         SPACE 1                                                                
JEF5G    DS    0H                                                               
         CLI   PDAY,X'FF'          SPORTS BUY DESCRIPTIVE?                      
         BNE   JEF5H                                                            
         MVC   PJDAT(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     JEF50                                                            
JEF5H    DS    0H                                                               
         OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   JEF6                                                             
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   JEF5L                                                            
         MVC   PJ+6(13),=C'*REP ORD CMT*'                                       
         B     JEF5P                                                            
JEF5L    CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   JEF5S                                                            
         MVC   PJ+6(13),=C'*STA ORD CMT*'                                       
JEF5P    MVC   21(60,R3),PDAY+10                                                
         B     JEF50                                                            
         SPACE 1                                                                
*    IT'S A BUY COMMENT, BUT THEY DON'T WANT THE LABEL TO PRINT                 
JEF5S    CLC   =C'P=',PDAY+6                                                    
         BNE   JEF5T                                                            
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     JEF50                                                            
*                                                                               
JEF5T    MVC   21(66,R3),PDAY+6                                                 
         B     JEF50                                                            
         SPACE 1                                                                
JEF6     MVC   PJCHG,PCHG          REVISION CODE                                
         OC    PJCHG,PJCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
*                                                                               
         MVC   PJLIN,PLIN          LINE NUMBER                                  
         MVC   PJDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK       DATES                            
         OC    DMCB(4),DMCB                                                     
         BZ    JEFF10                                                           
         MVC   PJDAT(2),WORK+2                                                  
         MVC   PJDAT+2(2),WORK+4                                                
         SPACE 1                                                                
JEFF10   GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    JEFF15                                                           
         MVC   PJDAT+7(2),WORK+2                                                
         MVC   PJDAT+9(2),WORK+4                                                
         SPACE 1                                                                
JEFF15   CLI   PDAT+11,C'A'                                                     
         BNE   *+8                                                              
         MVI   PJDAT+11,C'A'       ALTERNATE WEEKS INDICATOR                    
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    JEF20               NO                                           
         DROP  RF                                                               
         MVC   PJCAL(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     JEF30                                                            
*                                                                               
JEF20    MVC   PJDAY,PDAY          DAY                                          
         MVC   PJTIM,PTIM          TIME                                         
         MVC   PJLEN,PLEN          LEN                                          
         MVC   PJNPW,PNPW          NUMBER PER WEEK                              
         MVC   PJTRADE,PNPW+3      POSSIBLE TRADE FLAG                          
         MVC   PJCAL,=C'/ / / / / /'   BROADCAST PATTERN                        
         MVC   PJCLA,PCLS          CLASS                                        
         MVC   PJSEC,PSEC          SECTION                                      
         MVC   PJPLA,PPLN          PLAN                                         
JEF30    MVC   PJRAT,PRAT          RATE                                         
         MVC   PJTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
JEF40    B     JEF50                                                            
         SPACE 1                                                                
JEF50    BAS   RE,GOSPOOL                                                       
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,JEF5                                                          
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
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         TM    TWAWSFLG,X'20'                                                   
         BZ    GOSPL2                                                           
         MVI   SPACING,1                                                        
GOSPL2   GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  RF                                                               
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         LA    R4,H15                                                           
         ZIC   R3,XTRHED                                                        
GOSPL5   MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         MVI SPACING,1             NO DOUBLE SPACE BTWN HEADS                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL5                                                        
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR SPACING                 
         SPACE 1                                                                
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
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
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         BAS   RE,GOSPOOL                                                       
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
         MVC   P+30(60),0(R3)                                                   
         OC    P+30(60),SPACES                                                  
*                                                                               
         BAS   RE,GOSPOOL                                                       
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
***********************************************************************         
* PRTCFC - PRINT CFC COMMENT AND K COMMENT                                      
***********************************************************************         
PRTCFC   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         BAS   RE,SETCOMBO         GET CORRECT K NUMBER                         
         XC    KEY,KEY                                                          
         MVI   KEY,RCFCKTYQ                                                     
         MVC   KEY+21(2),RCONKREP                                               
         MVC   KEY+23(4),FULL                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   PRTCFC50                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RCFCREC,R6                                                       
         MVC   P+6(66),=66C'*'                                                  
         MVC   P+21(36),=C' PARTIAL CONFIRMATION WITH COMMENTS '                
         BAS   RE,GOSPOOL                                                       
         TM    RCFCIFLG,X'80'                                                   
         BZ    *+10                                                             
         MVC   P+9(L'MGOYMSG),MGOYMSG                                           
         TM    RCFCIFLG,X'40'                                                   
         BZ    *+10                                                             
         MVC   P+9(L'MGONMSG),MGONMSG                                           
         MVI   P+6,C'*'                                                         
         MVI   P+71,C'*'                                                        
         BAS   RE,GOSPOOL                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRTCFC10 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PRTCFC20                                                         
         MVC   P+9(60),2(R6)                                                    
         MVI   P+06,C'*'                                                        
         MVI   P+71,C'*'                                                        
         BAS   RE,GOSPOOL                                                       
         B     PRTCFC10                                                         
PRTCFC20 DS    0H                                                               
         MVC   P+06(66),=66C'*'                                                 
         BAS   RE,GOSPOOL                                                       
         DROP  R6                                                               
*                                                                               
PRTCFC50 DS    0H                                                               
         OC    SVCONCOM(60),SVCONCOM         CONTRACT COMMENT LINE 1            
         BZ    PRTCFC60                                                         
         BAS   RE,GOSPOOL          SKIP A LINE                                  
         MVC   P+6(18),=C'*CONTRACT COMMENT*'                                   
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,SVCONCOM),(RC),RR=Y  PRINT K CMT 1            
*                                                                               
PRTCFC60 OC    SVCONCOM+60(60),SVCONCOM+60    CONTRACT COMMENT LINE 2           
         BZ    PRTCFC80                                                         
*                                                                               
         OC    SVCONCOM(60),SVCONCOM                                            
         BNZ   PRTCFC70                                                         
         BAS   RE,GOSPOOL                                                       
         MVC   P+6(18),=C'*CONTRACT COMMENT*'                                   
PRTCFC70 DS    0H                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,SVCONCOM+60),(RC),RR=Y  PRINT K CMT 2         
         BAS   RE,GOSPOOL                                                       
*                                                                               
PRTCFC80 DS    0H                                                               
         ICM   R2,15,ASVACMT       ADDRESS AGY COMMENT                          
         OC    0(60,R2),0(R2)                                                   
         BZ    PRTCFC90                                                         
         MVC   P+6(16),=C'*AGENCY COMMENT*'                                     
         GOTO1 =A(PSTCMT),DMCB,(3,0(R2)),(RC),RR=Y  PRINT AGY CMT 1             
         OC    60(60,R2),60(R2)                                                 
         BZ    PRTCFC90                                                         
         GOTO1 =A(PSTCMT),DMCB,(3,60(R2)),(RC),RR=Y  PRINT AGY CMT 2            
         BAS   RE,GOSPOOL                                                       
PRTCFC90 DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20' TURN OFF SINGLE SPACING                     
         DROP  RF                                                               
         MVC   KEY(27),WORK2X      RESTORE BYREC SEQ LOOP                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
*  IF COMBO ORDER, RETURNS LOWEST K NUMBER IN COMBO IN 'FULL' ELSE              
*  RETURNS K NUMBER                                                             
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   FULL,RCONKCON                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   SCMBX                                                            
*                                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
SCMB20   DS    0H                                                               
         CLC   FULL,0(R5)          FULL VS. CURRENT K?                          
         BL    *+10                FULL IS LOWER - SKIP                         
         MVC   FULL,0(R5)          FULL IS HIGHER - REPLACE W/CURRENT           
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         BCT   R3,SCMB20                                                        
*                                                                               
SCMBX    DS    0H                                                               
         B     EXXMOD                                                           
       ++INCLUDE RECFCMSG                                                       
                                                                                
*                                                                               
***********************************************************************         
*              ROUTINE TO GENERATE HEADLINES (HEADHOOK)                         
         SPACE 2                                                                
HOOK     NTR1  BASE=MYRB                                                        
         L     R9,MYR9                                                          
         CLI   FORMAT,X'FF'        BLAIR REP FORMAT                             
         BE    HD100                                                            
         CLI   FORMAT,C'R'         STATION REP FORMAT                           
         BE    HD100                                                            
         CLI   FORMAT,C'O'         WIDE ORBIT USES REP FMT                      
         BE    HD100                                                            
         CLI   FORMAT,C'J'         JEFFERSON FORMAT                             
         BE    HD500                                                            
         CLI   FORMAT,C'X'         VSS/IBS   FORMAT                             
         BE    HD500                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
HD100    DC    0H'0'                                                            
         CLC   =C'MGS',CONACT                                                   
         BNE   HD100A                                                           
         MVC   H1+41(24),=C'MAKEGOOD OFFER WORKSHEET'                           
         MVC   H2+41(24),=24C'-'                                                
*                                                                               
HD100A   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWACFFLG,X'08'      CFC?                                         
         BZ    HD100B              NO                                           
         MVC   H1+37(30),=C'PARTIAL CONFIRMED WITH COMMENT'                     
         MVC   H2+37(30),=30C'-'                                                
*                                                                               
HD100B   DS    0H                                                               
         MVC   H1(33),TWAREPNM     REP NAME                                     
*                                                                               
*                                  IF COMBO ORDER, DON'T SHOW STATION           
*                                  CALLS, SHOW '*COMBO*' INSTEAD                
         CLI   TWACOMBO,0                                                       
         BE    HD101                                                            
         MVC   H2(7),=C'*COMBO*'                                                
         B     HD103                                                            
         DROP  RF                                                               
*                                                                               
HD101    DS    0H                                                               
         MVC   H2(5),ACTSTAT       STATION                                      
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+14                                                             
         MVC   H2+4(3),=C'-TV'                                                  
         B     HD103                                                            
*                                                                               
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+14                                                             
         MVC   H2+4(3),=C'-L '                                                  
         B     HD103                                                            
*                                                                               
         CLI   ACTSTAT+4,C'A'                                                   
         BNE   *+14                                                             
         MVC   H2+4(3),=C'-AM'                                                  
         B     HD103                                                            
*                                                                               
         CLI   ACTSTAT+4,C'F'                                                   
         BNE   *+14                                                             
         MVC   H2+4(3),=C'-FM'                                                  
         B     HD103                                                            
*                                                                               
         CLI   ACTSTAT+4,C'C'                                                   
         BNE   HD103                                                            
         MVC   H2+0(4),SVCAM                                                    
         MVC   H2+4(3),=C'-AM'                                                  
         MVC   H2+8(4),SVCFM                                                    
         MVC   H2+12(3),=C'-FM'                                                 
         MVC   H2+17(20),SVSTAMKT                                               
         B     *+10                                                             
*                                                                               
HD103    MVC   H2+9(20),SVSTAMKT   MARKET                                       
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
*                                                                               
         GOTO1 VGENDMV,DMCB,RCONREC,H3,GENOLD                                   
*                                                                               
         LR    RF,RA               IF COMBO ORDER, DON'T SHOW CONTRACT          
         AH    RF,=Y(TWAWORKQ)     NUMBER.  K NUMS WILL BE DISPLAYED IN         
         USING TWAWORK,RF          BODY OF WORKSHEET                            
         CLI   TWACOMBO,0                                                       
         BNE   HD121                                                            
         DROP  RF                                                               
*                                                                               
         MVC   H3+50(8),CONCNUM                                                 
*                                                                               
HD121    OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    HD123                                                            
         MVC   H4+45(9),=C'TRAFFIC #'                                           
         MVC   H4+55(10),SVTRAF                                                 
HD123    GOTO1 DATCON,DMCB,(5,0),(8,H2+89)                                      
         MVC   H2+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H2+103(2),DUB+1     TIME                                         
         MVI   H2+105,C'.'                                                      
         MVC   H2+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS DUPLICATE COPY                   
         MVC   H3+99(9),=C'DUPLICATE'                                           
         B     HD124                                                            
*                                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     HD124                                                            
*                                                                               
         MVC   H3+100(8),=C'FROM REP'                                           
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H3+96(12),=C'FROM STATION'                                       
         SPACE 1                                                                
HD124    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD124A                                                           
         NI    SPOOLIND,X'7F'                                                   
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H4,0                                                             
         LA    R6,H5                                                            
         BAS   RE,REPBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD124A   DS    0H                                                               
         CLC   SVAGYC,SPACES       IF AGENCY CODE,                              
         BE    HD125                                                            
         MVC   H4(20),TWABUYER     PUT BUYER ONE LINE UP                        
         MVC   H5(8),=C'AGENCY #'  AND FILL IN AGENCY CODE                      
         MVC   H5+9(10),SVAGYC                                                  
         B     *+10                                                             
         SPACE 1                                                                
HD125    MVC   H5(20),TWABUYER     BUYER NAME                                   
         DROP  RF                                                               
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD130                                                            
         USING RCONIEL,R6                                                       
         MVC   H6+33(16),=C'EI  ADV      EST'                                   
         MVC   H7+37(12),=C'PRD      PRD'                                       
         MVC   H6+41(4),RCONIADV                                                
         MVC   H6+50(10),RCONXEST                                               
         OC    H6+50(10),MYSPACES                                               
         CLC   H6+50(10),MYSPACES                                               
         BNE   *+10                                                             
         MVC   H6+50(4),RCONIEST                                                
         MVC   H7+41(4),RCONIPRD                                                
         MVC   H7+50(4),RCONIPR2                                                
         DROP  R6                                                               
*                                                                               
HD130    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+10                NO                                           
         MVC   H5(20),TWAADVNM                                                  
*                                                                               
         LA    RE,H6                                                            
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+14                NO                                           
         MVC   0(04,RE),=C'C/O '                                                
         LA    RE,4(RE)                                                         
*                                                                               
         MVC   0(33,RE),TWAAGNM2           AGENCY NAME                          
         MVC   H7(L'TWAAGAD1),TWAAGAD1     ADDRESS                              
         MVC   H8(L'TWAAGAD2),TWAAGAD2                                          
         MVC   H9(L'TWAAGAD3),TWAAGAD3                                          
*                                                                               
         CLC   SVADVC,SPACES       IF ADV. CODE, FILL IT IN                     
         BE    *+16                                                             
         MVC   H4+66(12),=C'ADVERTISER #'                                       
         MVC   H4+79(10),SVADVC                                                 
         MVC   H5+66(20),TWAADVNM  ADVERTISER                                   
         MVC   H6+66(20),CONPRD    PRODUCT                                      
         CLC   SVCONPRD,SPACES                                                  
         BE    HD250                                                            
         MVC   H6+66(20),TWAPRDNM                                               
         MVC   H6+95(3),=C'CTG'                                                 
         MVC   H6+99(2),SVPRDCTG   CATEGORY                                     
         DROP  RF                                                               
*                                                                               
HD250    TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD280                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD280                                                            
         CLI   FORMAT,X'FF'        AND REP FORMAT                               
         BE    HD260                                                            
         TM    SVSTAT,X'04'        OR FMT C'R' & PRINT CHANGES ONLY             
         BZ    HD280                                                            
HD260    DS    0H                                                               
         CLC   =C'RSND',CONACT                                                  
         BE    HD265                                                            
         CLC   =C'LAST',CONACT                                                  
         BE    HD265                                                            
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BO    HD265                                                            
         TM    SVSTAT,X'04'                                                     
         BO    HD270                                                            
HD265    MVC   H5+50(5),=C'RECAP'                                               
         B     HD280                                                            
HD270    MVC   H5+46(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD280    GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H7+66)                               
         MVI   H7+75,C'-'                                                       
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H7+77)                             
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H8+66(20),TWASALNM  SALESPERSON NAME                             
*                                                                               
         MVC   H8+89(16),CONOFFN   OFFICE NAME                                  
         EDIT  (1,SVWKS),(2,H7+89)                                              
         MVC   H7+92(3),=C'WKS'                                                 
*                                                                               
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD320                                                            
         MVC   H10(7),=C'PHONE #'   AGENCY PHONE NUMBER                         
         MVC   H10+8(3),TWAAGYPH                                                
         MVI   H10+11,C'-'                                                      
         MVC   H10+12(3),TWAAGYPH+3                                             
         MVI   H10+15,C'-'                                                      
         MVC   H10+16(4),TWAAGYPH+6                                             
*                                                                               
HD320    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD330                                                            
         MVC   H11(7),=C'FAX   #'  AGENCY FAX NUMBER                            
         MVC   H11+8(3),TWAAFAX                                                 
         MVI   H11+11,C'-'                                                      
         MVC   H11+12(3),TWAAFAX+3                                              
         MVI   H11+15,C'-'                                                      
         MVC   H11+16(4),TWAAFAX+6                                              
*                                                                               
HD330    DS    0H                                                               
         MVC   H9+66(16),=C'SALESPERSON PH #'                                   
         MVC   H9+83(L'TWASALTL),TWASALTL                                       
*                                                                               
HD340    DS    0H                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD350                                                            
         MVC   H10+66(16),=C'SALESPERSON FAX#'                                  
         MVC   H10+83(L'TWASALFX),TWASALFX                                      
         B     HD360                                                            
*                                                                               
HD350    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD360                                                            
         MVC   H10+66(12),=C'OFFICE FAX# '                                      
         MVC   H10+78(3),TWAOFFFX                                               
         MVI   H10+81,C'-'                                                      
         MVC   H10+82(3),TWAOFFFX+3                                             
         MVI   H10+85,C'-'                                                      
         MVC   H10+86(4),TWAOFFFX+6                                             
*                                                                               
HD360    DS    0H                  SALES ASSISTANT                              
         MVC   H11+66(16),=C'SALES ASSISTANT:'                                  
         OC    SVSASST,SVSASST                                                  
         BZ    HD365                                                            
         MVC   H11+83(L'SVSASST),SVSASST                                        
*                                                                               
HD365    DS    0H                  POINT PERSON                                 
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    HD370               IF OFF SKIP POINT PERSON                     
         OC    WPTPEXP,SPACES                                                   
         CLC   WPTPEXP,SPACES                                                   
         BE    HD370                                                            
         MVC   H12+66(13),=C'POINT PERSON:'                                     
         MVC   H12+81(20),WPTPEXP     PRINT OUT POINT PERSON NAME               
*                                                                               
HD370    DS    0H                                                               
         DROP  RF                                                               
         B     XIT                                                              
         EJECT                                                                  
*  HEADLINES FOR JEFFERSON FORMAT                                               
         SPACE 2                                                                
HD500    DC    0H'0'                                                            
         CLI   RCSUBPRG,7                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,8          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         MVC   H2+8(5),ACTSTAT       STATION                                    
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+10                                                             
         MVC   H2+12(3),=C'-TV'                                                 
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+10                                                             
         MVC   H2+12(2),=C'-L'                                                  
         MVC   H2+17(20),SVSTAMKT   MARKET                                      
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H1,GENOLD                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,H2+87)                                      
         MVC   H2+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H2+103(2),DUB+1     TIME                                         
         MVI   H2+105,C'.'                                                      
         MVC   H2+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS DUPLICATE COPY                   
         MVC   H3+99(9),=C'DUPLICATE'                                           
         B     HD540                                                            
*                                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     HD540                                                            
*                                                                               
         MVC   H1+87(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+87(12),=C'FROM STATION'                                       
         SPACE 1                                                                
HD540    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD550                                                            
         MVC   H2+50(8),CONCNUM                                                 
         NI    SPOOLIND,X'7F'                                                   
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H3,0                                                             
         LA    R6,H4                                                            
         BAS   RE,REPBLH                                                        
         B     XIT                                                              
HD550    DS    0H                                                               
         MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,5            MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD560    MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD560                                                         
         SPACE 1                                                                
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD570                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD570                                                            
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD570                                                            
         MVC   H2+46(12),=C'CHANGES ONLY'   THEN SAY SO IN HEADLINE             
         SPACE 1                                                                
HD570    MVC   H7+14(10),SVAGYC      AGENCY CODE PRINTS IN CUST # FLD           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H7+41(20),TWABUYER    BUYER NAME                                 
         DROP  RF                                                               
         MVC   H7+77(2),SVOFF        OFFICE CODE                                
         MVC   H13+61(8),CONCNUM     CONTRACT NUMBER                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H7+80(6),TWAEOPSL  SALESPERSON CODE                              
         DROP  RF                                                               
*                                                                               
HD575    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD580               NO                                           
*                                                                               
         LA    RE,H16+9                                                         
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD582                                                            
*                                                                               
HD580    DS    0H                                                               
         MVC   H16+9(L'TWAAGNM2),TWAAGNM2  AGENCY NAME FROM SCREEN              
*                                                                               
HD582    DS    0H                                                               
         MVC   H17+9(34),TWAAGAD1    ADDRESS                                    
         MVC   H18+9(36),TWAAGAD2                                               
         MVC   H19+9(36),TWAAGAD3                                               
         DROP  RF                                                               
*                                                                               
HD595    DS    0H                                                               
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    HD600                                                            
         MVC   H7+5(4),SVTRAF    JDS TRAF IS ALWAYS ONLY 4 CHARACTERS           
         SPACE 1                                                                
HD600    MVC   H13(6),SVADVC       ADVERTISER CODE                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H13+8(20),TWAADVNM  ADVERTISER                                   
         MVC   H13+30(20),CONPRD    PRODUCT                                     
         CLC   SVCONPRD,SPACES                                                  
         BE    HD610                                                            
         MVC   H13+30(20),TWAPRDNM                                              
         DROP  RF                                                               
         SPACE 1                                                                
HD610    GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H16+50)                              
         MVI   H16+58,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H16+59)                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H7+87(20),TWASALNM  SALESPERSON NAME                             
         DROP  RF                                                               
*                                                                               
HD615    DS    0H                                                               
         EDIT  (1,SVWKS),(2,H16+74)                                             
         MVC   H16+69(3),=C'WKS'                                                
         SPACE 1                                                                
         MVC   H16+98(9),SVSASST   SALES ASSISTANT                              
         SPACE 1                                                                
*  DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                               
         SPACE 1                                                                
         MVC   H16(6),=C'AGENCY'                                                
         MVC   H17(8),=C'NAME AND'                                              
         MVC   H18(7),=C'ADDRESS'                                               
         MVC   H16+44(5),=C'DATES'                                              
         MVC   H16+80(15),=C'SALES ASSISTANT'                                   
*                                                                               
HD618    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H20                                                           
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD620                                                            
         MVC   0(7,R1),=C'PHONE #'   AGENCY PHONE NUMBER                        
         MVC   8(3,R1),TWAAGYPH                                                 
         MVI   11(R1),C'-'                                                      
         MVC   12(3,R1),TWAAGYPH+3                                              
         MVI   15(R1),C'-'                                                      
         MVC   16(4,R1),TWAAGYPH+6                                              
         LA    R1,27(R1)                                                        
*                                                                               
HD620    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD630                                                            
         MVC   0(5,R1),=C'FAX #'  AGENCY FAX NUMBER                             
         MVC   6(3,R1),TWAAFAX                                                  
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
*                                                                               
HD630    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H17+80(16),=C'SALESPERSON PH #'                                  
         MVC   H17+97(L'TWASALTL),TWASALTL                                      
*                                                                               
HD640    DS    0H                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD650                                                            
         MVC   H18+80(16),=C'SALESPERSON FAX#'                                  
         MVC   H18+97(L'TWASALFX),TWASALFX                                      
         B     HD660                                                            
*                                                                               
HD650    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD660                                                            
         MVC   H18+80(12),=C'OFFICE FAX# '                                      
         MVC   H18+92(3),TWAOFFFX                                               
         MVI   H18+95,C'-'                                                      
         MVC   H18+96(3),TWAOFFFX+3                                             
         MVI   H18+99,C'-'                                                      
         MVC   H18+100(4),TWAOFFFX+6                                            
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
HD660    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD670                                                            
         USING RCONIEL,R6                                                       
         MVC   H17+50(15),=C'EI ADV      EST'                                   
         MVC   H18+53(12),=C'PRD      PRD'                                      
         MVC   H17+57(4),RCONIADV                                               
         MVC   H17+66(10),RCONXEST                                              
         OC    H17+66(10),MYSPACES                                              
         CLC   H17+66(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H17+66(4),RCONIEST                                               
         MVC   H18+57(4),RCONIPRD                                               
         MVC   H18+66(4),RCONIPR2                                               
         DROP  R6                                                               
HD670    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
REPBLH   NTR1                                                                   
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         MVC   0(39,R6),=C'MOD/                                   '             
         MVC   39(29,R6),=C' EFFECTIVE   PER             '                      
         MVC   68(19,R6),=C'TOTAL    FLIGHT    '                                
         MVC   87(20,R6),=C'               PLAN '                               
         LA    R6,132(R6)                                                       
         MVC   0(39,R6),=C'LINE#     DAYS     TP    TIME     LEN  '             
         MVC   39(29,R6),=C'   DATES      WK  RATE/SPOT  '                      
         MVC   68(19,R6),=C'SPOTS    TOTAL     '                                
         MVC   87(20,R6),=C'CLASS SEC PLAN PRICE'                               
         LA    R6,132(R6)                                                       
         MVC   0(39,R6),=C'------ ----------- -- ----------- ---- '             
         MVC   39(29,R6),=C'------------ --- ----------- '                      
         MVC   68(19,R6),=C'----- ------------ '                                
         MVC   87(20,R6),=C'----- --- ---- -----'                               
REPBLHX  B     XIT                                                              
*                                                                               
JEFBLH   NTR1                                                                   
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         MVC   0(22,R6),=C'LN     T O START   END'                              
         MVC   24(30,R6),=C'    START   END       CALENDAR'                     
         MVC   57(3,R6),=C'ROT'                                                 
         MVC   84(24,R6),=C'TOT              LINE #/'                           
         LA    R6,132(R6)                                                       
         MVC   0(22,R6),=C'#   MC C R DATE   DATE'                              
         MVC   24(32,R6),=C'LEN TIME   TIME PC M/T/W/T/F/S/S'                   
         MVC   57(23,R6),=C'WK  SEC CLA PLA    RATE'                            
         MVC   84(26,R6),=C'SPT DAYS         DESC/TIME'                         
         LA    R6,132(R6)                                                       
         MVC   0(22,R6),=C'--- -- - - -----------'                              
         MVC   24(32,R6),=C'--- ----------- -- -------------'                   
         MVC   57(26,R6),=C'--- --- --- --- ----------'                         
         MVC   84(26,R6),=C'--- ------------ ---------'                         
JEFBLHX  B     XIT                                                              
         EJECT                                                                  
*        CONSTANTS, LITERAL POOL, ETC.                                          
         SPACE 1                                                                
MYRELO   DS    F                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 2                                                                
HEDSPECS DS    0D                                                               
         SPACE 1                                                                
         SPROG 3,4,5      ***REP WORKSHEET - ALL PAGES***                       
         SPACE 1                                                                
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H2,46,C'---------------'                                         
         PSPEC H1,103,PAGE                                                      
         EJECT                                                                  
         SPROG 7,8,9 *** JEFFERSON WORKSHEET - ALL PAGES*****                   
         SPACE 1                                                                
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H2,1,C'STATION'                                                  
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 7     ***JEFFERSON WRKSHEET - PAGE 1 ONLY ******                 
         SPACE 1                                                                
         PSPEC H4,1,C'RATE'                                                     
         PSPEC H5,1,C'CARD CON# TC  CUSTOMER #'                                 
         PSPEC H6,1,C'---- ---- --  ----------'                                 
         PSPEC H8,1,C'---- ---- --  ----------'                                 
         PSPEC H4,26,C'BILLING                              BILL'               
         PSPEC H5,26,C'CUSTOMER # AGY  ATTENTION            CYCLE'              
         PSPEC H6,26,C'---------- ---- -------------------- -----'              
         PSPEC H8,26,C'---------- ---- -------------------- -----'              
         PSPEC H4,69,C'REV          SSP'                                        
         PSPEC H5,69,C'CTG TRD  OF  #     SSP'                                  
         PSPEC H6,69,C'--- --- ---- ----- --------------------'                 
         PSPEC H8,69,C'--- --- ---- ----- --------------------'                 
         PSPEC H10,1,C'ADV'                                                     
         PSPEC H11,1,C'#       ADVERTISER'                                      
         PSPEC H12,1,C'------  --------------------'                            
         PSPEC H14,1,C'------  --------------------'                            
         PSPEC H11,31,C'PRODUCT'                                                
         PSPEC H12,31,C'--------------------'                                   
         PSPEC H14,31,C'--------------------'                                   
         PSPEC H10,53,C'PROD               CO-  CON  SPEC'                      
         PSPEC H11,53,C'CODES    ACE #     OP   SEP  HDLG'                      
         PSPEC H12,53,C'-------  --------  ---  ---  ----'                      
         PSPEC H13,53,C'   /'                                                   
         PSPEC H14,53,C'-------  --------  ---  ---  ----'                      
         PSPEC H10,88,C'LOG   CON       INV'                                    
         PSPEC H11,88,C'TYPE  COM  INV  MSG'                                    
         PSPEC H12,88,C'----  ---  ---  ---'                                    
         PSPEC H14,88,C'----  ---  ---  ---'                                    
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
*                                                                               
* DSECT FOR REGENPBY OUTPUT                                                     
       ++INCLUDE REGENPBYD                                                      
*                                                                               
*           FORMAT FOR REP WORKSHEET BUY LINES                                  
PREPD    DSECT                                                                  
PR       DS    0CL110                                                           
PRCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PRLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PRDAY    DS    CL11                DAY                                          
         DS    CL1                                                              
PRTYP    DS    CL1                 TYPE                                         
         DS    CL2                                                              
PRTIM    DS    CL11                TIME                                         
         DS    CL1                                                              
PRLEN    DS    CL3                 LENGTH                                       
         DS    CL2                                                              
PRDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PRNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL1                                                              
PRTRADE  DS    CL1                                                              
PRRAT    DS    CL10                RATE                                         
         DS    CL3                                                              
PRTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL1                                                              
PRTCOS   DS    CL12                TOTAL COST                                   
         DS    CL2                                                              
PRCLA    DS    CL3                 CLASS                                        
         DS    CL2                                                              
PRSEC    DS    CL3                 SECTION                                      
         DS    CL2                                                              
PRPLA    DS    CL3                 PLAN                                         
         DS    CL1                                                              
PRPRATE  DS    CL5                 PLAN PRICE                                   
         DS    CL3                                                              
         EJECT                                                                  
*           FORMAT FOR JEFFERSON WORKSHEET BUY LINES                            
PJEFD    DSECT                                                                  
PJ       DS    0CL110                                                           
PJLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PJCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL5                                                              
PJDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PJLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PJTIM    DS    CL11                TIME                                         
         DS    CL5                                                              
PJCAL    DS    CL11                BROADCAST PATTERN                            
         DS    CL2                                                              
PJNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL1                                                              
PJSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PJCLA    DS    CL3                 CLASS                                        
         DS    CL1                                                              
PJPLA    DS    CL3                 PLAN                                         
PJTRADE  DS    CL1                                                              
PJRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PJTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL1                                                              
PJDAY    DS    CL12                DAY                                          
         DS    CL10                                                             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENCFC                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'173RECNT67   03/15/05'                                      
         END                                                                    
