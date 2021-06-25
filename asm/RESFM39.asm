*          DATA SET RESFM39    AT LEVEL 249 AS OF 02/24/15                      
*PHASE T81839B                                                                  
*&&      SET   TT=Y                                                             
*INCLUDE REPIO                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE UNBOOK                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE GETBROAD                                                               
         TITLE 'T81839 - RESFM39 - NEW BUSINESS ACTIVITY REPORT'                
*                                                                               
**********************************************************************          
*                                                                    *          
*        RESFM39 (T81839) --- BUSINESS ACTIVITY REPORT               *          
*                                                                    *          
* JUN10/98 (AST) - BIRTH DATE                                        *          
*                                                                    *          
* OCT20/98 (AST) - ADDED NATIONAL/LOCAL OFFICE FILTER                *          
*                                                                    *          
* MAY13/99 (AST) - USE RCONRFLT FOR PRINTING FLIGHT DATES IF '1E'    *          
*                      ELEM IS PRESENT                               *          
*                                                                    *          
* JAN03/00 (MLB) - ADDING OFFICE AND SALESPERSON FILTERING           *          
*                                                                    *          
* AUG10/00 (BU ) - TEST FOR BUYLINES ENTERED FOR PENDING             *          
*                                                                    *          
* AUG29/00 (BU ) - PENDING START DATE                                *          
*                                                                    *          
* JUN26/01 (BU ) - STATION SET FILTERING                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
                                                                                
T81839   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1839**,R7,RA,RR=RE                                           
         ST    RE,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R1,ABOX             FOR WIDE PRINT LINE                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         L     R1,BOXAWIDE                                                      
         ST    R1,ADRWIDE                                                       
         DROP  R1                                                               
*                                                                               
         BAS   RE,LOADTAB          LOAD TABLE FROM ACTIVITY RECORD              
         BAS   RE,LOADOFF                                                       
*                                                                               
         LA    R1,HOOK             DEFAULT HOOK/SPECS                           
         ST    R1,HEADHOOK                                                      
         L     R1,=A(REGSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
*                                                                               
MAIN04   DS    0H                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
         B    EXIT                                                              
****************************************************************                
NUMSTAS  EQU   11                                                               
NUMLVLS  EQU   5                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PROCESS REPORT                                  *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
PREP     DS    0H                                                               
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         XC    MYCOUNT,MYCOUNT     FOR TESTING                                  
         MVI   RIPSKIP,C'Y'        DON'T NEED PREVIOUS $                        
*                                                                               
         GOTO1 =A(INIT),DMCB,SORTREC,RR=RELO                                    
*                                                                               
*                                                                               
         BAS   RE,READRECS         READ CONTRACTS-PUT TO TSAR                   
*                                                                               
         BAS   RE,PRRECS           GET FROM TSAR-PRINT REPORT                   
*                                                                               
         BAS   RE,CLOSEUP          CLOSE IT UP                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
****************************************************************                
*   SUBROUTINE TO FILL TABLE WITH FIELDS FROM ACTIVITY RECORD                   
****************************************************************                
LOADTAB  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY,BACTKEY                                                      
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         LA    R3,SCRTAB                                                        
*                                                                               
LT10     DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    LTX                 END OF TABLE, DONE                           
         L     R6,AIO                                                           
         MVC   ELCODE,21(R3)                                                    
         BAS   RE,GETEL                                                         
         BNE   LT20                SKIP TO NEXT TABLE ENTRY                     
*                                                                               
         USING RACTELTP,R6                                                      
         MVC   18(1,R3),RACTELRL   COPY REQ LEN TO TABLE                        
         MVC   19(1,R3),RACTELCP   COPY PAGE BREAK FLAG                         
         MVC   20(1,R3),RACTELCN   COPY COLUMN NUMBER                           
*                                                                               
         CLI   19(R3),C'P'         IS THIS A PAGE BREAK?                        
         BNE   LT20                NO SKIP                                      
         CLI   21(R3),X'12'        IS THIS STATION ELEM?                        
         BNE   LT20                                                             
         OI    REPORT,RPTQSTA      STATION TYPE REPORT                          
*                                                                               
LT20     LA    R3,TABENTQ(R3)          GOTO NEXT TABLE ENTRY                    
         B     LT10                                                             
*                                                                               
LTX      L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET COMMENT VALUES                           
         BAS   RE,GETEL                                                         
         BNE   LTXX                NO COMMENT, EXIT                             
         USING RACTCOM,R6                                                       
*                                                                               
         MVC   ROWCOL,RACTCMRC                                                  
         MVC   COMLEN,RACTCMRL                                                  
         MVC   COMCOL,RACTCMCN                                                  
LTXX     B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
* CLOSE UP SHOP                                                  *              
******************************************************************              
CLOSEUP  NTR1                                                                   
         L     R1,TSARBUFF                       RETURN MEMORY                  
         L     R2,TSARBUFL         FREE TSAR + AFFL TABLE                       
         FREEMAIN RC,LV=(2),A=(1)                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* READ CONTRACT RECORDS                                          *              
******************************************************************              
READRECS NTR1                                                                   
                                                                                
GOREPIO  GOTO1 =V(REPIO),DMCB,REPIOTBL,0                                        
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         TM    RIPSTAT,RIPRQERR             ANY REQUIRED DATA MISSING           
         BZ    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         XC    RIPFULL,RIPFULL     IGNORE MAX IO COUNT FOR THIS REPRT           
         TM    RIPSTAT,RIPENDF     EOF                                          
         BO    RDRX                                                             
*                                                                               
*                                                                               
*   TEST                                                                        
**       L     R6,AIO                                                           
**       USING RCONREC,R6                                                       
**       CLC   =X'03845325',RCONKCON                                            
**       BNE   TEST0020                                                         
*                                                                               
*   TEST DISPLAY                                                                
**       L     R5,ADRWIDE                                                       
**       USING WIDED,R5                                                         
**       MVC   XP+1(09),=C'SPEC REC:'                                           
**       MVC   XP+12(23),RCONKEY                                                
**       GOTO1 HEXOUT,DMCB,RCONKCON,XP+44,4,=C'TOG'                             
**       BAS   RE,SPLAT                                                         
**       DROP  R6                                                               
         DROP  R5                                                               
TEST0020 EQU   *                                                                
*   TEST DISPLAY END                                                            
*                                                                               
*   TEST                                                                        
*                                                                               
***      TM    RIPSTAT,RIPMAXIO    RESTART ON MAX IO?                           
***      BZ    *+12                                                             
***      NI    RIPSTAT,X'FF'-RIPMAXIO                                           
***      B     GOREPIO                                                          
*                                                                               
* CONTRACT FILTERING                                                            
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
*   TEST DISPLAY                                                                
**       L     R5,ADRWIDE                                                       
**       USING WIDED,R5                                                         
**       MVC   XP+1(09),=C'CON READ:'                                           
**       MVC   XP+12(23),RCONKEY                                                
**       GOTO1 HEXOUT,DMCB,RCONKCON,XP+44,4,=C'TOG'                             
**       BAS   RE,SPLAT                                                         
**       DROP  R5                                                               
*   TEST DISPLAY END                                                            
*                                                                               
                                                                                
         CLC   =C'ACC-',RCONBUYR   SKIP ACC CONTRACTS                           
         BE    GOREPIO             GET NEXT CONTRACT                            
                                                                                
         L     R1,=A(OFFLIST)      REGION FILTERING ?                           
         A     R1,RELO                                                          
         CLI   0(R1),0                                                          
         BE    RDR2A               NO                                           
RDR2     CLC   RCONKOFF,0(R1)      YES / IS OFFICE VALID FOR REGION?            
         BE    RDR2A                                                            
         LA    R1,2(R1)            BUMP TO NEXT OFFICE IN LIST                  
         CLI   0(R1),0             EOF                                          
         BE    GOREPIO                                                          
         B     RDR2                                                             
                                                                                
RDR2A    TM    RCONMODR,X'08'      WAS/IS TAKEOVER                              
         BO    GOREPIO             EXCLUDE WAS/IS TAKEOVER                      
*                                                                               
***** OFFICE/SALESPERSON FILTERING                                              
*                                                                               
         L     R1,SETOFFS          ADDRESS OF THE TABLE                         
         CHI   R1,0                OFFICE FILTERING ?                           
         BE    STAFLTR             NO--GO TO STATION FILTERING                  
*                                                                               
         ZIC   RE,3(R1)            GET LENGTH OF SINGLE ENTRY                   
         TM    0(R1),X'20'         SET OF SETS ?                                
         BZ    OFFF0040                                                         
*                                                                               
         AHI   R1,4                SKIP FLAGS                                   
OFFF0020 AR    R1,RE               ADVANCE TO NEXT ENTRY                        
         CLI   0(R1),0             END OF TABLE ?                               
         BNE   OFFF0020                                                         
         AHI   R1,1                SKIP THE END OF TABLE MARKER                 
         B     *+8                                                              
*                                                                               
OFFF0040 AHI   R1,4                SKIP FLAGS                                   
         L     RE,SETOFFS                                                       
         TM    0(RE),X'40'         EXCLUSION SET ?                              
         BO    OFFF0080                                                         
*                                                                               
OFFF0060 CLI   0(R1),0             END OF TABLE ?                               
         BE    GOREPIO             YES--GET NEXT CONTRACT                       
         CLC   RCONKOFF,0(R1)      OFFICE CODE MATCH ?                          
         BE    STAFLTR                                                          
         AHI   R1,L'RCONKOFF       ADVANCE TO NEXT ENTRY                        
         B     OFFF0060                                                         
*                                                                               
OFFF0080 CLI   0(R1),0             END OF TABLE ?                               
         BE    STAFLTR                                                          
         CLC   RCONKOFF,0(R1)      OFFICE CODE MATCH ?                          
         BE    GOREPIO             YES--GO TO NEXT CONTRACT                     
         AHI   R1,L'RCONKOFF       ADVANCE TO NEXT ENTRY                        
         B     OFFF0080                                                         
*                                                                               
**<<>>                                                                          
*                                                                               
***** STATION FILTERING                                                         
*                                                                               
STAFLTR  EQU   *                                                                
         L     R1,SETSTAS          ADDRESS OF THE TABLE                         
         CHI   R1,0                STATION FILTERING ?                          
         BE    SPFLTR              NO--GO TO SALESPERSON FILTERING              
*                                                                               
         ZIC   RE,3(R1)            GET LENGTH OF SINGLE ENTRY                   
         TM    0(R1),X'20'         SET OF SETS ?                                
         BZ    STAF0040                                                         
*                                                                               
         AHI   R1,4                SKIP FLAGS                                   
STAF0020 AR    R1,RE               ADVANCE TO NEXT ENTRY                        
         CLI   0(R1),0             END OF TABLE ?                               
         BNE   STAF0020                                                         
         AHI   R1,1                SKIP THE END OF TABLE MARKER                 
         B     *+8                                                              
*                                                                               
STAF0040 AHI   R1,4                SKIP FLAGS                                   
         L     RE,SETSTAS                                                       
         TM    0(RE),X'40'         EXCLUSION SET ?                              
         BO    STAF0080                                                         
*                                                                               
STAF0060 CLI   0(R1),0             END OF TABLE ?                               
         BE    GOREPIO             YES--GET NEXT CONTRACT                       
         CLC   RCONKSTA,0(R1)      STATION CODE MATCH ?                         
         BE    SPFLTR                                                           
         AHI   R1,L'RCONKSTA       ADVANCE TO NEXT ENTRY                        
         B     STAF0060                                                         
*                                                                               
STAF0080 CLI   0(R1),0             END OF TABLE ?                               
         BE    SPFLTR                                                           
         CLC   RCONKSTA,0(R1)      STATION CODE MATCH ?                         
         BE    GOREPIO             YES--GO TO NEXT CONTRACT                     
         AHI   R1,L'RCONKSTA       ADVANCE TO NEXT ENTRY                        
         B     STAF0080                                                         
*                                                                               
**<<>>                                                                          
SPFLTR   L     R1,SETSALS          GET TABLE ADDRESS                            
         CHI   R1,0                SALESPERSON FILTERING ?                      
         BE    CTFLTR              NO--GO TO CONTRACT TYPE FILTERING            
*                                                                               
*   ALL CONTRACT RECORDS HAVE AN X'01' ELEMENT.  AS THIS CODE IS ONLY           
*        ACCESSED IF S/P FILTERING IS DONE, ONLY THE USING BELOW TOOK           
*        EFFECT.  THE CHANGE OF ADDRESS MAY OR MAY NOT BE DONE.  IF             
*        IT IS NOT DONE, THE ADDRESSES OF ALL SUBSEQUENT FIELDS ARE             
*        WRONG..   THIS TOOK ME HALF A DAY OR SO TO FIND!!                      
*                                                                               
*        MVI   ELCODE,X'01'                                                     
*        BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         L     R1,SETSALS          GET TABLE ADDRESS                            
****     *SING RCONELEM,R6                                                      
         ZIC   RE,3(R1)            GET LENGTH OF SINGLE ENTRY                   
         TM    0(R1),X'20'         SET OF SETS ?                                
         BZ    EXTEST2                                                          
*                                                                               
         AHI   R1,4                SKIP FLAGS                                   
FLTR40   AR    R1,RE               ADVANCE TO NEXT ENTRY                        
         CLI   0(R1),0             END OF TABLE ?                               
         BNE   FLTR40                                                           
         AHI   R1,1                SKIP THE END OF TABLE MARKER                 
         B     *+8                                                              
*                                                                               
EXTEST2  AHI   R1,4                SKIP FLAGS                                   
         L     RE,SETSALS                                                       
         TM    0(RE),X'40'         EXCLUSION SET ?                              
         BO    FLTR60                                                           
*                                                                               
FLTR50   CLI   0(R1),0             END OF TABLE ?                               
         BE    GOREPIO             YES--GET NEXT CONTRACT                       
         CLC   RCONSAL,0(R1)       SALES PERSON MATCH ?                         
         BE    CTFLTR                                                           
         AHI   R1,L'RCONSAL        ADVANCE TO NEXT ENTRY                        
         B     FLTR50                                                           
*                                                                               
FLTR60   CLI   0(R1),0             END OF TABLE ?                               
         BE    CTFLTR                                                           
         CLC   RCONSAL,0(R1)       OFFICE CODE MATCH ?                          
         BE    GOREPIO             YES--GO TO NEXT CONTRACT                     
         AHI   R1,L'RCONSAL        ADVANCE TO NEXT ENTRY                        
         B     FLTR60                                                           
*                                                                               
                                                                                
CTFLTR   CLI   CONTYPES,0          CONTRACT TYPE FILTERING ?                    
         BE    RDR3                NO                                           
                                                                                
         CLI   RCONTYPE,0          UNCODED?                                     
         BE    GOREPIO             YES - SKIP PER RRG/NRGON                     
                                                                                
         LA    RE,CONTYPES                                                      
         LA    RF,255(RE)                                                       
RDR2B    DS    0H                                                               
         CLI   0(RE),0             TYPE NOT FOUND                               
         BE    RDR2C                                                            
         CLC   RCONTYPE,0(RE)                                                   
         BE    RDR2D               TYPE FOUND                                   
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RDR2B                                                            
*                                                                               
RDR2C    TM    REPORT,RPTQNCT      NEGATIVE FILTER?                             
         BNO   GOREPIO             NO                                           
         B     RDR3                                                             
*                                                                               
RDR2D    TM    REPORT,RPTQNCT      NEGATIVE FILTER?                             
         BO    GOREPIO             YES                                          
*                                                                               
RDR3     DS    0H                                                               
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    RDR4                                                             
         CLI   RTBFLTR,C'Y'        READY TO BOOK FILTER 'Y'                     
         BE    GOREPIO             YES - MUST HAVE THIS ELEM                    
         B     RDR5                                                             
RDR4     DS    0H                                                               
         USING RSARXCO,R6                                                       
         TM    RSARXFLG,X'08'      FORECAST CONTRACT?                           
         BO    GOREPIO             YES - EXCLUDE                                
*                                                                               
         CLI   RTBFLTR,C'Y'        READY TO BOOK FILTER 'Y'                     
         BNE   *+12                                                             
         TM    RSARXFLG,X'02'      CONTRACT READY TO BOOK?                      
         BZ    GOREPIO             NO - EXCLUDE                                 
*                                                                               
         CLI   RTBFLTR,C'N'        READY TO BOOK FILTER 'N'                     
         BNE   *+12                                                             
         TM    RSARXFLG,X'02'      CONTRACT READY TO BOOK?                      
         BO    GOREPIO             YES - EXCLUDE                                
*                                                                               
RDR5     L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   NLFLAG,0            NO FILTER?                                   
         BE    RDR10               ALWAYS DISPLAY                               
*                                                                               
CS11     L     R1,=A(OFF2TBL)                                                   
         B     CSO02                                                            
CSO01    LA    R1,TBENTEQ(R1)      BUMP TO NEXT OFFICE IN TABLE                 
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BNE   CSO02               NO, SKIP                                     
* END OF TABLE, OFFICE MUST BE NATIONAL                                         
         CLI   NLFLAG,C'L'         LOCAL FILTER?                                
         BNE   RDR10               NO, DISPLAY RECORD                           
         B     GOREPIO             DON'T DISPLAY, GET NEXT K                    
CSO02    CLC   0(2,R1),RCONKOFF    COMPARE OFFICES                              
         BNE   CSO01               CHECK NEXT TABLE ENTRY                       
         CLC   NLFLAG,2(R1)        COMPARE FILTER TO TABLE                      
         BNE   GOREPIO             DON'T DISPLAY, GET NEXT K                    
*                                                                               
**** ELSE DISPLAY                                                               
*                                                                               
RDR10    EQU   *                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
* CHECK CONTRACT REC TO ASSIGN REPORT STATUS                                    
*                                                                               
* 1 = PENDING    = NO BUCKETS AND NO SPL ELEMENT                                
* 2 = INCOMPLETE = BUCKETS BUT NO SPL OR SPL FOR 1ST STATION ONLY               
* 3 = COMPLETED  = BUCKETS AND SPL FOR ALL STATIONS                             
* 4 = LOSSES     = 0$ FOR 1ST STATION, SPL FOR OTHER STATIONS                   
*                                                                               
         MVI   BYTE,0              USE BYTE AS CONDITION INDICATOR              
         TM    RCONMODR,X'10'      BUYS ENTERED FLAG SET?                       
         DROP  R6                                                               
         BNO   RDR11               NO                                           
         OI    BYTE,X'01'          YES                                          
RDR11    EQU   *                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        BUCKETS ELEMENT?                             
         BAS   RE,GETEL                                                         
         BNE   *+8                 NO                                           
         OI    BYTE,X'01'          YES/MARK X'01'=BUCKET ELEMENT                
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        IS THERE SPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    BYTE,X'02'          X'02'=SPL ELEMENT                            
         B     RDR12                                                            
                                                                                
* CHECK BYTE STATUE                                                             
RDR12    EQU   *                                                                
         CLI   BYTE,0              ..BYTE=0 = NO SPL/NO BUCKETS                 
         BNE   RDR12B                                                           
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         TM    RCONMODR,X'70'      IF BUY LINE ADDED, PREV. X'10'               
         DROP  R6                                                               
         BM    RDR80               IT'S INCOMPLETE, PREV. BO                    
         B     RDR20               ELSE PENDING                                 
                                                                                
RDR12B   CLI   BYTE,X'01'          BUCKETS ONLY?                                
         BE    RDR80               YES- SO ITS INCOMPLETE                       
*                                                                               
         L     R6,AIO              BUCKETS AND SPL                              
         MVI   ELCODE,X'06'        CHK IF SPL FOR ALL STATIONS                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSPEL,R6                                                      
         ZIC   R1,RCONSPNU         NUMBER OF MINI ELEMENTS                      
         LA    R2,RCONSPST         IS THERE SPL FOR 1ST STATION?                
         DROP  R6                                                               
         OC    5(4,R2),5(R2)                                                    
         BZ    *+8                                                              
         OI    BYTE,4              BYTE =X'04' = SPL FOR 1ST STATION            
         B     RDR13B              AND FOR OTHER STATIONS ?                     
RDR13    OC    5(4,R2),5(R2)       SPL AMOUNT?                                  
         BZ    *+8                 NO                                           
         OI    BYTE,8              BYTE =X'08' = SPL FOR OTHERS                 
         LA    R2,9(R2)            BUMP TO NEXT STATION/AMOUNT                  
RDR13B   BCT   R1,RDR13                                                         
         TM    BYTE,X'0F'          BUCKETS AND SPL FOR 1ST AND OTHERS ?         
         BNO   RDR14                                                            
         MVI   BYTE,3              BUCKETS AND SPL FOR ALL - COMPLETED          
         B     RDR40                                                            
                                                                                
* CHECK IF INCOMPLETE OR LOSSES                                                 
RDR14    EQU   *                                                                
         TM    BYTE,X'04'          IS THERE SPL FOR 1ST STATION ONLY            
*****    BO    RDR80               YES/INCOMPLETE ****                          
         BNO   RDR14B                                                           
         MVI   BYTE,3              CHANGED PER PATRICE - COMPLETED              
         B     RDR40                                                            
                                                                                
RDR14B   TM    BYTE,X'08'          IS IT SPL FOR OTHERS ONLY?                   
         BO    RDR15               YES/LOSSES                                   
         MVI   BYTE,3              ,,SPL BUT NO $ FOR ANYONE                    
         B     RDR40               ,,COMPLETED                                  
RDR15    MVI   BYTE,4                 SET LOSSES ID                             
         B     RDR40                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* - FILL IN PENDING RECORD X'01'                                                
RDR20    EQU   *                                                                
         OC    ACTPSTA,ACTPSTA     ANY PENDING START DATE?                      
         BZ    RDR22               NO  - PROCEED                                
         L     R6,AIO              YES - CHECK DATE ENTERED                     
         USING RCONREC,R6                                                       
         CLC   RCONHDRD,ACTPSTA    CONTRACT CREATED BEFORE START DATE?          
         DROP  R6                                                               
         BL    GOREPIO             YES - SKIP IT                                
*                                                                               
*                                                                               
RDR22    EQU   *                                                                
         TM    REPORT,RPTQPND      DO PENDING ?                                 
         BNO   GOREPIO                                                          
                                                                                
*                                                                               
*   TEST                                                                        
*        L     R6,AIO                                                           
*        USING RCONREC,R6                                                       
*        CLC   =X'03845325',RCONKCON                                            
*        BNE   TEST0120                                                         
*                                                                               
*   TEST DISPLAY                                                                
*        L     R5,ADRWIDE                                                       
*        USING WIDED,R5                                                         
*        MVC   XP+1(09),=C'SPEC REC:'                                           
*        MVC   XP+12(23),RCONKEY                                                
*        GOTO1 HEXOUT,DMCB,RCONKCON,XP+44,4,=C'TOG'                             
*        BAS   RE,SPLAT                                                         
*        MVC   XP+1(09),=C'CONTYPEX:'                                           
*        MVC   XP+12(50),CONTYPEX                                               
*        BAS   RE,SPLAT                                                         
*        DROP  R5,R6                                                            
TEST0120 EQU   *                                                                
*   TEST DISPLAY END                                                            
*                                                                               
         BAS   R5,CHKXCTYP         EXCLUDE CON TYPES                            
         BE    GOREPIO                                                          
                                                                                
         L     R6,AIO              YES - PENDING MUST HAVE SAR                  
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   GOREPIO                                                          
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVI   PNDKID,X'01'        PENDING ID                                   
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
*        CLI   KNUMFLG,C'Y'        IS K # IN SRTKEY ALREADY?                    
*        BE    *+10                YES, DON'T PUT IN AGAIN                      
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
                                                                                
* - NEED BOP COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         USING RCONBCEL,R6                                                      
         MVI   ELCODE,X'11'        BOP COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR30                                                            
         MVC   PNDCOMMT(3),=C'BOP'             BOP COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY                                            
         B     RDR30                                                            
         DROP  R5                                                               
                                                                                
* - PENDING DONE - PASS REC TO TSAR                                             
RDR30    EQU   *                                                                
         B     RDR100                                                           
                                                                                
         EJECT                                                                  
* COMPLETED BUSINESS / LOSSES                                                   
RDR40    EQU   *                                                                
         CLI   BYTE,3              3=COMPLETED                                  
         BNE   RDR42                                                            
         TM    REPORT,RPTQCPL                                                   
         BNO   GOREPIO                                                          
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONTYPE,C'X'       X AND N ONLY INCLUDED IF IN TABLE            
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   RDR44                                                            
         BAS   R5,CHKICTYP                                                      
         BE    RDR44                                                            
         DROP  R6                                                               
*                                                                               
RDR42    CLI   BYTE,4              4=LOSSES                                     
         BNE   GOREPIO                                                          
         TM    REPORT,RPTQLOS                                                   
         BNO   GOREPIO                                                          
         BAS   R5,CHKXCTYP                                                      
         BE    GOREPIO             EXCLUDE IF IN TABLE                          
                                                                                
* - NEED SPL ELEMENT TO CHECK IF SPL DATE IS WITHIN THIS WEEK                   
* - THIS WEEK = FROM RUN DATE TO PREVIOUS MONDAY                                
RDR44    L     R6,AIO                                                           
         USING RCONSPEL,R6                                                      
         MVI   ELCODE,X'06'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
                                                                                
         MVC   WORK(3),BTODAY                                                   
         MVC   WORK+3(3),MONDAYDT                                               
         CLI   ACTSTR,0           OVERRIDE DEFAULT ACTV DATES ?                 
         BE    RDR45                                                            
         MVC   WORK(3),ACTEND     YES                                           
         MVC   WORK+3(3),ACTSTR                                                 
                                                                                
RDR45    CLC   RCONSPDT,WORK       SPL DATE MUST BE WITHIN THESE DATES          
         BH    GOREPIO             HIGH/SKIP                                    
         CLC   RCONSPDT,WORK+3                                                  
         BL    GOREPIO             LOW/SKIP                                     
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVC   PNDKID,BYTE         COMPLETED = X'03'/ LOSSES=X'04'              
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
*        CLI   KNUMFLG,C'Y'        IS K # IN SRTKEY ALREADY?                    
*        BE    *+10                YES, DON'T PUT IN AGAIN                      
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
*                                                                               
* - NEED SPL COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         USING RCONSMEL,R6                                                      
         MVI   ELCODE,X'07'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR50                                                            
         MVC   PNDCOMMT(3),=C'SPL'             SPL COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY           DISK ADDRESS                     
         DROP  R5                                                               
         B     RDR50                                                            
                                                                                
RDR50    EQU   *                                                                
* GET CONTRACT $ INTO FULL                                                      
* IF IT'S LOSS, $ FROM X'08' ELEM - IF IT'S COMPLETE, $ FROM BUCKETS            
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         CLI   PNDKID,4            IS IT LOSS?                                  
         BNE   RDR51                                                            
         MVI   ELCODE,8            YES/GET X'08' ELEM FOR BUDGET $              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONACEL,R6                                                      
         MVC   FULL,RCONAC$$       TOTAL BUDGET $                               
         B     RDR54B                                                           
*                                                                               
         USING RCONREC,R6                                                       
RDR51    LA    R4,RCONELEM         COMPLETED- GET $ FROM BUCKETS                
         XC    FULL,FULL                                                        
RDR52    SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),3                                                          
         BE    RDR54                                                            
         CLI   0(R4),0                                                          
         BE    RDR54B                                                           
         B     RDR52                                                            
*                                                                               
         USING RCONBKEL,R4                                                      
RDR54    SR    R1,R1                                                            
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     RDR52                                                            
*                                                                               
RDR54B   EQU   *                                                                
* - FULL HAS $ AMOUNT                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        GET SPL STATION DATA                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSPEL,R6                                                      
         ZIC   R3,RCONSPNU         NUMBER OF STATIONS                           
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         C     R3,=F'11'                                                        
         BNH   *+8                                                              
         LA    R3,11               11 MAX NUMBER OF STATIONS                    
         LA    R5,11                                                            
         LA    R2,RCONSPST         POINT R2 TO STATION/AMOUNT FIELD             
         LA    R6,COMSCALL         POINT R6 TO SORT REC STATION                 
         MVI   BYTE,0                                                           
         CLI   PNDKID,4            IF IT'S A LOSS                               
         BE    *+8                 TOTAL $ AMT SET FROM 08 ELEM                 
         BAS   RE,CALCAMT          ELSE NEED TO CALCULATE TOTAL $ AMT           
         BAS   RE,COMPSTAT         SET STATION+COMPETITIVES TO SRTREC           
                                                                                
RDR55    DS    0H                                                               
         CLC   0(5,R6),0(R2)      MATCH STATION CALL LETTERS ?                  
         BE    RDR60                                                            
         LA    R6,COMLENE(R6)      BUMP SORTREC                                 
         BCT   R5,RDR55                                                         
***      DC    H'0'                SHOULD ALWAYS BE A MATCH !                   
         MVC   P+2(42),=C'*** COMPETITIVE STATION ERROR - CONTRACT #'           
         MVC   P+44(8),PNDCON                                                   
         BAS   RE,SPLAT                                                         
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         OI    RIPSTAT,RIPRDHI                                                  
         B     GOREPIO                                                          
         DROP  R5                                                               
                                                                                
RDR60    DS    0H                                                               
         ICM   R4,15,5(R2)         PERCENT SHARE TO R4                          
         LTR   R4,R4               ..IF 0%                                      
         BZ    RDR65               ..SKIP $ CALC                                
         MVC   8(4,R6),5(R2)       SET SHARE TO SORT REC                        
                                                                                
         L     R1,FULL             FULL HAS TOTAL $ AMOUNT                      
         CVD   R1,DUB                                                           
         ZAP   WORK(16),DUB         PACKED $ INTO WORK                          
                                                                                
         CVD   R4,DUB               PACKED % INTO DUB                           
                                                                                
         MP    WORK(16),DUB+4(4)    % ONLY USES LAST 4 OF DUB                   
         AP    WORK(16),=P'5000'      ROUND UP                                  
         DP    WORK(16),=P'10000'                                               
         ZAP   12(L'COMSAMT,R6),WORK(13)                                        
                                                                                
RDR65    LA    R2,9(R2)            BUMP R2 TO NEXT STATION & % SHARE            
         LA    R6,COMSCALL         RESET TO START OF SORT AREA                  
         LA    R5,11               RESET R3 TO MAX NO OF STATIONS               
         BCT   R3,RDR55                                                         
         B     RDR70                                                            
                                                                                
RDR70    EQU   *                                                                
         B     RDR100              PASS REC TO TSAR                             
         EJECT                                                                  
**********************************************************************          
* INCOMPLETE                                                                    
**********************************************************************          
RDR80    DS    0H                                                               
         TM    REPORT,RPTQINC                                                   
         BNO   GOREPIO                                                          
                                                                                
         BAS   R5,CHKXCTYP         EXCLUDE IF IN TABLE                          
         BE    GOREPIO                                                          
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVI   PNDKID,X'02'        INCOMPLETE                                   
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
*        CLI   KNUMFLG,C'Y'        IS K # IN SRTKEY ALREADY?                    
*        BE    *+10                YES, DON'T PUT IN AGAIN                      
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
*                                                                               
* - NEED SPL COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR81                                                            
         MVC   PNDCOMMT(3),=C'SPL'             SPL COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY           DISK ADDRESS                     
         DROP  R5                                                               
                                                                                
* GET CONTRACT $ INTO FULL                                                      
RDR81    L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         LA    R4,RCONELEM                                                      
         XC    FULL,FULL                                                        
RDR82    SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),3                                                          
         BE    RDR84                                                            
         CLI   0(R4),0                                                          
         BE    RDR86                                                            
         B     RDR82                                                            
*                                                                               
         USING RCONBKEL,R4                                                      
RDR84    SR    R1,R1                                                            
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     RDR82                                                            
         DROP  R4                                                               
*                                                                               
RDR86    EQU   *                                                                
* - FULL HAS $ AMOUNT - GET RID OF PENNIES                                      
         L     R1,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'100'          GO PENNIES                                   
         ST    R1,FULL                                                          
*                                                                               
         LA    R2,COMSCALL         SORT REC STATION CALL LETTERS                
         BAS   RE,GETSTATS         GET COMPETITIVE STATIONS/AFFIL               
         MVI   ELCODE,0            CLEAR ELCODE                                 
                                                                                
RDR87    MVC   0(5,R2),WORK       STATION CALL LETTERS                          
         MVC   5(3,R2),WORK+5     AFFILIATION                                   
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   12(L'COMSAMT,R2),DUB                                             
         XC    FULL,FULL           ALL $ GO TO 1ST STATION                      
         LA    R2,COMLENE(R2)      BUMP SORTREC                                 
         CLI   ELCODE,2            ARE WE IN ELEMENT ALREADY                    
         BE    RDR88               YES                                          
*                                                                               
         L     R6,AIO2             NO-POINT TO STATION REC                      
         MVI   ELCODE,X'02'                                                     
         USING RSTAMKEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   RDR90                                                            
         B     *+8                                                              
RDR88    BAS   RE,NEXTEL                                                        
         BNE   RDR90                                                            
         MVC   WORK(5),RSTAMKST                                                 
         MVC   WORK+5(3),RSTAMKAF                                               
         B     RDR87                                                            
         DROP  R6                                                               
*                                                                               
RDR90    EQU   *                                                                
         B     RDR100              PASS REC TO TSAR                             
         EJECT                                                                  
**********************************************************************          
* ADD RECORD TO TSAROFF                                                         
**********************************************************************          
RDR100   DS    0H                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         LA    RE,MYTSREC                                                       
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         LA    RF,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         LA    RE,SRTREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
         MVI   TSOFFACT,TSAADD                                                  
         LA    RE,MYTSREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
         TM    TSERRS,X'20'        DUPLICATE KEY ON ADD?                        
         BO    *+14                YES, ALLOW IT                                
*                                                                               
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MYCOUNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,MYCOUNT                                                       
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         OI    RIPSTAT,RIPRDHI                                                  
         B     GOREPIO             GET NEXT RECORD                              
*                                                                               
RDRX     B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
*********************************************************************           
* - GET RECS FROM TSAROFF                                                       
*********************************************************************           
PRRECS   NTR1                                                                   
*                                                                               
         XC    DUB,DUB                                                          
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         OC    XP,XSPACES                                                       
         DROP  R5                                                               
*                                                                               
         BAS   RE,FNDBKCL          FIND LAST COL FOR BREAKS $ TOTS              
*                                  ALSO INITIALIZE TABEXPER                     
*                                                                               
         XC    PREVIOUS,PREVIOUS                                                
         XC    PREVSPER,PREVSPER                                                
*                                                                               
         LA    R5,TSAREA                                                        
         USING TSARD,R5                                                         
         LA    RE,MYTSREC          CLEAR I/O AREA                               
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         MVI   TSOFFACT,TSAGET     READ BY NUMBER                               
         LA    R0,1                                                             
PRT20    STH   R0,TSRNUM                                                        
         LA    R0,MYTSREC                                                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF,(R5)                                                    
         TM    TSERRS,TSEEOF                  END OF FILE?                      
         BNO   PRT25                          NO - CONTINUE                     
         OC    PREVIOUS,PREVIOUS              ANY DATA PASSED?                  
         BZ    PRTX                           NO  - EXIT                        
*                                                                               
*         DO LAST PROCESSING/PRINTING, THEN EXIT                                
         MVC   LASTSTAT,PREVIOUS   SAVE LAST STATUS                             
         MVI   PREVIOUS,0          FORCE BREAK ON STATUS                        
         XC    DUB,DUB             SET BREAK TO ZERO FOR PRTOTS                 
         BAS   RE,PRTOTS            PRINT TOTALS, BRK POSSIBLE                  
         B     PRTX                                                             
*                                                                               
PRT25    LA    RF,SRTREC                MOVE RECORD TO SRTREC AREA              
         LA    RE,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
* LOOP TO CHECK FOR BREAKS IN KEY TO PAGE BREAK/(SUB)TOTAL                      
*                                                                               
         OC    PREVIOUS,PREVIOUS   FIRST RECORD?                                
         BZ    PRT36               YES, JUST PRINT                              
*                                                                               
         CLC   PNDKID(1),PREVIOUS     DIFFERENT STATUS?                         
         BE    PRT29               NO, CHECK OTHER KEY FIELDS                   
         XC    DUB,DUB             SET BREAK TO ZERO FOR PRTOTS                 
         B     PRT31                                                            
*                                                                               
PRT29    LA    R3,PREVIOUS+1       POINT TO SRT FIELDS OF PREV KEY              
         LA    R4,SRTCHNK          POINT TO SRT FIELDS OF CURR KEY              
         ZIC   R2,COLBKFLG         NUMB OF POS BREAK FIELDS                     
*                                                                               
PRT30    DS    0H                                                               
         CLC   0(8,R3),0(R4)       BREAK ON KEY FIELD?                          
         BE    PRT32               NO, KEYS SAME, CHECK NEXT                    
*                                                                               
         LR    R1,R3               COPY FOR MATH                                
         LA    RF,PREVIOUS+1                                                    
         SR    R1,RF               FIND DISPLACEMENT TO KEY FIELD               
         SR    R0,R0               CLEAR FOR DIVIDE                             
         D     R0,=F'8'            YIELDS # OF KEY FIELD (N-1)                  
*                                                                               
         OC    STATCOL,STATCOL     NO STATUS SUBTOTAL?                          
         BZ    PRT30A              NONE, SKIP                                   
         ZIC   RF,STATCOL                                                       
         CR    R1,RF               ARE WE ON/AFTER STATUS COLUMN?               
         BL    *+6                 YES, SKIP                                    
         BCTR  R1,0                DECREMENT COL BRK FLD BY ONE                 
PRT30A   ST    R1,DUB                                                           
*                                                                               
PRT31    MVI   PGBKFLG,C'N'        CLEAR PAGE BREAK FLAG                        
         MVC   LASTSTAT,PREVIOUS   SAVE LAST STATUS                             
         BAS   RE,PRTOTS                                                        
         CLI   PGBKFLG,C'Y'        DO WE PAGE BREAK?                            
         BNE   *+8                 NO, DON'T SKIP PAGE                          
         MVI   FORCEHED,C'Y'       PAGE BREAK                                   
         CLI   FORCEHED,C'Y'       SKIP PAGE OR JUST TOTAL?                     
         BE    *+8                 YES, DON'T PRINT BLANK LINE                  
         BAS   RE,SPLAT            JUST TOTAL, SO PRINT BLANK LINE              
         B     PRT36                                                            
*                                                                               
PRT32    LA    R3,8(R3)            CHECK NEXT KEY FIELD                         
         LA    R4,8(R4)            CHECK NEXT KEY FIELD                         
         BCT   R2,PRT30                                                         
*                                                                               
PRT36    MVC   PREVIOUS,SRTREC                                                  
         MVC   PREVSPER,PNDSLSNM   SAVE SALESPERSON NAME                        
         BAS   RE,PRCON            PRINT CONTRACT ACCORD TO TABLE               
*                                                                               
PRT350   DS    0H                                                               
         LH    R0,TSRNUM           BUMP TSAR ID NUMBER                          
         A     R0,=F'1'                                                         
         STH   R0,TSRNUM                                                        
         B     PRT20               GET NEXT TSAR REC                            
*                                                                               
PRTX     B     EXIT                                                             
*                                                                               
         DROP R5                                                                
         EJECT                                                                  
*                                                                               
*** END OF MAIN ROUTINES *********************************************          
*                                                                               
*                                                                               
*** SUBROUTINES CALLED IN READRECS  ***********************************         
*                                                                               
**********************************************************************          
*  FILL IN FIELDS COMMON TO ALL REPORT TYPES                                    
**********************************************************************          
FILLCMN  NTR1                                                                   
         USING RCONREC,R6                                                       
         L     R6,AIO                                                           
                                                                                
         UNPK  PNDCON(9),RCONKCON(5)  CONTRACT NUMBER (PWO)                     
         MVI   PNDCON+8,0             CLEAR SIGN BYTE                           
* GET RID OF LEADING ZEROS                                                      
         XC    WORK,WORK                                                        
         LA    R1,8                                                             
         LA    RE,PNDCON                                                        
FIL02    CLI   0(RE),C'0'                                                       
         BNE   FIL03                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,FIL02                                                         
         DC    H'0'                CONTRACT # CAN'T BE ALL ZEROS                
FIL03    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         MVC   PNDCON,WORK                                                      
*                                                                               
         BAS   RE,GETCOMM          PUT COMMENT IN SORTRECORD                    
*                                                                               
*** BUILD SORTKEY FROM TABLE ***************************************            
*                                                                               
         LA    R3,SCRTAB                                                        
*                                                                               
FIL10    DS    0H                                                               
         CLI   20(R3),0            TABLE ENTRY EMPTY?                           
         BE    FIL20               YES, SKIP TO NEXT TABLE ENTRY                
         LA    R4,SRTCHNK                                                       
         L     R6,AIO              POINT TO BEGINNING OF RECORD                 
         ZIC   RF,20(R3)           GET COLUMN #                                 
         BCTR  RF,0                                                             
         MH    RF,=H'8'                                                         
         AR    R4,RF               BUMP TO CORRECT POSITION IN SRTKEY           
*                                                                               
** CHECK FOR EXCEPTIONS                                                         
*                                                                               
         XC    WORK,WORK                                                        
         BAS   RE,CHKEXC           CHECK IF IT'S AN EXCEPTION                   
         OC    0(8,R4),0(R4)       SRTKEY FIELD FILLED IF EXCEPTION             
         BNZ   FIL20               EXCEPTION, GOTO NEXT ENTRY                   
         CLI   EXNFLG,C'Y'         EXCEPTION, WORK NOT FILLED?                  
         BE    FIL20               YES, SKIP                                    
         OC    WORK,WORK           WORK IS FILLED IF AN EXCEPTION               
         BZ    FIL14                                                            
*        BZ    *+12                NOT EXCEPTION, PROCESS NORMALLY              
         LA    R6,WORK             POINT TO WORK FOR EX MOVE                    
*                                                                               
*        CLC   =C'10236',PNDCON                                                 
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         B     FIL17                                                            
FIL14    DS    0H                                                               
*                                                                               
** MOVE POINTER TO BEGINNING OF KEY OR DATA                                     
*                                                                               
*** NEW CODE TO CHECK FOR '1E' ELEM                                             
         CLI   21(R3),X'20'        IS IT FLIGHT START?                          
         BE    *+12                YES, GET '1E' ELEM                           
         CLI   21(R3),X'22'        IS IT FLIGHT END?                            
         BNE   FIL14Z              NO, SKIP                                     
*                                                                               
         MVI   ELCODE,X'1E'        LOOK FOR NEW ELEM                            
         BAS   RE,GETEL                                                         
         BNE   FIL14A              NO '1E' ELEM, HANDLE NORMALLY                
*                                                                               
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   IS RCONRFLT NULLS?                           
         BZ    FIL14A              NULLS, HANDLE NORMALLY                       
         MVI   23(R3),RCONRFLT-RCONRFEL     STORE DISPL. TO RCONRFLT            
         CLI   21(R3),X'20'        IS IT FLIGHT START?                          
         BE    *+8                 YES, DONE                                    
         MVI   23(R3),RCONRFLT+3-RCONRFEL   NO, GET FLIGHT END                  
         MVI   22(R3),X'1E'                                                     
*** END NEW '1E' CODE                                                           
*                                                                               
FIL14A   DS    0H                                                               
         USING RCONREC,R6                                                       
         L     R6,AIO                                                           
***                                                                             
FIL14Z   CLI   22(R3),0            IS ENTRY IN THE KEY OF K?                    
         BE    FIL15               YES, GET DISPLACEMENT                        
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   FIL20               ELEM MISSING, SKIP TO NEXT ENTRY             
*                                                                               
FIL15    ZIC   R1,23(R3)           GET DISPLACEMENT INTO KEY/ELEM               
         AR    R6,R1                                                            
*                                                                               
*        CLC   =C'10236',PNDCON                                                 
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
* EX MOVE                                                                       
FIL17    ZIC   R1,18(R3)           SIZE OF MOVE = REQUESTED LENGTH              
         BAS   RE,CHKEXC2          CHECK IF IT'S AN EXCEPTION                   
         CLI   EXDFLG,C'Y'         IS IT A DATE?                                
         BNE   *+8                 NO, SKIP                                     
         LA    R1,3                                                             
         CH    R1,=H'8'            IS IT BIGGER THAN 8?                         
         BNH   *+8                 NO, LEAVE LENGTH ALONE                       
         LA    R1,8                TOO BIG, TRUNCATE FIELD TO 8                 
         BCTR  R1,0                DECREMENT FOR EX MOVE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)                                                    
*                                                                               
*        CLC   =C'10236',PNDCON                                                 
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
FIL20    DS    0H                                                               
*** NEW                                                                         
         CLI   21(R3),X'20'        IS IT FLIGHT START?                          
         BNE   *+12                NO                                           
         MVI   23(R3),RCONDATE-RCONELEM                                         
         MVI   22(R3),1            RESTORE ELCODE                               
         CLI   21(R3),X'22'        IS IT FLIGHT END?                            
         BNE   *+12                NO, SKIP                                     
         MVI   23(R3),RCONDATE+3-RCONELEM                                       
         MVI   22(R3),1            RESTORE ELCODE                               
*** END NEW                                                                     
*                                                                               
         LA    R3,TABENTQ(R3)      BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R3),X'FF'                                                      
         BNE   FIL10               NOT DONE, CONTINUE                           
*                                                                               
FILX     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXCLUDE ANY CONTYPES THAT MATCH                                               
***********************************************************************         
CHKXCTYP DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONTYPE,X'40'      IS THERE A CONTRACT TYPE ?                   
         BNH   XCT20               NO/ SO NO MATCH                              
         CLI   RCONTYPE,C'X'        ALWAYS EXCLUDE X AND N                      
         BER   R5                                                               
         CLI   RCONTYPE,C'N'                                                    
         BER   R5                                                               
CHKICTYP L     R6,AIO            COMPLETED ENTER HERE INCLUDE MATCHES           
         LA    R1,CONTYPEX                                                      
         LA    RE,50                                                            
XCT10    CLC   RCONTYPE,0(R1)                                                   
         BER   R5                                                               
         LA    R1,1(R1)                                                         
         BCT   RE,XCT10                                                         
XCT20    LTR   R6,R6                                                            
         BR    R5                                                               
         DROP  R6                                                               
***********************************************************************         
* - CALCULATE TOTAL $ AMOUNT FROM TOTAL $ BUCKETS AND ANY ONE STA %             
* FULL HAS TOTAL BUCKET $                                                       
* 5(R2) POINTS TO 1ST STATION %                                                 
* (TOTAL BUCKET $) / (1ST STATION %) = TOTAL $ AMT                              
* RETURNS TOTAL $ AMT IN FULL                                                   
* R3 HAS MAX NUMBER OF STATIONS                                                 
***********************************************************************         
CALCAMT  NTR1                                                                   
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   WORK(12),DUB        $                                            
*        MP    WORK(12),=P'100'                                                 
         MP    WORK(12),=P'1000'                                                
                                                                                
CLC10    ICM   R1,15,5(R2)         %                                            
         LTR   R1,R1                                                            
         BNZ   CLC12                                                            
         LA    R2,9(R2)            BUMP TO NXT STA /  % MINI ELELENT            
         BCT   R3,CLC10                                                         
         B     CLC14               NO % - SPL ELEM BUT NO %                     
CLC12    CVD   R1,DUB                                                           
         DP    WORK(12),DUB+4(4)                                                
         SRP   WORK(8),64-1,0      ROUND UP                                     
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
CLC14    ST    R1,FULL                                                          
CLCX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* READ STATION RECORD AND RETURNS STATION/AFFILIATION IN WORK                   
**********************************************************************          
GETSTATS NTR1                                                                   
         L     R6,AIO              NO/ GET CONTRACT STATION REC                 
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA   STATION CALL LETTERS                         
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(5),RSTAKSTA          STATION                                
         MVC   WORK+5(3),RSTAAFFL          STATION AFFILIATION                  
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
* READ STATION REC TO GET COMPETITIVE STATIONS/AFFILIATION                      
* SET TO SORTREC                                                                
* R6 POINTS TO SORTREC AREA                                                     
*********************************************************************           
COMPSTAT NTR1                                                                   
         LR    R3,R6               SET R3 TO SORTREC AREA                       
         L     R6,AIO              GET STATION RECORD                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA   STATION CALL LETTERS                         
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   0(5,R3),RCONKSTA          STATION                                
         MVC   5(3,R3),RSTAAFFL          STATION AFFILIATION                    
                                                                                
* NOW GET COMPETITIVES                                                          
         LA    R2,10               MAX NUMBER OF COMPETITIVES                   
         L     R6,AIO2             POINT TO STATION REC                         
         MVI   ELCODE,X'02'        COMPETITIVE STATION ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   COMPX                                                            
COMP10   LA    R3,COMLENE(R3)            BUMP SORTREC                           
         USING RSTAMKEL,R6                                                      
         MVC   0(5,R3),RSTAMKST    COMPETITIVE STATION                          
         MVC   5(3,R3),RSTAMKAF    AFFILIATION                                  
         BAS   RE,NEXTEL                                                        
         BNE   COMPX                                                            
         BCT   R2,COMP10                                                        
*                                                                               
COMPX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*** SUBROUTINES CALLED FROM PRRECS - TO PRINT THE K'S **********                
*                                                                               
****************************************************************                
*   SUBROUTINE TO PRINT CONTRACT                                                
****************************************************************                
PRCON    NTR1                                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
*                                                                               
         LA    R5,1                COLUMN # SEARCHING FOR                       
         LA    R6,SRTCHNK          POINT TO DATA                                
         LA    R2,XP              POINT TO PRINT LINE                           
         LA    R3,SCRTAB                                                        
*                                                                               
PC10     CLI   0(R3),X'FF'                                                      
         BNE   PC15                NOT END, GO ON                               
*                                                                               
         ZIC   R1,COMCOL           END OF TABLE, IS IT A COMMENT?               
         CR    R5,R1               MATCHING COL #'S                             
         BNE   PCX                 NO, DONE                                     
         ZIC   R1,COMLEN           GET REQUESTED LENGTH                         
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDCOM1     PRINT COMMENT BY COLUMN                      
         B     PC30                                                             
*                                                                               
PC15     ZIC   R1,20(R3)           GET COLUMN #                                 
         CR    R5,R1               MATCHING COLUMN NUMBERS?                     
         BE    PC20                YES, PRINT FIELD                             
         LA    R3,TABENTQ(R3)      NO, CHECK NEXT ENTRY                         
         B     PC10                                                             
*                                                                               
PC20     DS    0H                                                               
         ZIC   R1,18(R3)           GET REQUESTED LENGTH                         
         BCTR  R1,0                DECREM FOR EX MOVE                           
         BAS   RE,CHKEXP           CHECK EXCEPTIONS FOR PRINTING                
         CLI   EXPFLG,C'Y'         HAVE WE PRINTED YET?                         
         BE    PC30                YES, DON'T PRINT KEY                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)       PRINT PIECE OF SRTKEY                        
*                                                                               
* INCREMENT PRINT LINE POSITION                                                 
*                                                                               
PC30     DS    0H                                                               
         BAS   RE,FILPRAD          SAVE A(PRINTED KEY BREAK FLDS)               
         LA    R5,1(R5)            LOOK FOR NEXT COLUMN                         
         LA    R6,8(R6)            POINT TO NEXT KEY FIELD                      
         LA    R2,2(R1,R2)         INCREMENT FOR EX AND FOR SPACE               
         LA    R3,SCRTAB                                                        
         B     PC10                                                             
*                                                                               
PCX      DS    0H                                                               
*                                                                               
*        MVC   0(8,R2),PNDCON      PRINT K #                                    
         BAS   RE,SPLAT            PRINT LINE                                   
         CLI   ROWCOL,C'R'         ARE COMMENTS BY ROWS?                        
         BNE   PCX1                NO, DON'T PRINT COMMENTS BY ROWS             
         BAS   RE,PRROWCOM         PRINT COMMENTS BY ROWS                       
*                                                                               
PCX1     DS    0H                                                               
         CLI   COMPOPT,C'P'        COMPETITIVE 'P' OPTION?                      
         BE    *+12                                                             
         CLI   COMPOPT,C'D'        COMPETITIVE 'D' OPTION?                      
         BNE   *+8                                                              
         BAS   RE,PRCOMP           PRINT COMPETITIVE SHARE LINES                
*                                                                               
         CLI   KSPACING,C'Y'       SKIP LINE BETWEEN CONTRACTS?                 
         BNE   PCXX                                                             
         BAS   RE,SPLAT            PRINT BLANK LINE                             
PCXX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*  CHECK FOR EXCEPTIONS FOR PRINTING                                            
**********************************************************************          
CHKEXP   NTR1                                                                   
         MVI   EXPFLG,C'N'                                                      
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
*                                                                               
         CLI   21(R3),X'14'        SALESPERSON?                                 
         BNE   CEP10                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDSLSNM                                                 
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP10    CLI   21(R3),X'18'        AGENCY?                                      
         BNE   CEP20                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDDAGY                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP20    CLI   21(R3),X'1A'        ADVERTISER?                                  
         BNE   CEP30                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDADVN                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP30    CLI   21(R3),X'1C'        PRODUCT?                                     
         BNE   CEP40                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDPROD                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP40    CLI   21(R3),X'1E'        BUYER?                                       
         BNE   CEP50                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PNDBUYER                                                 
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP50    DS    0H                                                               
*                                                                               
CEP60    DS    0H                                                               
         LA    RF,TABDATE          LOOK AT TABLE OF DATES                       
*                                                                               
CEP65    CLI   0(RF),X'FF'         AT END OF TABLE?                             
         BE    CEP80               YES, DONE                                    
         CLC   0(1,RF),21(R3)                                                   
         BE    CEP70               YES, PRINT                                   
         LA    RF,1(RF)            BUMP TO NEXT DATE TYPE                       
         B     CEP65                                                            
*                                                                               
CEP70    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,0(R6)),(5,0(R2))                                  
         MVI   EXPFLG,C'Y'         EXCEPTION PRINT FLAG                         
         B     CEPX                                                             
*                                                                               
CEP80    DS    0H                                                               
         CLI   21(R3),X'13'        STATUS?                                      
         BNE   CEP90                                                            
*                                                                               
         CLI   PNDKID,X'01'        IS IT PENDING?                               
         BNE   CEP82                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TXTPEND                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP82    DS    0H                                                               
         CLI   PNDKID,X'02'        IS IT INCOMPLETE?                            
         BNE   CEP84                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TXTINCM                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP84    DS    0H                                                               
         CLI   PNDKID,X'03'        IS IT COMPLETE?                              
         BNE   CEP86                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TXTCOMP                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP86    DS    0H                                                               
         CLI   PNDKID,X'04'        IS IT A LOSS?                                
         BNE   CEP90               SKIP IF NOT ONE OF THE FOUR                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TXTLOSS                                                  
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP90    DS    0H                                                               
         CLI   21(R3),X'15'        SHARE GOAL?                                  
         BNE   CEP100                                                           
         EDIT  (1,0(R6)),(3,0(R2)),ZERO=NOBLANK                                 
         MVI   3(R2),C'%'                                                       
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP100   DS    0H                                                               
         CLI   21(R3),X'24'        FLIGHT WEEKS?                                
         BNE   CEP110                                                           
         EDIT  (1,0(R6)),(2,0(R2)),ZERO=NOBLANK                                 
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP110   DS    0H                                                               
         CLI   21(R3),X'25'        PRIMARY DEMO?                                
         BNE   CEP120                                                           
         MVC   WORK(3),0(R6)       GET DEMO INTO KEY                            
         BAS   RE,GETDEMNM         PUTS DEMO NAME IN WORK                       
         ZIC   R1,18(R3)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP120   DS    0H                                                               
         CLI   21(R3),X'30'        READY TO BOOK?                               
         BNE   CEP130                                                           
         MVC   0(3,R2),0(R6)                                                    
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP130   DS    0H                                                               
         CLI   21(R3),X'31'        DAYS TO FLIGHT?                              
         BNE   CEP140                                                           
         EDIT  (2,0(R6)),(4,0(R2)),ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK              
         MVI   EXPFLG,C'Y'                                                      
         B     CEPX                                                             
*                                                                               
CEP140   DS    0H                                                               
         LA    R5,TABEXPER         LOOK AT TABLE OF PERIODS AND BUDG'S          
*                                                                               
CEP145   CLI   0(R5),X'FF'         AT END OF TABLE?                             
         BE    CEP150              YES, DONE                                    
         CLC   0(1,R5),21(R3)      PERIOD OR BUDGET FOUND?                      
         BE    CEP149              YES, PRINT                                   
         LA    R5,PERENTQ(R5)      BUMP TO NEXT PERIOD                          
         B     CEP145                                                           
*                                                                               
CEP149   DS    0H                                                               
         CLI   0(R5),X'19'         STATION BUD?                                 
         BE    CEP1499             YES, PRINT                                   
         CLI   0(R5),X'17'         MARKET BUD?                                  
         BE    CEP1499             YES, PRINT                                   
*                                                                               
         CLI   4(R5),0             PERIOD INPUT FOR THAT PERIOD?                
         BE    CEP149B             NO, DON'T PRINT                              
*                                                                               
CEP1499  ZIC   R4,1(R5)            GET DISPLACEMENT INTO TABLE                  
         BAS   RE,ADDTOT           ACCUMULATE TOTALS                            
*        EDIT  (4,0(R6)),(8,0(R2)),ZERO=NOBLANK                                 
*                                                                               
         L     R1,0(R6)            GET $ VALUE                                  
         CLI   RDOLLAR,C'Y'        ROUND DOLLARS?                               
         BNE   CEP149A             NO,CONTINUE                                  
         OC    0(8,R6),0(R6)       IS VALUE ZERO?                               
         BZ    CEP149A             YES, DON'T ROUND                             
         SR    R0,R0               ZERO OUT REMAINDER                           
         L     R4,=F'500'                                                       
         AR    R1,R4               DO FOR HALF-ROUNDING                         
         SLA   R4,1                NEED 1000                                    
         DR    R0,R4               DIVIDE BY 1000                               
*                                                                               
CEP149A  EDIT  (R1),(10,WORK2),ZERO=NOBLANK                                     
         LA    R1,10               MAXIMUM POSSIBLE OUTPUT LENGTH               
         ZIC   RF,18(R3)           REQUESTED LENGTH                             
         SR    R1,RF               GET DISPLACEMENT INTO WORK2                  
         LA    RF,WORK2                                                         
         AR    RF,R1               POINT TO START OF DATA IN WORK2              
*                                                                               
         ZIC   R1,18(R3)          GET REQ LENGTH AGAIN                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
* GET PRINT LINE DISPLACEMENT                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R0,XP               POINT TO BEG OF PRINT LINE                   
         LR    R1,R2               GET CURRENT PRINT LINE POS                   
         SR    R1,R0               FIND DISPL INTO PRINT LINE                   
         STC   R1,2(R5)            STORE PRINT LINE DISPL, ALWAYS 1 BYT         
         DROP  R4                                                               
*                                                                               
         MVC   3(1,R5),18(R3)      MOVE # PRINT CHAR'S INTO TABLE               
*                                                                               
CEP149B  MVI   EXPFLG,C'Y'         EXCEPTION PRINT FLAG                         
         B     CEPX                                                             
*                                                                               
CEP150   DS    0H                                                               
CEPX     B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* - PRINT ROUTINE                                                               
*********************************************************************           
SPLAT    NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   XP,XSPACES                                                       
         DROP  R5                                                               
EXIT     XIT1                                                                   
*                                                                               
*********************************************************************           
*  THE INTERNAL TABLE                                                           
*********************************************************************           
* DISP   ENTRY #  DESCRIPTION                                                   
*********************************************************************           
*                                                                               
*  0      1ST     16 BYTE LITERAL - ONSCREEN NAME FOR THE FIELD                 
*  16     2ND     ONE BYTE CHAR FOR ACTUAL LENGTH, FIXED LEN ('F') OR A         
*                 BLANK                                                         
*  17     3RD     ONE BYTE HEX ACTUAL LEN - TWO BYTE MAX OUTPUT                 
*  18     4TH     ONE BYTE HEX REQUESTED LEN - TWO BYTE MAX OUTPUT              
*  19     5TH     ONE BYTE CHAR FOR COLUMN #, PAGE BREAK ('P') OR ' '           
*  20     6TH     ONE BYTE HEX COLUMN # - TWO BYTE MAX OUTPUT                   
*  21     7TH     ONE BYTE ELEMENT CODE FOR THIS FIELD IN REPORT RECORD         
*  22     8TH     ONE BYTE ELEMENT CODE FOR THIS FILED IN CONRACT REC -         
*                 - X'00' IF IN THE KEY, X'FE' IF NOT IN CONTRACT REC           
*                 OR AN EXCEPTION                                               
*  23     9TH     DISPLACEMENT (IN CONTRACT REC) OF THIS FIELD FROM             
*                 KEY OR START OF ELEM -- 1 BYTE FIELD                          
*                                                                               
*  24 BYTES PER TABLE ENTRY --- 'TABENTQ'                                       
*                                                                               
**********************************************************************          
SCRTAB   DS    0H                                                               
OFFTAB   DS    0H                                                               
         DC    CL16'Office'                                                     
         DC    C'F'                                                             
         DC    X'02'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'10'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKOFF-RCONKEY)                                            
*                                                                               
TABENTQ  EQU   *-SCRTAB            EQUATED LENGTH FOR A TABLE ENTRY             
*                                                                               
STATAB   DS    0H                                                               
         DC    CL16'Sta'           station                                      
         DC    C' '                                                             
         DC    X'05'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'12'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKSTA-RCONKEY)                                            
*                                                                               
SALTAB   EQU   *                                                                
         DC    CL16'SalN'          Salesperson                                  
         DC    C' '                                                             
         DC    X'14'               20 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'14'                                                            
         DC    X'FE'               *** WILL GET FROM SALESP REC                 
         DS    XL1                ***                                           
*                                                                               
SALCTAB  EQU   *                                                                
         DC    CL16'Salesperson Code'                                           
         DC    C'F'                                                             
         DC    X'03'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'16'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONSAL-RCONELEM)                                            
*                                                                               
         DC    CL16'Agy'           Agency                                       
         DC    C' '                                                             
         DC    X'14'               20 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'18'                                                            
         DC    X'FE'               NEED TO GET AGY NAME FROM RECORD             
         DS    XL1                                                              
*                                                                               
         DC    CL16'Adv'           advertiser                                   
         DC    C' '                                                             
         DC    X'14'               20 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1A'                                                            
         DC    X'FE'                                                            
         DS    XL1                 NEED TO GET FROM ADV RECORD                  
*                                                                               
         DC    CL16'Prd'           product                                      
         DC    C' '                                                             
         DC    X'14'               20 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1C'                                                            
         DC    X'FE'                                                            
         DS    XL1                                                              
*                                                                               
         DC    CL16'Buyer'                                                      
         DC    C' '                                                             
         DC    X'14'               20 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1E'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONBUYR-RCONELEM)                                           
*                                                                               
         DC    CL16'Flt St'        flight start                                 
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'20'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONDATE-RCONELEM)                                           
*                                                                               
         DC    CL16'Flt Ed'        flight end                                   
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'22'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONDATE+3-RCONELEM)                                         
*                                                                               
         DC    CL16'Flight Weeks'                                               
         DC    C'F'                                                             
         DC    X'02'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'24'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONWKS-RCONELEM)                                            
*                                                                               
         DC    CL16'PDemo'         primary demo                                 
         DC    C'F'                                                             
         DC    X'06'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'25'                                                            
         DC    X'12'               SAR ELEMENT                                  
         DC    AL1(RSARDEM-RSAREL) FIRST ONE                                    
*                                                                               
         DC    CL16'Dpt'           daypart code                                 
         DC    C' '                                                             
         DC    X'06'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'26'                                                            
         DC    X'12'                                                            
         DC    AL1(RSARDPT-RSAREL)                                              
*                                                                               
         DC    CL16'Crt Dt'        create date                                  
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'11'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONCREA-RCONCODE)                                           
*                                                                               
         DC    CL16'Last Update Date'                                           
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'27'                                                            
         DC    X'12'               UNKNOWN                                      
         DC    AL1(RSARXLAD-RSARXEL)                                            
*                                                                               
         DC    CL16'Status'                                                     
         DC    C' '                                                             
         DC    X'0A'               LENGTH 10                                    
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'13'                                                            
         DC    X'1F'                                                            
         DC    AL1(RCONSTAT-RCONXEL)                                            
*                                                                               
         DC    CL16'ShrG'          Share Goal                                   
         DC    C'F'                                                             
         DC    X'04'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'15'                                                            
         DC    X'12'               NEED MKT BUD AND SHARE GOAL                  
         DC    AL1(RSARXSHG-RSARXEL)                                            
*                                                                               
         DC    CL16'MktBud'        market budget                                
         DC    C' '                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'17'                                                            
         DC    X'12'                                                            
         DC    AL1(RSARXBGT-RSARXEL)                                            
*                                                                               
         DC    CL16'StaBud'        station budget                               
         DC    C' '                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'19'                                                            
         DC    X'12'                                                            
         DS    XL1                                                              
*                                                                               
PER1TAB  DC    CL16'Per1'                                                       
         DC    C' '                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1B'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
*                                                                               
         DC    CL16'Per2'                                                       
         DC    C' '                                                             
         DC    X'08'               30 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1D'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
*                                                                               
         DC    CL16'Per3'                                                       
         DC    C' '                                                             
         DC    X'08'               30 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1F'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
*                                                                               
         DC    CL16'Per4'                                                       
         DC    C' '                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'21'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
*                                                                               
PER5TAB  DC    CL16'Per5'                                                       
         DC    C' '                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'23'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
*                                                                               
         DC    CL16'ConNum'        contract number                              
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'29'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKCON-RCONKEY)                                            
*                                                                               
         DC    CL16'RTB'           READY TO BOOK                                
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'03'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'30'                                                            
         DC    X'FE'                                                            
         DC    AL1(0)                                                           
*                                                                               
         DC    CL16'DTF'           DAYS TO FLIGHT                               
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'04'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'31'                                                            
         DC    X'FE'                                                            
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* STORAGE FOR COMMENT PRINTING FORMAT                                           
*                                                                               
COMMLAB  DC    CL16'Cmts'          comments                                     
ROWCOL   DS    CL1                 ROW/COLUMN DESIGNATION FOR COMMENTS          
COMLEN   DS    XL1                 COMMENT LENGTH                               
COMCOL   DS    XL1                 COMMENT COLUMN #                             
*                                                                               
         EJECT                                                                  
*                                                                               
* TABLE FOR PERIODS AND BUDGETS ($ FIELDS)                                      
*                                                                               
*    - FIRST BYTE  -  ELEM CODE OF PERIOD OR BUDGET IN BACT RECORD              
*    - SECOND BYTE -  DISPLACEMENT INTO 7F BLOCK OF THE DOLTOTS TABLE           
*    - THIRD BYTE  -  DISPLACEMENT INTO THE PRINT LINE                          
*    - FOURTH BYTE -  NUMBER OF BYTES TO PRINT                                  
*    - FIFTH BYTE  -  PERIOD INPUT? FLAG, USED ONLY BY PERIODS                  
*                                                                               
TABEXPER DS    0H                                                               
         DC    X'1B',X'0'          PERIOD 1                                     
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
PERENTQ  EQU   *-TABEXPER          EQUATED LENGTH FOR A TABLE ENTRY             
         DC    X'1D',X'4'          PER2                                         
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
         DC    X'1F',X'8'          PER3                                         
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
         DC    X'21',X'C'          PER4, 12 BYTE DISPLACEMENT                   
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
         DC    X'23',X'10'         PER5, 16 BYTES                               
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
         DC    X'19',X'14'         STA BUD, 20 BYTES                            
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                 NOT USED                                     
*                                                                               
         DC    X'17',X'18'         MKT BUD, 24 BYTES                            
         DS    XL1                                                              
         DS    XL1                                                              
         DS    XL1                 NOT USED                                     
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* TABLE OF ELEM CODES FROM BUS. ACT. REC. - CORRESPOND TO SRTKEY                
* - FLDS THAT ARE 'LEGIT' FIELDS TO TOTAL ON & POSSIBLY PAGE BREAK ON           
*                                                                               
BRKTAB   DS    0H                                                               
         DC    X'10'               OFFICE                                       
         DS    XL1                 DISPL ON PRINT LINE TO FLD                   
BKTBEQ   EQU   *-BRKTAB                                                         
         DC    X'12'               STATION                                      
         DS    XL1                                                              
         DC    X'14'               SALESPERSON                                  
         DS    XL1                                                              
         DC    X'16'               SALESPERSON CODE                             
         DS    XL1                                                              
         DC    X'18'               AGENCY                                       
         DS    XL1                                                              
         DC    X'1A'               ADVERTISER                                   
         DS    XL1                                                              
         DC    X'1C'               PRODUCT                                      
         DS    XL1                                                              
         DC    X'1E'               BUYER                                        
         DS    XL1                                                              
         DC    X'13'               STATUS                                       
         DS    XL1                 NOT USED BY STATUS                           
         DC    X'FF'               END                                          
         EJECT                                                                  
*                                                                               
* TABLES OF EXCEPTIONS - ELEM CODES OF REPORT REC THAT NEED                     
*                        SPECIAL HANDLING                                       
*                                                                               
TABDATE  DS    0H                                                               
         DC    X'27'               LAST UPDATE DATE                             
         DC    X'20'               FLIGHT START DATE                            
         DC    X'22'               FLIGHT END DATE                              
         DC    X'11'               CREATE DATE                                  
         DC    X'FF'                                                            
*                                                                               
TXTPEND  DC    CL10'PENDING'                                                    
TXTINCM  DC    CL10'INCOMPLETE'                                                 
TXTCOMP  DC    CL10'COMPLETE'                                                   
TXTLOSS  DC    CL10'LOSS'                                                       
*                                                                               
         EJECT                                                                  
**********************************************************************          
*  CHECK FOR EXCEPTIONS TO FILL IN SORT RECORD (MUST ACCESS OTHER RECS)         
**********************************************************************          
CHKEXC   NTR1                                                                   
         MVI   EXNFLG,C'N'         CLEAR FLAG                                   
*                                                                               
         CLI   21(R3),X'14'        SALESPERSON?                                 
         BNE   CE10                                                             
         BAS   RE,GETSLSNM         GET NAME FROM SALESPERSON REC                
         MVC   PNDSLSNM,WORK                                                    
         B     CEX                                                              
*                                                                               
CE10     CLI   21(R3),X'18'        AGENCY?                                      
         BNE   CE20                                                             
         BAS   RE,FILLAGY          GET NAME FROM AGY REC                        
         MVC   PNDDAGY,WORK                                                     
         B     CEX                                                              
*                                                                               
CE20     CLI   21(R3),X'1A'        ADVERTISER?                                  
         BNE   CE30                                                             
         BAS   RE,GETADVR          GET NAME FROM ADV REC                        
         MVC   PNDADVN,WORK                                                     
         B     CEX                                                              
*                                                                               
CE30     CLI   21(R3),X'1C'        PRODUCT?                                     
         BNE   CE35                                                             
         BAS   RE,FILLPROD         GET NAME FROM PRD REC                        
         MVC   PNDPROD,WORK                                                     
         B     CEX                                                              
*                                                                               
CE35     CLI   21(R3),X'26'        DAYPART?                                     
         BNE   CE40                                                             
*                                                                               
*        LA    RF,6                MAX OF SIX DAYPARTS                          
         ZIC   RF,18(R3)           USE REQ LEN FOR NUM DAYP'S                   
         LA    R4,WORK                                                          
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CE35X               ELEM MISSING, SKIP TO NEXT ENTRY             
         ZIC   R1,23(R3)           GET DISPLACEMENT INTO KEY/ELEM               
         AR    R6,R1                                                            
*                                                                               
CE36     MVC   0(1,R4),0(R6)                                                    
         LA    R6,3(R6)            GOTO NEXT DAYPART                            
         LA    R4,1(R4)            POINT TO NEXT AVAIL SPACE IN WORK            
         BCT   RF,CE36             GET NEXT DAYPART                             
CE35X    DS    0H                                                               
         B     CEX                                                              
*                                                                               
CE40     DS    0H                                                               
         CLI   21(R3),X'15'        SHARE GOAL?                                  
         BNE   CE50                                                             
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                 ELEM MISSING, SKIP TO END                    
         ZIC   R1,23(R3)           GET DISPLACEMENT INTO KEY/ELEM               
         AR    R6,R1                                                            
         MVC   0(1,R4),0(R6)       PUT SHARE GOAL IN SRTKEY                     
         B     CEX                                                              
*                                                                               
CE50     DS    0H                                                               
         CLI   21(R3),X'24'        FLIGHT WEEKS?                                
         BNE   CE60                                                             
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                 ELEM MISSING, SKIP TO END                    
         ZIC   R1,23(R3)           GET DISPLACEMENT INTO KEY/ELEM               
         AR    R6,R1                                                            
         MVC   0(1,R4),0(R6)       PUT FLIGHT WEEKS IN SRTKEY                   
         B     CEX                                                              
*                                                                               
CE60     DS    0H                                                               
         CLI   21(R3),X'25'        PRIMARY DEMO?                                
         BNE   CE70                                                             
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                                                              
         USING RSAREL,R6                                                        
         MVC   0(3,R4),RSARDEM     GET PRIMARY DEMO                             
         LA    RE,RSARDEM                                                       
         LA    RF,8                                                             
CE63     TM    0(RE),X'40'         IS IT MARKED AS PRIMARY                      
         BO    CE60X               YES                                          
         LA    RE,3(RE)            NO/BUMP TO NEXT                              
         MVC   0(3,R4),0(RE)                                                    
         BCT   RF,CE63                                                          
         MVC   0(3,R4),RSARDEM     NO 'P' DEMO FOUND/USE 1ST DEMO               
CE60X    B     CEX                                                              
         DROP  R6                                                               
*                                                                               
CE70     DS    0H                                                               
         CLI   21(R3),X'19'        STATION BUDGET                               
         BNE   CE80                                                             
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                 ELEM MISSING, SKIP TO END                    
*                                                                               
         USING RSARXEL,R6                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         OC    RSARXBGT,RSARXBGT   IS MARKET BUDGET ZERO?                       
         BZ    CE75                YES, MAKES STATION DOLLARS ZERO              
         IC    R1,RSARXSHG         GET SHARE GOAL                               
         M     R0,RSARXBGT         MULTIPLY BY MARKET BUDGET                    
         D     R0,=F'100'          DIVIDE BY 100 FOR PERCENTAGE                 
*                                                                               
CE75     ST    R1,0(R4)            PUT STATION INTO SRTKEY                      
         XC    STADOLRS,STADOLRS                                                
         ST    R1,STADOLRS         SAVE STATION BUDGET                          
         MVI   EXNFLG,C'Y'         EXCEPTION FLAG (IF WORK NOT USED)            
         B     CEX                                                              
         DROP  R6                                                               
*                                                                               
CE80     DS    0H                                                               
         CLI   21(R3),X'17'        MARKET BUDGET                                
         BNE   CE90                                                             
         MVC   ELCODE,22(R3)       GET THE ELEMENT CODE                         
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                 ELEM MISSING, SKIP TO END                    
         ZIC   R1,23(R3)           GET DISPLACEMENT INTO SAR ELEM               
         AR    R6,R1               POINT TO MKT BUD                             
*                                                                               
         L     R1,0(R6)            GET MKT BUD                                  
CE85     ST    R1,0(R4)            PUT MKT BUDGET INTO SRTKEY                   
         MVI   EXNFLG,C'Y'         EXCEPTION FLAG (IF WORK NOT USED)            
         B     CEX                                                              
*                                                                               
* ARE WE LOOKING AT PERIODS?                                                    
*                                                                               
CE90     DS    0H                                                               
         CLI   21(R3),X'1B'        PERIOD 1                                     
         BNE   CE92                                                             
         MVC   TYMDST,YMDST1       MOVE PERIOD DATES INTO TEMP STORAGE          
         MVC   TYMDND,YMDND1                                                    
         B     CE99                PUT IN SORT KEY                              
*                                                                               
CE92     DS    0H                                                               
         CLI   21(R3),X'1D'        PERIOD 2                                     
         BNE   CE94                                                             
         MVC   TYMDST,YMDST2       MOVE PERIOD DATES INTO TEMP STORAGE          
         MVC   TYMDND,YMDND2                                                    
         B     CE99                PUT IN SORT KEY                              
*                                                                               
CE94     DS    0H                                                               
         CLI   21(R3),X'1F'        PERIOD 3                                     
         BNE   CE96                                                             
         MVC   TYMDST,YMDST3       MOVE PERIOD DATES INTO TEMP STORAGE          
         MVC   TYMDND,YMDND3                                                    
         B     CE99                PUT IN SORT KEY                              
*                                                                               
CE96     DS    0H                                                               
         CLI   21(R3),X'21'        PERIOD 4                                     
         BNE   CE98                                                             
         MVC   TYMDST,YMDST4       MOVE PERIOD DATES INTO TEMP STORAGE          
         MVC   TYMDND,YMDND4                                                    
         B     CE99                PUT IN SORT KEY                              
*                                                                               
CE98     DS    0H                                                               
         CLI   21(R3),X'23'        PERIOD 5                                     
         BNE   CE100                                                            
         MVC   TYMDST,YMDST5       MOVE PERIOD DATES INTO TEMP STORAGE          
         MVC   TYMDND,YMDND5                                                    
         B     CE99                PUT IN SORT KEY                              
*                                                                               
CE99     XC    PERTOT,PERTOT                                                    
***>     BAS   RE,CALCPERD         CALCULATE PERIOD DOLLARS                     
         GOTO1 =A(CALCPERD),RR=Y                                                
         L     R1,PERTOT           GET TOTAL PERIOD DOLLARS                     
         ST    R1,0(R4)            PUT PERIOD INTO SRTKEY                       
         MVI   EXNFLG,C'Y'         EXCEPTION FLAG (IF WORK NOT USED)            
         B     CEX                                                              
*                                                                               
CE100    DS    0H                                                               
         CLI   21(R3),X'29'        CONTRACT #?                                  
         BNE   CE110                                                            
         MVC   0(8,R4),PNDCON      PUT CONTRACT NUMBER IN KEY                   
         B     CEX                                                              
*                                                                               
CE110    DS    0H                                                               
         CLI   21(R3),X'13'        STATUS?                                      
         BNE   CE120                                                            
         CLI   19(R3),C'S'         SUBTOTAL BY STATUS?                          
         BNE   CEX                                                              
         MVC   STATCOL(1),20(R3)   SAVE COLUMN NUMBER FOR STATUS                
         B     CEX                                                              
*                                                                               
CE120    DS    0H                                                               
         CLI   21(R3),X'30'        READY TO BOOK?                               
         BNE   CE130                                                            
         MVI   EXNFLG,C'Y'         EXCEPTION FLAG (IF WORK NOT USED)            
         MVC   0(2,R4),=C'No'      DEFAULT                                      
         MVI   ELCODE,X'12'        ELEMENT CODE                                 
         L     R6,AIO                                                           
         BAS   RE,GETEL            POINT TO CORRECT ELEMENT                     
         BNE   CEX                                                              
         USING RSARXEL,R6                                                       
         TM    RSARXFLG,X'02'      READY TO BOOK?                               
         BZ    CEX                 NO                                           
         MVC   0(3,R4),=C'Yes'     YES                                          
         B     CEX                                                              
         DROP  R6                                                               
*                                                                               
CE130    DS    0H                                                               
         CLI   21(R3),X'31'        DAYS TO FLIGHT?                              
         BNE   CE140                                                            
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVI   0(R4),0             DEFAULT = 0 DAYS                             
         MVI   EXNFLG,C'Y'         EXCEPTION FLAG (IF WORK NOT USED)            
         GOTO1 DATCON,DMCB,(5,0),WORK                                           
         GOTO1 DATCON,DMCB,(3,RCONDATE),WORK+6                                  
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   0(2,R4),8(R1)       CALCULATED DAYS TO FLIGHT                    
         B     CEX                                                              
         DROP  R6                                                               
*                                                                               
CE140    DS    0H                                                               
CEX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  CHECK FOR EXCEPTIONS TO FILL IN SORT DATA                                    
**********************************************************************          
CHKEXC2  NTR1                                                                   
         MVI   EXDFLG,C'N'         INITIALIZE FLAG                              
*                                                                               
         CLI   21(R3),X'1E'        BUYER?                                       
         BNE   CE210                                                            
         MVC   PNDBUYER,0(R6)                                                   
         B     CE2X                                                             
*                                                                               
CE210    DS    0H                                                               
CE220    DS    0H                                                               
         LA    RF,TABDATE          LOOK AT TABLE OF DATES                       
*                                                                               
CE220A   CLI   0(RF),X'FF'         AT END OF TABLE?                             
         BE    CE230               YES, DONE                                    
         CLC   0(1,RF),21(R3)                                                   
         BE    CE220B                                                           
         LA    RF,1(RF)            BUMP TO NEXT DATE TYPE                       
         B     CE220A                                                           
*                                                                               
CE220B   DS    0H                                                               
         MVI   EXDFLG,C'Y'         EXCEPTION DATE FLAG                          
         B     CE2X                                                             
*                                                                               
CE230    DS    0H                                                               
CE2X     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  GET COMMENTS FROM '07' OR '11' ELEM AND PUT IN RECORD                        
**********************************************************************          
GETCOMM  NTR1                                                                   
         CLI   ROWCOL,C'C'         IS COMMENT BY COLUMN                         
         BNE   GC50                NO, DON'T PUT IN SORT KEY                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        ASSUME NON - PENDING COMMENT                 
         CLI   PNDKID,X'01'        PENDING K?                                   
         BNE   *+8                 NO, GET SPL COMMENT ELEM                     
         MVI   ELCODE,X'11'        YES, GET PENDING COMMENT ELEM                
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   GCX                 NO COMMENTS, EXIT                            
*                                                                               
*        PUT COMMENT IN SORT KEY                                                
         LA    R4,SRTCHNK                                                       
         ZIC   RF,COMCOL           GET COLUMN #                                 
         BCTR  RF,0                                                             
         MH    RF,=H'8'                                                         
         AR    R4,RF               BUMP TO CORRECT POSITION IN SRTKEY           
*                                                                               
         ZIC   R1,1(R6)            GET LEN OF COMMENT ELEM                      
         BCTR  R1,0                DECREMENT FOR ELCODE                         
         BCTR  R1,0                DECREM FOR ELEM LEN                          
         CH    R1,=H'8'            IS IT BIGGER THAN 8?                         
         BNH   *+8                 NO, LEAVE LENGTH ALONE                       
         LA    R1,8                TOO BIG, TRUNCATE FIELD TO 8                 
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R6)       MOVE COMMENT INTO SORT KEY                   
*                                                                               
         LA    R4,PNDCOM1                                                       
         ZIC   R1,1(R6)            GET LEN OF COMMENT ELEM                      
         BCTR  R1,0                DECREMENT FOR ELCODE                         
         BCTR  R1,0                DECREM FOR ELEM LEN                          
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R6)       MOVE COMMENT INTO SORT REC                   
         B     GCX                                                              
*                                                                               
GC50     DS    0H                  FOR COLUMNS BY ROWS                          
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDKADDR,RIPKEY+28     GET CONTRACT D/A TO PRINT LATER           
         DROP  R5                                                               
*                                                                               
GCX      B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*   SUBROUTINE TO PRINT HEADER FROM TABLE                                       
****************************************************************                
PRHED    NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
*                                                                               
         LA    R2,XHEAD7           POINT TO PRINT LINE                          
         CLI   PRANGFLG,C'Y'       DO WE NEED TO PRINT A PRE-HEADER?            
         BNE   *+8                 NO                                           
         LA    R2,XHEAD8           YES, SO PUT HEADER ON NEXT LINE              
*                                                                               
         LA    R6,1                COLUMN # SEARCHING FOR                       
         LA    R3,SCRTAB                                                        
*                                                                               
PH10     CLI   0(R3),X'FF'                                                      
         BNE   PH15                NOT END, GO ON                               
*                                                                               
         ZIC   R1,COMCOL           END OF TABLE, IS IT A COMMENT?               
         CR    R6,R1               MATCHING COL #'S                             
         BNE   PHX                 NO, DONE                                     
         ZIC   R1,COMLEN           GET REQUESTED LENGTH                         
         CR    R1,R0               IS REQUESTED LEN > LABEL LEN?                
         BNH   *+8                 NO, PRINT NORMALLY                           
         LA    R1,16               REQUESTED LEN TOO LONG, TRUNCATE             
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),COMMLAB     PRINT COMMENT BY COLUMN LABEL                
         ZIC   R1,COMLEN           GET REQUESTED LENGTH FOR SPACING             
         BCTR  R1,0                DECREM FOR EX MOVE & PRINT LINE POS          
         B     PH30                                                             
*                                                                               
PH15     ZIC   R1,20(R3)           GET COLUMN #                                 
         CR    R6,R1               MATCHING COLUMN NUMBERS?                     
         BE    PH20                YES, PRINT HEADER                            
         LA    R3,TABENTQ(R3)      NO, CHECK NEXT ENTRY                         
         B     PH10                                                             
*                                                                               
PH20     DS    0H                                                               
         ZIC   R1,18(R3)           GET REQUESTED LENGTH                         
         LA    R0,16               MAX LENGTH OF TAB ENT LABEL                  
         CR    R1,R0               IS REQUESTED LEN > LABEL LEN?                
         BNH   *+8                 NO, PRINT NORMALLY                           
         LA    R1,16               REQUESTED LEN TOO LONG, TRUNCATE             
*                                                                               
         BCTR  R1,0                DECREM FOR EX MOVE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)       MOVE FIELD NAME                              
*                                                                               
* CHECK IF PERIOD                                                               
         LA    R1,PER1TAB                                                       
         CR    R3,R1                                                            
         BL    PH25                BEFORE PERIODS, SKIP                         
         LA    R1,PER5TAB                                                       
         CR    R3,R1                                                            
         BH    PH25                AFTER PERIODS, SKIP                          
*                                                                               
*  PUT DISPLACEMENTS IN FOR PRE-HEADER IF PRANGFLG ON                           
         CLI   PRANGFLG,C'Y'                                                    
         BNE   PH25                                                             
         LA    R1,0(R2)            GET PRINT ADDRESS                            
         LA    R0,XHEAD8                                                        
         SR    R1,R0               GET DISPL FOR TABLE                          
         STC   R1,23(R3)           STORE IN TABLE FOR PPREHED                   
*                                                                               
PH25     ZIC   R1,18(R3)           GET REQUESTED LENGTH FOR SPACING             
         BCTR  R1,0                DECREM FOR EX MOVE & PRINT LINE POS          
*                                                                               
* INCREMENT PRINT LINE POSITION                                                 
*                                                                               
PH30     LA    R2,2(R1,R2)            INCREMENT FOR EX AND FOR SPACE            
         LA    R6,1(R6)            LOOK FOR NEXT COLUMN                         
         LA    R3,SCRTAB                                                        
         B     PH10                                                             
*                                                                               
PHX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
****************************************************************                
*   SUBROUTINE TO PRINT PRE-HEADER IF WE HAVE PERIOD RANGES                     
****************************************************************                
PPREHED  NTR1                                                                   
         L     R5,ATWA                                                          
         USING CONHEADH-64,R5                                                   
*                                                                               
         LA    R3,PER1TAB          POINT PERIODS IN SCRTAB                      
         LA    R6,ARRONEH          POINT TO FIRST PERIOD                        
*                                                                               
PP10     DS    0H                                                               
         LA    R1,PER5TAB          PAST FIFTH PERIOD?                           
         CR    R3,R1                                                            
         BH    PPX                 DONE                                         
         CLI   5(R6),0             INPUT PERIOD BLANK?                          
         BE    PPX                 YES, SHOULD EXIT?                            
* RIGHT ALIGN 'PER1' ETC. IF PER. INPUT BLANK?                                  
*                                                                               
*************************************************                               
* FIND NUM OF DATES INPUT & A LENGTH                                            
         ZIC   R1,5(R6)            GET INPUT LENGTH                             
         LA    R4,8(R6)            POINT TO BEGIN OF PERIOD INPUT               
PP20     CLI   0(R4),C'-'          DASH?                                        
         BE    PP30                TWO DATES, PNT TO SECOND & GET LEN           
         LA    R4,1(R4)                                                         
         BCT   R1,PP20             CHECK NEXT INPUT BYTE                        
*                                                                               
* ONE DATE - NOT A RANGE DON'T PRINT THIS PERIOD IN PRE-HEADER                  
         B     PP50                                                             
*                                                                               
PP30     LR    R1,R4               GET CURRENT POSITION                         
         LA    R0,8(R6)            GET BEGIN OF INPUT POS                       
         SR    R1,R0               GET DIFFERENCE                               
*        LA    R1,1(R1)            GET LENGTH                                   
         STC   R1,BYTE             STORE LENGTH                                 
*                                                                               
*******************************************                                     
         XC    0(16,R3),0(R3)       CLEAR TEXT FIELD FOR HEADER                 
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
*                                                                               
         LA    R2,XHEAD7           POINT TO PRINT LINE                          
         ZIC   R1,23(R3)           GET DISPL FOR PERIOD                         
         AR    R2,R1               POINT TO CORRECT OUTPUT SPOT                 
*                                                                               
         ZIC   R0,BYTE             GET LENGTH OF DATE                           
*        LA    R1,8                MAX POSSIBLE OUTPUT                          
         ZIC   R1,18(R3)                                                        
         CR    R1,R0                                                            
         BL    *+8                                                              
         SR    R1,R0               GET DISPL INTO SCRTAB                        
         AR    R2,R1               RIGHT ALIGN THE MVC                          
*                                                                               
         ZIC   R1,18(R3)                                                        
         ZIC   R0,BYTE             GET INPUT LEN                                
         CR    R1,R0               REQ LEN > INPUT LEN                          
         BNH   *+6                 NO, SKIP                                     
         LR    R1,R0               REQ LEN TOO BIG TRUNC TO INPUT LEN           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),8(R6)       MOVE DATE INTO SCRTAB                        
*                                                                               
PP50     DS    0H                                                               
         LA    R3,TABENTQ(R3)                                                   
         ZIC   R1,0(R6)                                                         
         AR    R6,R1               POINT TO NEXT FLD                            
         ZIC   R1,0(R6)                                                         
         AR    R6,R1               POINT TO NEXT PERIOD                         
         B     PP10                                                             
*                                                                               
PPX      B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PRROWCOM - PRINT COMMENTS BY ROWS                                             
**********************************************************************          
PRROWCOM NTR1                                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R2,XP              POINT TO PRINT LINE                           
         MVC   KEY+28(4),PNDKADDR GET CONTRACT RECORD ADDRESS                   
         GOTO1 GETREC                                                           
*                                                                               
         CLI   COMLEN,C'1'         ONE ROW ONLY?                                
         BNE   PRC50               NO, PRINT ALL OF PENDING COMMENT             
*                                                                               
********** FILL ONE PRINT LINE WITH TWO COMMENT ELEMENTS                        
         L     R6,AIO              POINT TO K RECORD                            
         MVI   ELCODE,X'07'        ASSUME NON-PENDING COM                       
         CLI   PNDKID,X'01'        PENDING K?                                   
         BNE   *+8                 NO, SKIP                                     
         MVI   ELCODE,X'11'        GET PENDING COMMENT ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   PRCX                NONE FOUND, DONE                             
*                                                                               
         ZIC   R1,1(R6)            GET ELEM LEN                                 
         BCTR  R1,0                DECR ONCE FOR ELCODE                         
         BCTR  R1,0                DECR ONCE FOR ELEM LEN                       
         BCTR  R1,0                DECR ONCE FOR EX MOVE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R2),2(R6)      PRINT FIRST COMMENT ELEM                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PRCP                ONLY ONE COMMENT, PRINT AND EXIT             
*                                                                               
         ZIC   R1,1(R6)            GET ELEM LEN                                 
         BCTR  R1,0                DECR ONCE FOR ELCODE                         
         BCTR  R1,0                DECR ONCE FOR ELEM LEN                       
         BCTR  R1,0                DECR ONCE FOR EX MOVE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   84(0,R2),2(R6)      PRINT FIRST COMMENT ELEM                     
*                                                                               
PRCP     BAS   RE,SPLAT                                                         
         B     PRCX                                                             
*                                                                               
********** PRINT ALL OF PENDING COMMENT                                         
PRC50    DS    0H                                                               
         L     R6,AIO              POINT TO K RECORD                            
         MVI   ELCODE,X'07'        ASSUME NON-PENDING COM                       
         CLI   PNDKID,X'01'        PENDING K?                                   
         BNE   *+8                 NO, SKIP                                     
         MVI   ELCODE,X'11'        GET PENDING COMMENT ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   PRCX                NONE FOUND, DONE                             
*                                                                               
PRC60    ZIC   R1,1(R6)            GET ELEM LEN                                 
         BCTR  R1,0                DECR ONCE FOR ELCODE                         
         BCTR  R1,0                DECR ONCE FOR ELEM LEN                       
         BCTR  R1,0                DECR ONCE FOR EX MOVE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R2),2(R6)      PRINT FIRST COMMENT ELEM                     
         BAS   RE,SPLAT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PRC60               ONLY ONE COMMENT, PRINT AND EXIT             
PRCX     B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PRCOMP - PRINT COMPETITIVE SHARE INFORMATION                                  
*        ONLY APPLICABLE FOR COMPLETED ORDERS                                   
**********************************************************************          
PRCOMP   NTR1                                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R2,XP              POINT TO PRINT LINE                           
         MVC   KEY+28(4),PNDKADDR GET CONTRACT RECORD ADDRESS                   
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SUMORDER         BUCKET UP THE ORDER'S $$                     
         L     R6,AIO              POINT TO K RECORD                            
         MVI   ELCODE,X'06'        RETRIEVE COMPETITIVE ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   PRCO0800            NO COMPETITIVE - FINISHED                    
         TM    RCONSPES-RCONSPEL(R6),X'20'                                      
*                                  ORDER $$ OVERRIDDEN?                         
         BNO   PRCO0005            NO                                           
         BAS   RE,GET08DOL         RETRIEVE OVERRIDE VALUE                      
         B     PRCO0015                                                         
PRCO0005 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R6),X'40'                                      
*                                  LOSS?                                        
         BNO   PRCO0015            NO                                           
         BAS   RE,GET08DOL         RETRIEVE LOSS VALUE                          
         B     PRCO0015                                                         
PRCO0015 EQU   *                                                                
         ZIC   R5,RCONSPNU-RCONSPEL(R6)                                         
*                                  SET NUMBER OF MINI-ELEMENTS                  
         LA    R3,RCONSPST-RCONSPEL(R6)                                         
*                                  SET A(1ST STATION CALL LETTERS)              
         BAS   RE,SETMKT$$         PROP STATION $$ ON PERCENT                   
*                                     DONE AFTER SETTING ADDRS ABOVE            
PRCO0020 EQU   *                                                                
         MVC   0(5,R2),0(R3)       INSERT STATION CALL LETTERS                  
         ZICM  RF,5(R3),4          RETRIEVE PERCENT VALUE                       
         LA    RF,50(RF)           HALF ROUND TO DROP DEC'L POS'S               
         SR    RE,RE               CLEAR DOUBLE REGISTER                        
         LA    R1,100                                                           
         DR    RE,R1               DIVIDE BY 100                                
         ST    RF,WORK+60          SAVE VALUE                                   
         MVI   5(R2),C'('          NO  - DISPLAY ONLY PERCENT                   
         MVI   9(R2),C')'          INSERT AFFILIATE BRACKETS                    
         BAS   RE,AFFLETRS         INSERT AFFILIATE LETTERS                     
         MVC   6(3,R2),DUB         INSERT AFFILIATE LETTERS                     
         EDIT  (4,WORK+60),(3,11(R2)),ZERO=NOBLANK                              
         MVI   14(R2),C'%'                                                      
         CLI   COMPOPT,C'D'        DOLLAR REQUEST?                              
         BNE   PRCO0040            NO                                           
         ZICM  RF,WORK+20,4        RETRIEVE DOLLAR AMOUNT                       
         SR    RE,RE               CLEAR REGISTER FOR MULT                      
         ZICM  R1,WORK+60,4        RETRIEVE STATION PERCENTAGE                  
         MR    RE,R1               $$ * %                                       
         A     RF,=F'50'           HALF-ADD FOR ROUNDING                        
         SR    RE,RE                                                            
         LA    R1,100              SET NEW DIVISOR                              
         DR    RE,R1               DROP DECIMAL POSITIONS                       
         ST    RF,WORK+24          SAVE STATION'S DOLLARS                       
         LR    RF,R2               POSITION TO XP LINE 2                        
         LA    RF,L'XP(RF)                                                      
         EDIT  (4,WORK+24),(10,4(RF)),ZERO=NOBLANK                              
*                                  EDIT STATION'S VALUE                         
PRCO0040 EQU   *                                                                
         LA    R3,9(R3)            BUMP TO NEXT ELEMENT                         
         LA    R2,16(R2)           BUMP TO NEXT PRINT POSITION                  
         BCT   R5,PRCO0020                                                      
         BAS   RE,SPLAT            PRINT OUTPUT                                 
         B     PRCO0800            EXIT                                         
PRCO0800 XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* SETMKT$$ - prop station $$ on percent to figure market $$                     
*        R3 -> first station calls/percent                                      
**********************************************************************          
SETMKT$$ NTR1                                                                   
         TM    RCONSPES-RCONSPEL(R6),X'40'                                      
*                                  LOSS?                                        
         BO    SMKT0020            YES - $$ ARE MARKET FIGURE                   
         ZICM  R1,WORK+20,4        RETRIEVE DOLLAR VALUE                        
         SR    R0,R0                                                            
         LA    RF,100                                                           
         MR    R0,RF               MULTIPLY VALUE BY 100                        
         ST    R1,WORK+32          STORE VALUE TEMPORARILY                      
         ZICM  RF,5(R3),4          RETRIEVE REP'D STA PERCENT VALUE             
         SR    RE,RE                                                            
         LA    RF,50(RF)           HALF ROUND TO DROP DEC'L POS'S               
         SR    RE,RE               CLEAR DOUBLE REGISTER                        
         LA    R1,100                                                           
         DR    RE,R1               DIVIDE BY 100                                
         LR    R1,RF               PUT PERCENT INTO R1                          
         LR    R2,R1               ALSO PUT INTO R2                             
         SRL   R2,1                DIVIDE PERCENT BY 2 FOR ROUNDING             
         L     RF,WORK+32          RETRIEVE DOLLAR VALUE                        
         AR    RF,R2               ADD 1/2 OF THE PERCENT                       
         SR    RE,RE               CLEAR SECOND REGISTER                        
         XC    WORK+20(4),WORK+20                                               
         LTR   R1,R1               ANY PERCENT HERE?                            
         BZ    SMKT0020            NO  - DON'T DIVIDE                           
         DR    RE,R1               $$ / %                                       
         ST    RF,WORK+20          REPLACE $$ AS MARKET DOLLARS                 
SMKT0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* GET08DOL - retrieve order budget from x'08' element                           
**********************************************************************          
GET08DOL NTR1                                                                   
         L     R6,AIO              POINT TO K RECORD                            
         MVI   ELCODE,X'08'        RETRIEVE TRUE ACT DATE ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         MVC   WORK+20(4),RCONAC$$-RCONACEL(R6)                                 
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* AFFLETRS - retrieve this station's affiliate.                                 
**********************************************************************          
AFFLETRS NTR1                                                                   
         XC    DUB,DUB             CLEAR OUT DUB                                
         LA    RF,COMSCALL         SET A(1ST CALL LETTERS)                      
AFFL0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BE    AFFL0060            YES                                          
         CLC   0(5,R3),0(RF)       STATION CALLS IN TABLE?                      
         BE    AFFL0040            YES - SET A(ENTRY)                           
         LA    RF,COMLENE(RF)      BUMP TO NEXT ENTRY                           
         B     AFFL0020            GO BACK FOR NEXT                             
AFFL0040 EQU   *                                                                
         MVC   DUB(3),5(RF)        INSERT AFFILIATE FOR PASSBACK                
AFFL0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUMORDER - ACCUMULATE BUCKET TOTALS                                           
*        THIS VERSION DOESN'T APPLY AN AS-AT DATE TO THE BUCKETS                
**********************************************************************          
SUMORDER NTR1                                                                   
         L     R6,AIO              POINT TO K RECORD                            
         XC    WORK+20(4),WORK+20                                               
         MVI   ELCODE,X'03'        RETRIEVE ESTIMATE ELEMENT                    
         BAS   RE,GETEL                                                         
         B     SUMO0040                                                         
SUMO0020 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
SUMO0040 EQU   *                                                                
         BNE   SUMO0800            NO COMPETITIVE - FINISHED                    
         ZICM  RF,6(R6),4          UNLOAD DOLLAR VALUE                          
         L     RE,WORK+20                                                       
         AR    RE,RF               ACCUMULATE TOTAL                             
         ST    RE,WORK+20          PUT IT BACK                                  
         B     SUMO0020            GO BACK FOR NEXT                             
SUMO0800 EQU   *                                                                
         L     RF,WORK+20          REMOVE PENNIES                               
         LA    RE,0                                                             
         LA    R1,100                                                           
         DR    RE,R1               DIVIDE BY 100                                
         ST    RF,WORK+20          PUT BACK $$ W/0 PENNIES                      
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ADDTOT - ACCUMULATE TOTALS - R4 HOLDS DISPL INTO DOLTOTS                      
**********************************************************************          
ADDTOT   NTR1                                                                   
         LA    R5,DOLTOTS          BEG OF TABLE                                 
*                                                                               
AT10     DS    0H                                                               
         ZIC   RF,COLBKFLG         NEED TO FIND LAST DOLTOT BLK                 
         BCTR  RF,0                                                             
         CLI   STATCOL,0           DO WE SUBT BY STATUS?                        
         BE    AT15                NO, DON'T DECREM LAST DOLTOT BLK             
         BCTR  RF,0                STAT SUBT, DECREM LAST DOLTOT BLK            
*                                                                               
AT15     LA    R1,KYBKLN                                                        
         SR    R0,R0                                                            
         MR    R0,RF               DISP TO LAST POS TOT BLOCK                   
         LA    RF,DOLTOTS          BEG OF TABLE                                 
         AR    R1,RF               POINT TO  LAS TOT BLOCK                      
         CR    R5,R1               ARE WE PAST IT?                              
         BH    AT20                YES, DONE                                    
*                                                                               
         LR    RF,R5               COPY POSITION                                
         AR    RF,R4               GET CORRECT DIPLACEMENT INTO BLOCK           
         L     R1,0(RF)            GET TOTAL                                    
         A     R1,0(R6)            ACCUMULATE                                   
         ST    R1,0(RF)            STORE IN TOTAL                               
*                                                                               
         LA    R5,KYBKLN(R5)                                                    
         B     AT10                LOAD NEXT TOTAL                              
*                                                                               
AT20     DS    0H                                                               
         LA    RF,STATTOT          POINT TO STATUS PART OF TOTAL TABLE          
         AR    RF,R4               GET CORRECT DIPLACEMENT INTO BLOCK           
         L     R1,0(RF)            GET TOTAL                                    
         A     R1,0(R6)            ACCUMULATE                                   
         ST    R1,0(RF)            STORE IN TOTAL                               
ATX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PRTOTS- PRINT TOTALS - DUB   HOLDS # OF FIRST BREAK FIELD (N-1)               
**********************************************************************          
PRTOTS   NTR1                                                                   
         LA    R5,KYBKLN                                                        
         SR    R4,R4                                                            
         L     RF,DUB              # OF FIRST BRK FIELD (IN N-1 TERMS)          
         MR    R4,RF               GET DISPL TO FIRST BRK IN DOLTOTS            
         LA    RF,DOLTOTS                                                       
         AR    R5,RF               POINT TO 1ST BRK IN DOLTOT                   
*                                                                               
         ZIC   RF,COLBKFLG         # OF POSSIBLE KEY FIELDS                     
         BCTR  RF,0                                                             
         CLI   STATCOL,0           DO WE SUBT BY STATUS?                        
         BE    PRTT05              NO, DON'T DECREM LAST DOLTOT BLK             
         BCTR  RF,0                STAT SUBT, DECREM LAST DOLTOT BLK            
*                                                                               
PRTT05   SR    R0,R0                                                            
         LA    R1,KYBKLN           LENGTH OF DOLTOT BLOCK                       
         MR    R0,RF               DISP TO LAST POS TOT BLOCK                   
         LA    R3,DOLTOTS          BEG OF TABLE                                 
         AR    R3,R1               POINT TO  LAS TOT BLOCK                      
*                                                                               
PRTT10   DS    0H                                                               
         CR    R3,R5               ARE WE PAST FIRST BRK?                       
         BL    PRTT20              YES, DONE                                    
         BE    PRTT15              AT FIRST BREAK, SKIP SAME TOTS CHK           
*                                                                               
* CHECK IF TOTALS SAME, IF YES SKIP, PRINT NEXT                                 
         LR    R1,R3                                                            
         LA    RF,KYBKLN                                                        
         SR    R1,RF               DECREM TO PREVIOUS TOT BLOCK                 
         CLC   0(KYBKLN,R3),0(R1)  TOTALS SAME?                                 
         BE    PRTT19              SKIP THIS TOTAL                              
*                                                                               
PRTT15   DS    0H                                                               
         BAS   RE,PRTOTTX          PRINT 'TOTAL' TEXT                           
         BAS   RE,PRTONE           PRINT A SINGLE LINE OF TOTALS                
*                                                                               
PRTT19   DS    0H                                                               
         XC    0(KYBKLN,R3),0(R3)       CLEAR A TOTALS BLOCK                    
         LA    RF,KYBKLN                                                        
         SR    R3,RF               DECREM TO PREVIOUS TOT BLOCK                 
         B     PRTT10                                                           
*                                                                               
PRTT20   DS    0H                                                               
         CLC   PNDKID(1),PREVIOUS  STATUS CHANGE?                               
         BE    PRTTX               NO, EXIT                                     
*                                                                               
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R2,XP                                                            
*                                                                               
         MVC   PREVIOUS(1),LASTSTAT                                             
         CLI   PREVIOUS,1                                                       
         BNE   *+14                                                             
         MVC   0(13,R2),=C'PENDING TOTAL'                                       
         B     PRTT30                                                           
         CLI   PREVIOUS,2                                                       
         BNE   *+14                                                             
         MVC   0(16,R2),=C'INCOMPLETE TOTAL'                                    
         B     PRTT30                                                           
         CLI   PREVIOUS,3                                                       
         BNE   *+14                                                             
         MVC   0(14,R2),=C'COMPLETE TOTAL'                                      
         B     PRTT30                                                           
         MVC   0(10,R2),=C'LOSS TOTAL'                                          
*                                                                               
PRTT30   LA    R3,STATTOT                                                       
         BAS   RE,PRTONE           PRINT A SINGLE LINE OF TOTALS                
         XC    0(KYBKLN,R3),0(R3)       CLEAR A TOTALS BLOCK                    
         DROP  R4                                                               
PRTTX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PRTONE- PRINT A LINE OF TOTALS, R3 POINTS TO CORRECT DOLTOTS BLOCK            
**********************************************************************          
PRTONE   NTR1                                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R2,XP                                                            
*                                                                               
         LA    R5,TABEXPER         POINT TO PER AND BUD TABLE                   
*                                                                               
PTON10   DS    0H                                                               
         CLI   0(R5),X'FF'         END?                                         
         BE    PTONX               YES, DONE                                    
         CLI   2(R5),0             PER/BUD DOES NOT EXIST?                      
         BE    PTON20              CHECK NEXT TABLE ENTRY                       
*                                                                               
         LR    RF,R3               GET A(DOLTOTS ENTRY)                         
         ZIC   R0,1(R5)            GET DISPL INTO BLOCK                         
         AR    RF,R0               GET CORRECT TOTAL                            
*                                                                               
         ZIC   R0,2(R5)            GET DISPL TO PRINT                           
         AR    R2,R0                                                            
*                                                                               
         L     R1,0(RF)            GET $ VALUE                                  
         CLI   RDOLLAR,C'Y'        ROUND DOLLARS?                               
         BNE   PTON19              NO,CONTINUE                                  
         SR    R0,R0               ZERO OUT REMAINDER                           
         L     RF,=F'500'                                                       
         AR    R1,RF               DO FOR HALF-ROUNDING                         
         SLA   RF,1                NEED 1000                                    
         DR    R0,RF               DIVIDE BY 1000                               
*                                                                               
PTON19   EDIT  (R1),(10,WORK2),ZERO=NOBLANK                                     
         LA    R1,10               MAXIMUM POSSIBLE OUTPUT LENGTH               
         ZIC   RF,3(R5)            REQUESTED LENGTH                             
         SR    R1,RF               GET DISPLACEMENT INTO WORK2                  
         LA    RF,WORK2                                                         
         AR    RF,R1               POINT TO START OF DATA IN WORK2              
*                                                                               
         ZIC   R1,3(R5)           GET REQ LENGTH AGAIN                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
PTON20   LA    R2,XP               REPOSITION TO BEG. OF PRINT LINE             
         LA    R5,PERENTQ(R5)      CHECK NEXT PERIOD/BUDGET                     
         B     PTON10                                                           
*                                                                               
PTONX    BAS   RE,SPLAT            PRINT TOTALS                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PRTOTTX - PRINT 'TOTAL' IN THE APPROPRIATE SPOT                               
**********************************************************************          
PRTOTTX  NTR1                                                                   
* R3 HAS THE ADDRESS OF A DOLTOTS BLOCK CORRESPONDING TO THE CURRENT            
* - SUBTOTAL KEY BREAK FIELD                                                    
         XC    WORK2,WORK2                                                      
*                                                                               
         LR    R1,R3                                                            
         SR    R0,R0                                                            
         LA    RF,STATTOT          GET ADDRESS OF STATION TOT BLOCK             
         SR    R1,RF               GET DIFFERENCE BETWEEN ADD'S                 
         LA    RF,KYBKLN           GET LEN OF A DOLTOT BLOCK                    
         DR    R0,RF               YIELDS COLUMN NUMBER OF CURRENT FLD          
*                                                                               
         OC    STATCOL,STATCOL     NO STATUS SUBTOTAL?                          
         BZ    PTTX05              NONE, SKIP                                   
         ZIC   RF,STATCOL                                                       
         CR    R1,RF               ARE WE BEFORE STATUS COLUMN?                 
         BL    *+8                 YES, SKIP                                    
         LA    R1,1(R1)            NO, +1 TO SKIP STATUS SUBTOTAL               
*                                                                               
PTTX05   STC   R1,WORK2            STORE IN WORK2                               
         LA    R3,SCRTAB                                                        
*                                                                               
PTTX10   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    PTTXX               NOT FOUND, JUST EXIT                         
         CLC   WORK2(1),20(R3)     SAME COLUMN #?                               
         BE    PTTX20              YES, CHECK NEXT SCRTAB                       
         LA    R3,TABENTQ(R3)                                                   
         B     PTTX10                                                           
*                                                                               
PTTX20   DS    0H                                                               
         LA    R5,BRKTAB                                                        
*                                                                               
PTTX30   CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    PTTXX               YES, JUST EXIT                               
         CLI   0(R5),X'1F'         STATUS?                                      
         BE    PTTXX               YES, JUST EXIT                               
         CLC   21(1,R3),0(R5)      CORRECT FIELD/ELEM CODE?                     
         BE    PTTX40              YES, PRINT 'TOTAL'                           
         LA    R5,BKTBEQ(R5)       CHECK NEXT BREAK FIELD                       
         B     PTTX30                                                           
*                                                                               
PTTX40   DS    0H                                                               
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R2,XP                                                            
         ZIC   R1,1(R5)            GET PRINT DISPL                              
         AR    R2,R1               GET ADDRESS                                  
         MVC   0(5,R2),=C'TOTAL'                                                
*                                                                               
         CLI   19(R3),C'P'         IS BREAK FLD A PAGE BREAK?                   
         BNE   PTTXX                                                            
         MVI   PGBKFLG,C'Y'        SHOULD BREAK                                 
PTTXX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* FNDBKCL - FIND THE COLUMN OF LOWEST KEY FIELD TO CHK FOR BREAKS               
*         -  ALSO, INITIALIZE TABEXPER                                          
**********************************************************************          
FNDBKCL  NTR1                                                                   
         LA    R3,SCRTAB           THE INTERNAL TABLE                           
         LA    R6,1                CHECK FOR FIRST COLUMN #                     
         MVI   COLBKFLG,0                                                       
*                                                                               
BKCL10   DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    BKCLX               YES, DONE                                    
         ZIC   R1,20(R3)           GET COLUMN #                                 
         CR    R1,R6               ARE WE AT CORRECT COLUMN #?                  
         BE    BKCL20              YES, CHECK IF WE BREAK ON THIS FIELD         
         LA    R3,TABENTQ(R3)      NO WE DON'T, CHECK NEXT TABLE ENTRY          
         B     BKCL10                                                           
*                                                                               
BKCL20   DS    0H                  DO WE BREAK ON THIS KIND OF FLD?             
* COMPARE TO ELCODES                                                            
* IF NOT EQUAL, EXIT W/ COLBKFLG FILLED WITH R6                                 
* GARUANTEED TO BE AT LEAST ONE SINCE MUST 'P' ON STA OR OFF                    
         CLI   19(R3),C'P'         DO WE PAGE BREAK AND TOTAL?                  
         BE    BKCL25              YES                                          
         CLI   19(R3),C'S'         DO WE SUBTOTAL?                              
         BNE   BKCLX               NO, EXIT                                     
*                                                                               
* NEEDED TO TAKE THIS OUT.  WE DO HAVE A BLANK KEY FIELD FOR                    
* STATUS.  WITH THESE INSTR'S, WE SKIP LATER KEY FIELDS                         
*        CLI   21(R3),X'13'        STATUS SUBTOTAL?                             
*        BE    BKCL31              DON'T ACCUM # OF COLBK FIELDS                
*                                                                               
BKCL25   DS    0H                  DO WE BREAK ON THIS KIND OF FLD?             
         LA    RF,BRKTAB           TABLE OF KEY FIELDS TO BREAK ON              
*                                                                               
BKCL26   CLI   0(RF),X'FF'         AT END OF TABLE?                             
         BE    BKCLX               YES, DONE                                    
         CLC   0(1,RF),21(R3)                                                   
         BE    BKCL30              YES                                          
         LA    RF,BKTBEQ(RF)       *BUMP TO NEXT KIND OF KEY FIELD              
         B     BKCL26                                                           
*                                                                               
BKCL30   DS    0H                                                               
         ZIC   R1,COLBKFLG                                                      
         LA    R1,1(R1)                                                         
         STC   R1,COLBKFLG         ACCUMULATE NUMBER OF KEYBK FIELDS            
BKCL31   LA    R6,1(R6)            CHECK NEXT COL #                             
         LA    R3,SCRTAB           RESET                                        
         B     BKCL10                                                           
*                                                                               
BKCLX    DS    0H                                                               
         XC    STATTOT,STATTOT     CLEAR 9NTH TABLE ENTRY OF DOLTOTS            
         XC    DOLTOTS,DOLTOTS     CLEAR TABLE OF $ TOTALS                      
*                                                                               
* FILL PERIOD INPUT FLAGS TO FLOAT IN DATES FOR PERIOD HEADERS                  
*                                                                               
         LA    R3,TABEXPER                                                      
         LA    RF,PERFLG1          POINT TO FIRST PERIOD INPUT FLAG             
BKCLX10  CLI   0(R3),X'FF'                                                      
         BE    BKCLXX              DONE                                         
         XC    2(1,R3),2(R3)       CLEAR DISPLACEMENT ONTO PRINT LINE           
*                                                                               
         CLI   0(R3),X'19'         STATION BUDGET?                              
         BE    BKCLX20             YES, IGNORE PERIOD FLAGS                     
         CLI   0(R3),X'17'         MARKET BUDGET?                               
         BE    BKCLX20             YES, IGNORE PERIOD FLAGS                     
*                                                                               
         MVC   4(1,R3),0(RF)       MOVE IN PERIOD INPUT FLAG                    
         LA    RF,1(RF)            CHECK NEXT PERIOD FLAG                       
*                                                                               
BKCLX20  LA    R3,PERENTQ(R3)      CHECK NEXT                                   
         B     BKCLX10                                                          
*                                                                               
BKCLXX   B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* FILPRAD - SAVE THE PRINTING ADDRESS OF KEY BREAK FIELDS                       
**********************************************************************          
FILPRAD  NTR1                                                                   
         LA    R5,BRKTAB                                                        
*                                                                               
FPA10    DS    0H                                                               
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    FPAX                YES, DONE                                    
         CLI   0(R5),X'1F'         STATUS?                                      
         BE    FPAX                YES, JUST EXIT                               
         CLC   21(1,R3),0(R5)      CORRECT FIELD/ELEM CODE?                     
         BE    FPA20               YES, SAVE PRINT ADDRESS                      
         LA    R5,BKTBEQ(R5)       CHECK NEXT BREAK FIELD                       
         B     FPA10                                                            
*                                                                               
FPA20    DS    0H                                                               
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R1,XP               BEG OF PRINT LINE                            
         LR    R0,R2               GET CURRENT PRINTING ADDRESS                 
         SR    R0,R1               GET PRINTING DISPL TO KEY FLD                
         STC   R0,1(R5)            STORE DISPL ADDRESS                          
FPAX     B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* GETS PRODUCT NAME FROM ELEMENT 5 OR PRODUCT RECORD                            
* AND RETURNS IT IN WORK                                                        
**********************************************************************          
FILLPROD NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONPRD,X'40'       ,,IF PROD NOT = BLANK                        
         BNH   FILL10                                                           
         MVC   WORK(3),RCONPRD     ,,PASS PROD CODE IN WORK                     
         BAS   RE,GETPROD               RETURNS PRODUCT NAME IN WORK            
         B     FILL25                                                           
*                                                                               
FILL10   MVI   ELCODE,5            ,,ELSE PROD NAME ON ELEM 5                   
         BAS   RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   WORK(20),RCONEXPR                                                
*                                                                               
FILL25   EQU   *                                                                
         B     EXIT                                                             
         DROP R6                                                                
*****************************************************************               
* READS PRODUCT RECORD                                                          
* EXPECTS PROD CODE IN WORK - CONTRACT RECORD IN AIO                            
* RETURNS PROD NAME IN WORK                                                     
*****************************************************************               
GETPROD  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RPRDREC,R5                                                       
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RPRDNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
****************************************************************                
*  GET THE OFFICE AND NATIONAL/LOCAL FLAG AND STORE IN TBL                      
****************************************************************                
LOADOFF  NTR1                                                                   
         L     RE,=A(OFF2TBL)      CLEAR OFFICE TABLE                           
         LA    RF,TBLNEQ                                                        
         XCEF                                                                   
*                                                                               
         L     R4,=A(OFF2TBL)      LOAD  OFFICE TABLE                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           OFFICE2 RECORD                               
         MVC   KEY+23(2),AGENCY                                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     LO11                                                             
LO10     GOTO1 SEQ                                                              
LO11     CLC   KEY(25),KEYSAVE     SAME REP?                                    
         BNE   LOX                                                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        GET OFFICE FAX ELEM                          
         BAS   RE,GETEL                                                         
         BNE   LO10                CHECK NEXT RECORD                            
         USING ROFF2FXE,R6                                                      
*                                                                               
         MVI   2(R4),C'N'          ASSUME NATIONAL                              
         TM    ROFF2PRF+1,X'80'    LOCAL?                                       
         BZ    *+8                 NO, SKIP                                     
         MVI   2(R4),C'L'          YES, IT'S LOCAL                              
         L     R6,AIO                                                           
         USING ROFF2KEY,R6                                                      
         MVC   0(2,R4),ROFF2OFF    STORE OFFICE                                 
*                                                                               
         LA    R4,TBENTEQ(R4)      FILL IN NEXT TABLE ENTRY                     
         B     LO10                                                             
*                                                                               
LOX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
* RETURN AGENCY NAME - EXPECTS CONTRACT REC IN AIO                              
*********************************************************************           
FILLAGY  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RAGYKEY,R5                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,RCONKAOF                                                
         MVC   RAGYKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* HEADLINE HOOK                                                                 
*********************************************************************           
HOOK     NTR1                                                                   
         L     R4,ADRWIDE                                                       
         USING WIDED,R4                                                         
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
*                                                                               
         L     R1,ATWA             TO LOOK AT SCREEN FIELDS                     
         USING CONHEADH-64,R1                                                   
*                                                                               
         CLI   OFFTAB+19,C'P'        DO WE PAGEBREAK ON OFFICE?                 
         BE    HO10                                                             
         CLI   STATAB+19,C'P'      DO WE PAGEBREAK ON STATION?                  
         BE    HO9                                                              
         CLI   SALTAB+19,C'P'      DO WE PAGEBREAK ON S/P?                      
         BE    HO11                                                             
         CLI   SALCTAB+19,C'P'     DO WE PAGEBREAK ON S/P CODE?                 
         BE    HO12                YES                                          
         DC    H'0'                UNRECOGNIZED BREAK                           
*                                                                               
*                                                                               
* IF BREAK ON STATION, SHOW STATION FIELD                                       
HO9      MVC   XHEAD3(8),=C'STATION:'                                           
         MVC   XHEAD3+9(4),PREVIOUS+1                                           
         B     HO40                                                             
* IF BREAK ON OFFICE, SHOW OFFICE FIELD                                         
HO10     MVC   XHEAD3(6),=C'OFFICE'                                             
         MVC   XHEAD3+9(2),PREVIOUS+1                                           
         B     HO40                                                             
* IF BREAK ON S/P NAME, SHOW NAME                                               
HO11     MVC   XHEAD3(6),=C'S/P   '                                             
         MVC   XHEAD3+9(20),PREVSPER                                            
         B     HO40                                                             
* IF BREAK ON S/P CODE, SHOW CODE                                               
HO12     MVC   XHEAD3(6),=C'S/P   '                                             
         MVC   XHEAD3+9(3),PREVIOUS+1                                           
         B     HO40                                                             
         DROP  R1                                                               
*                                                                               
HO40     DS    0H                                                               
         MVC   XHEAD4(7),=C'PERIOD:'                                            
         GOTO1 DATCON,DMCB,(2,RIPDATS),(5,XHEAD4+9)                             
         MVI   XHEAD4+17,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,RIPDATE),(5,XHEAD4+18)                            
         MVC   XHEAD5(9),=C'ACTIVITY:'                                          
         MVC   WORK(3),MONDAYDT                                                 
         MVC   WORK+3(3),BTODAY                                                 
         CLI   ACTSTR,0                                                         
         BE    HO42                                                             
*        BAS   RE,CHKACT           CHECK IF ACTIVITY DATE IS ONE DATE           
         MVC   WORK(3),ACTSTR                                                   
         MVC   WORK+3(3),ACTEND                                                 
HO42     GOTO1 DATCON,DMCB,(3,WORK),(5,XHEAD5+9)                                
         MVI   XHEAD5+17,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,WORK+3),(5,XHEAD5+18)                             
*                                                                               
* CALL PRHED TO PRINT DYNAMIC HEADER                                            
         MVI   PRANGFLG,C'N'       SET PERIOD RANGE FLAG TO NO                  
*                                                                               
         MVI   XHEAD6,0            PRINT BLANK LINE                             
         GOTO1 =A(FLTPER),RR=RELO  FLOAT PERIOD DATES INTO HEADER               
         BAS   RE,PRHED            USE XHEAD7 OR XHEAD8 FOR HEADER              
         CLI   PRANGFLG,C'Y'       PERIOD RANGE FLAG ON?                        
         BE    *+12                                                             
         MVI   XHEAD8,0            NO, PRINT NORM HEADER AND BLANK LINE         
         B     HOXX                EXIT                                         
*                                                                               
         BAS   RE,PPREHED          PERIOD RANGE(S), PRINT PRE-HEADER            
         MVI   XHEAD9,0            PRINT BLANK LINE                             
*                                                                               
HOXX     B     EXIT                                                             
         DROP  R5,R4                                                            
**********************************************************************          
* EXPECTS DEMO CODE IN WORK                                                     
* RETURNS DEMO NAME IN WORK                                                     
**********************************************************************          
GETDEMNM NTR1                                                                   
         GOTO1 =A(XGETDEM),DMCB,SORTREC,RR=RELO                                 
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*                                                                               
MIDTABLE DS    0CL10                                                            
         DC    CL10'PENDING'                                                    
         DC    CL10'INCOMPLETE'                                                 
         DC    CL10'COMPLETED'                                                  
         DC    CL10'LOSSES'                                                     
**********************************************************************          
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GET ADVERTISER                                                                
**********************************************************************          
GETADVR  NTR1                                                                   
         XC    WORK(5),WORK                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),RCONKADV                                               
         MVC   KEY+25(2),RCONKREP                                               
GETADV2  GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETADV3                                                          
         CLC   RCONKREP,KEY+25                                                  
         BE    GETADV4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    GETADV4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     GETADV2                                                          
GETADV3  MVC   WORK(12),=C'ADV UNKNOWN '                                        
         B     GETADVX                                                          
*                                                                               
GETADV4  EQU   *                                                                
         USING RADVREC,R4                                                       
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RADVNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     GETADVX                                                          
         DROP  R4                                                               
GETADVX  EQU   *                                                                
         B     EXIT                                                             
**********************************************************************          
* GET SALESPERSON NAME                                                          
**********************************************************************          
GETSLSNM NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSALREC,R1                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSAL                                                          
         SPACE 1                                                                
GTDISSAL EQU   *                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSALREC,R1                                                       
         MVC   WORK(20),RSALNAME                                                
         B     GTSALX                                                           
         DROP  R1                                                               
                                                                                
GTNOSAL  MVC   WORK(20),=CL20'NOT FOUND'                                        
         B     GTSALX                                                           
                                                                                
GTSALX   EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET STATION MARKET                                                            
**********************************************************************          
GETMRKT  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSTAREC,R1                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSTA                                                          
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSTAREC,R1                                                       
         MVC   WORK(20),RSTAMKT                                                 
         B     GTSTAX                                                           
         DROP  R1                                                               
GTNOSTA  MVC   WORK(20),=CL20'UNKNOWN MARKET'                                   
         B     GTSTAX                                                           
GTSTAX   EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CHECK IF ACTIVITY DATE IS ONE DATE - CONVERT TO M-S DATES                     
**********************************************************************          
*&&DO                                                                           
CHKACT   NTR1                                                                   
         L     R5,ATWA                                                          
         USING CONHEADH-64,R5                                                   
*                                                                               
         GOTO1 DATVAL,DMCB,(0,ARRADTE),WORK                                     
         OC    DMCB(4),DMCB                                                     
*        BZ    ERREND                                                           
* - ARE THERE TWO DATES ?                                                       
         LA    R2,ARRADTE                                                       
         LA    R3,8(R2)                                                         
         LA    R1,9                LOOK FOR DASH                                
         CLI   0(R3),C'-'                                                       
         BE    CKACX               TWO DATES, DONE                              
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
* CONVERT ONE INPUT DATE TO M-S ACTIVITY DATES OF THAT WEEK                     
* GET MONDAY                                                                    
         GOTO1 GETDAY,DMCB,WORK,NMDAY                                           
         ZIC   R6,0(R1)            GET NUMERIC VALUE OF DAY                     
         BCTR  R6,0                SUBTRACT ONE FROM DAY                        
         LNR   R6,R6               CONVERT TO NEGATIVE NUMBER                   
         GOTO1 ADDAY,DMCB,WORK,WORK2,(R6)                                       
         GOTO1 DATCON,DMCB,(0,WORK2),(3,ACTSTR)                                 
*                                                                               
* GET SUNDAY                                                                    
         GOTO1 GETDAY,DMCB,WORK,NMDAY                                           
         LA    R6,7                                                             
         ZIC   R0,0(R1)            GET NUMERIC VALUE OF DAY                     
         SR    R6,R0                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK2,(R6)                                       
         GOTO1 DATCON,DMCB,(0,WORK2),(3,ACTEND)                                 
*                                                                               
CKACX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*&&                                                                             
*                                                                               
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
** SORTREC ***********************************************************          
**********************************************************************          
SORTREC  DS    0CL1000             USE THIS LABEL TO ESTABLISH                  
*                                  ADDRESSABILITY FOR XRTNS                     
SRTREC   DS    0CL1000                                                          
*                                                                               
SRTKEY   DS    0CL225              ((27*8)+9 BYTES)                             
PNDKID   DS    CL1                 X'01'/02/03/04  REPORT TYPE                  
SRTCHNK  DS    27CL8                                                            
PNDKCON  DS    CL8                 CONTRACT NUMBER                              
SRTDATA  DS    CL(L'SRTREC-L'SRTKEY)                                            
SRTEND   EQU   *                                                                
SRTLENE  EQU   *-SRTREC                                                         
*                                                                               
* COMMON DATA     ****************************************************          
         ORG   SRTDATA                                                          
PNDDATA  DS    0CL(L'SRTDATA)                                                   
PNDSLSNM DS    CL20                SALESPERSON NAME                             
PNDPROD  DS    CL20                PRODUCT                                      
PNDDAGY  DS    CL20                AGENCY                                       
PNDBUYER DS    CL20                BUYER                                        
PNDADVN  DS    CL30                ADVERTISER NAME                              
PNDCOMMT DS    CL65                COMMENT                                      
PNDCOM1  DS    CL65                COMMENT, FOR COMPLETE K'S                    
PNDCON   DS    CL8                 CONTRACT NUMBER                              
PNDKADDR DS    F                   A(CONTRACT REC)                              
PNDECOM EQU    *                   END OF COMMON DATA                           
*                                                                               
* COMPLETED & LOSS DATA **********************************************          
         ORG   PNDECOM                                                          
COMSCALL DS    CL5                 COMPLETED CALL LETTERS                       
COMSAFF  DS    CL3                 COMPLETED STATION AFFILIATION                
COMSSHR  DS    CL4                 COMPLETED STATION % SHARE                    
COMSAMT  DS    PL16                COMPLETED STATION $ SHARE                    
COMLENE  EQU   *-COMSCALL                                                       
         DS    (NUMSTAS-1)XL(COMLENE)                                           
         DS    XL(L'PNDDATA-(*-PNDDATA))                                        
*                                                                               
** END OF SORTREC ****************************************************          
*                                                                               
**********************************************************************          
* -  CONSTANTS FOR TSAROFF                                                      
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFF                      
TSARBUFL DC    A(SRTLENE*15000)                                                 
TSARBTOT DC    A((SRTLENE*15000)+10000)                                         
*                                  GET 10K FOR AFFILIATE TABLE                  
AFFTABLE DS    A                                                                
*                                  AFFILIATE TABLE: 8-CHAR ENTRIES              
*                                  BYTES 0 - 4 = STATION CALL LETTERS           
*                                  BYTES 5 - 8 = AFFILIATE ID                   
*                                                                               
RELO     DS    A                                                                
MYCOUNT  DS    F                   TESTING                                      
*                                                                               
LVLIND   DS    X                                                                
TSAREA   DS    XL64                                                             
*                                                                               
MYTSREC  DS    0D                                                               
*                                                                               
MYTSKEY  DS    CL(L'SRTKEY)                                                     
MYTSKEYL EQU   *-MYTSKEY                                                        
MYTSDATA DS    CL(SRTEND-SRTDATA)                                               
MYTSRECL EQU   *-MYTSREC                                                        
*                                                                               
OFFLIST  DS    CL500               VALID OFFICES FOR REGION (2 X 250)           
LOFFLIST EQU   *-OFFLIST                                                        
         DC    X'00'                                                            
*                                                                               
OFF2TBL  DS    0CL750              250 3-BYTE ENTRIES                           
         DS    CL2                 OFF CODE                                     
         DS    CL1                 OFF SCOPE                                    
*                                                                               
TBENTEQ  EQU   *-OFF2TBL LENGTH OF ONE ENTRY                                    
*                                                                               
         DS    249CL3               REST OF ENTRIES                             
*                                                                               
TBLNEQ   EQU   *-OFF2TBL LENGTH OF TABLE                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
*                                                                               
*********************************************************************           
* REPORT HEADLINE SPECS                                                         
*********************************************************************           
REGSPECS DS    0H                                                               
         SPROG 0                                                                
         XSPEC H1,89,AGYNAME                                                    
         XSPEC H2,89,RUN                                                        
         XSPEC H1,47,C'ACTIVITY REPORT'                                         
         XSPEC H2,47,C'-------- ------'                                         
         XSPEC H3,89,REQUESTOR                                                  
         XSPEC H5,89,REPORT                                                     
*                                                                               
*         STATION OR OFFICE APPEARS 3RD LINE, 2 COLUMN                          
*                                                                               
         XSPEC H4,89,PAGE                                                       
         DC    X'00'                                                            
*        DROP  R4                                                               
         EJECT                                                                  
* OVERFLOW ROUTINES                                                             
T81839   CSECT                                                                  
**********************************************************************          
* CALCPERD - CALCULATE TOTAL DOLLARS IN A PERIOD                                
**********************************************************************          
         DS    0D                                                               
CALCPERD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   CALCULATE $/DAY:                                                            
*        1.  CALCULATE DAYS IN FLIGHT                                           
*        2.  DIVIDE $$ BY DAYS IN FLIGHT                                        
*        3.    SAVE RESULT IN DOLRSDAY                                          
*                                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK+6)                              
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+12)                           
*                                  WORK+6  = FLIGHT START DATE                  
*                                  WORK+12 = FLIGHT END DATE                    
         GOTO1 =V(PERVERT),DMCB,WORK+6,WORK+12                                  
         ZICM  R1,DMCB+8,2         P3 = # DAYS INCLUSIVE IN FLIGHT              
         ST    R1,FLTDAYS          SAVE # DAYS IN FLIGHT                        
*                                                                               
         XC    FULL,FULL           ZERO OR LESS, EXIT WITH ZERO PERTOT          
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   CPDX                NO SAR ELEM, EXIT W/ 0 PERTOT                
         USING RSARXEL,R6                                                       
*                                                                               
* GET STADOLRS CAUSE IT MIGHT BE BLANK IF USER DOESN'T REQUEST TO SEE           
* STATION BUDGET ON REPORT                                                      
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         OC    RSARXBGT,RSARXBGT   IS MARKET BUDGET ZERO?                       
         BZ    CPD10               YES, MAKES STATION DOLLARS ZERO              
         IC    R1,RSARXSHG         GET SHARE GOAL                               
         M     R0,RSARXBGT         MULTIPLY BY MARKET BUDGET                    
         D     R0,=F'100'          DIVIDE BY 100 FOR PERCENTAGE                 
*                                                                               
         CLI   19(R3),C'M'         DISPLAY MARKET TOTALS?                       
         BNE   CPD10               NO                                           
         ICM   R1,15,RSARXBGT      YES - JUST USE MARKET BUDGET                 
*                                                                               
CPD10    DS    0H                                                               
         XC    STADOLRS,STADOLRS                                                
         ST    R1,STADOLRS         SAVE STATION BUDGET                          
*                                                                               
         L     R1,FLTDAYS          GET # DAYS IN FLIGHT                         
         SR    RE,RE               CLEAR FOR DIVIDE                             
         C     RE,STADOLRS         STATION BUD ZERO?                            
         BNL   CPDX                STATION NOT POS, EXIT                        
*                                     ROUNDING                                  
         L     RF,STADOLRS         GET DOLLARS FOR ORDER                        
         M     RE,=F'200'          DECIMAL ALIGNMENT + DOUBLE FOR               
         AR    RF,R1               HALF-ADD FOR ROUNDING                        
         DR    RE,R1               DIVIDE BY # DAYS                             
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,DOLRSDAY         SAVE DOLLARS/DAY                             
         DROP  R6                                                               
*                                                                               
*   GET MONTHS IN PERIOD - CONVERT TO YYMMDD FORM                               
*                                                                               
         XC    PERVDTS,PERVDTS                                                  
         XC    PERVDTE,PERVDTE                                                  
         GOTO1 DATCON,DMCB,(3,TYMDST),(0,PERVDTS)                               
         GOTO1 DATCON,DMCB,(3,TYMDND),(0,PERVDTE)                               
*                                                                               
         BAS   RE,CALCMON          CALCULATE DOLS FOR MONTH(S)                  
         L     R1,FULL             $/MONTH IN FULL                              
         ST    R1,PERTOT                                                        
CPDX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* CALCMON - CALCULATE DOLLARS IN A MONTH                                        
**********************************************************************          
CALCMON  NTR1                                                                   
*    FIRST, THE ARRAY MONTH IS CHECKED TO ENSURE THAT IT IS WITHIN              
*        THE FLIGHT DATES OF THE CONTRACT.  IF IT IS NOT, THE                   
*        RETURN FIELD IS SENT BACK AS ZERO.                                     
*    IF WITHIN FLIGHT:                                                          
*        1.  TOTAL LOSS / # DAYS IN FLIGHT -> $LOSS/DAY                         
*        2.  # DAYS IN FLIGHT IN BDCAST MONTH * $LOSS/DAY ->                    
*            $LOSS/BDCAST MONTH                                                 
*        3.  VALUE IS RETURNED IN DOLRSDAY                                      
*        4.  R4 -> ARRAY ADDRESS                                                
*                                                                               
         XC    FULL,FULL           CLEAR RETURN FIELD                           
         XC    WORK,WORK                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
*                                                                               
*        GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)   GET FLIGHT START/            
*        GOTO1 GETBROAD,DMCB,(C'1',WORK),WORK+6,GETDAY,ADDAY  END DATES         
*        GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
*        MVC   FLIGHTST,FULL                                                    
*        GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
*        GOTO1 GETBROAD,DMCB,(C'1',WORK),WORK+6,GETDAY,ADDAY                    
*        GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
*        MVC   FLIGHTND,FULL                                                    
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
*   TEST ARRAY DATE FOR INCLUSION WITHIN CONTRACT FLIGHT DATES                  
*                                                                               
         XC    FULL,FULL           CLEAR PERIOD TOTAL                           
         CLC   TYMDND,RCONDATE                                                  
*        CLC   TYMDND,FLIGHTST                                                  
*                                  PER END VS CON FLIGHT START                  
         BL    LOCA0100            PERIOD EARLIER: EXIT W/$0                    
         CLC   TYMDST,RCONDATE+3                                                
*        CLC   TYMDST,FLIGHTND                                                  
*                                  PERIOD STRT VS CON FLIGHT END                
         BH    LOCA0100            PERIOD LATER: EXIT W/$0                      
*                                                                               
*   CHECK IF FLIGHT START SUPERCEDES BCST START, OR                             
*            FLIGHT END   SUPERCEDES BCST END                                   
*                                                                               
         CLC   RCONDATE(3),TYMDST                                               
*                                  FLIGHT START VS BCST START                   
         BNH   LOCA0020            START NOT AFTER BCST START: LEAVE            
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,PERVDTS)                             
*                                  START AFTER BCST START:                      
*                                     REPLACE WITH FLIGHT START                 
LOCA0020 EQU   *                                                                
         CLC   RCONDATE+3(3),TYMDND                                             
*                                  FLIGHT END   VS BCST END                     
         BNL   LOCA0040            END  NOT BEFORE BCST END: LEAVE              
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,PERVDTE)                           
*                                  END BEFORE BCST END:                         
*                                     REPLACE WITH FLIGHT END                   
LOCA0040 EQU   *                                                                
*                                  WORK+6  = BCST MONTH OR FLIGHT               
*                                     START DATE (WHICHEVER IS LATER)           
*                                  WORK+12 = BCST MONTH OR FLIGHT               
*                                     END DATE (WHICHEVER IS EARLIER)           
*                                                                               
         GOTO1 =V(PERVERT),DMCB,PERVDTS,PERVDTE                                 
         ZICM  RF,DMCB+8,2         P3 = # DAYS INCLUSIVE IN MONTH               
         ST    RF,PERDAYS          SAVE # DAYS INCLUSIVE IN MONTH               
*                                                                               
         L     RF,DOLRSDAY         GET $/DAY                                    
         SR    RE,RE               CLEAR FOR MULTIPLICATION                     
         M     RE,PERDAYS          $LOSS/DAY * DAYS IN PERIOD                   
*                                     ->  $LOSS IN PERIOD                       
         AH    RF,=H'50'           ROUND PENNIES                                
         D     RE,=F'100'          GET RID OF PENNIES FROM DOLRSDAY             
         ST    RF,FULL             SAVE DOLLARS IN MONTH                        
LOCA0100 EQU   *                                                                
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* REPIOTBL INITIALIZED IN RESFM3A                                               
**********************************************************************          
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
         LA    R5,REPIOTBL         FINISH INITIALIZING REPIO                    
         USING REPIOD,R5                                                        
         MVI   RIPSKIP,C'Y'        DON'T NEED PREVIOUS $                        
*                                  SO SET REPIO FLAG ACCORDINGLY                
*                                                                               
* GET DATE OF MONDAY PREVIOUS TO TODAY'S DATE                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK)    TODAY'S DATE-YYMMDD           
         GOTO1 GETDAY,DMCB,WORK,WORK+6            RETURNS DAY OF WEEK           
         CLI   DMCB+4,X'40'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,DMCB           R2 =TODAY=1-7(DAY OF WEEK)                     
         BCTR  R2,0              NO OF DAYS DIFF BETWEEN TODAY-MONDAY           
         MH    R2,=H'-1'           MAKE NUM OF DAYS DIFF NEGATIVE               
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         PRINT NOGEN                                                            
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,MONDAYDT)                              
*                                                                               
                                                                                
* - INITIALIZE TSAROFF                                                          
         PRINT GEN                                                              
         GOTO1 =V(LOADER),DMCB,=CL8'T00A7D',0    TSAROFF                        
         PRINT NOGEN                                                            
         MVC   ATSAROFF,DMCB+4                                                  
         OC    ATSAROFF,ATSAROFF                                                
         BNO   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R0,TSARBUFL         TSAR BUFF SIZE + AFFL TABLE                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
***      L     RF,TSARBUFL         CALC DISPLACEMENT TO AFFL TABLE              
***      AR    R1,RF                                                            
***      ST    R1,AFFTABLE         SAVE A(AFFIL TABLE)                          
***      XC    0(64,R1),0(R1)      CLEAR FIRST PART OF TABLE                    
*                                                                               
         XC    TSAREA,TSAREA                                                    
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL     LOAD L(TSAR TABLE)                           
         LA    R0,MYTSKEYL                                                      
         STC   R0,TSKEYL                                                        
         LA    R0,MYTSRECL                                                      
         STH   R0,TSRECL                                                        
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
***      MVC   WORK(4),TSARBUFF                                                 
***      GOTO1 =V(PRNTBL),DMCB,=C'GTM',WORK,C'DUMP',4,=C'1D'                    
*                                                                               
         CLI   TSERRS,0                                                         
         BE    INIT6                                                            
         DC    H'0'                                                             
*                                                                               
INIT6    DS    0H                                                               
         L     RE,=A(OFFLIST)                                                   
         A     R2,RELO             SET A(OFFICE LIST)                           
         LA    RF,LOFFLIST                                                      
         XCEF                                                                   
         L     R2,=A(OFFLIST)                                                   
         A     R2,RELO             GET OFFICE LIST                              
****     XC    0(L'OFFLIST,R2),0(R2)                                            
         CLI   FLTREG,X'40'        ARE WE FILTERING ON REGION                   
         BNH   INIT10                                                           
         LA    R3,50                                                            
         LA    R6,KEY                                                           
         USING ROFFREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     INIT8                                                            
INIT7    GOTO1 SEQ                                                              
                                                                                
INIT8    CLC   KEY(25),KEYSAVE                                                  
         BNE   INIT10              THAT'S ALL                                   
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLC   FLTREG,ROFFREG      REGIONS MATCH                                
         BNE   INIT7               NO                                           
         MVC   0(2,R2),ROFFKOFF    YES/SAVE OFFICE IN LIST                      
         LA    R2,2(R2)            BUMP LIST                                    
         BCT   R3,INIT7                                                         
         DC    H'0'                MAXED OUT                                    
                                                                                
INIT10   DS    0H                                                               
         XC    CONTYPEX,CONTYPEX                                                
         LA    R4,50               50=MAX NUMBER OF CON TYPES                   
         MVC   AIO,AIO2                                                         
         LA    R2,CONTYPEX         LOAD CON TYPE TABLE                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTYKEY,R6                                                       
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,RIPREP                                                  
         GOTO1 HIGH                                                             
INIT12   CLC   KEY(26),KEYSAVE                                                  
         BNE   INIT19                                                           
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R6,RCTYELEM                                                      
INIT13   CLI   0(R6),X'10'                                                      
         BE    INIT14                                                           
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    INIT18                                                           
         AR    R6,R1                                                            
         B     INIT13                                                           
         USING RCTYFEL,R6                                                       
INIT14   TM    RCTYFPRC,X'20'      CONTRACT TYPE BIT ON                         
         BNO   INIT18                                                           
         MVC   0(1,R2),KEY+26      SET CONTRACT TYPE                            
         LA    R2,1(R2)                                                         
INIT17   BCT   R4,INIT18                                                        
         DC    H'0'                NEED TO EXPAND TABLE                         
INIT18   GOTO1 SEQ                                                              
         B     INIT12                                                           
*                                                                               
INIT19   MVC   AIO,AIO1                                                         
*                                                                               
INIT20   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXPECTS DEMO CODE IN WORK                                                     
* RETURNS DEMO NAME IN WORK                                                     
***********************************************************************         
         DS    0D                                                               
XGETDEM  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         NI    WORK,X'FF'-X'40'    CLEAR PRIMARY DEMO FLAGX'40'                 
         XC    WORK+10(15),WORK+10       CLEAR OUTPUT AREA                      
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         CLI   RCONKSTA+4,C'A'                                                  
         BE    *+16                                                             
         CLI   RCONKSTA+4,C'F'                                                  
         BE    *+8                                                              
         MVI   DBSELMED,C'T'                                                    
                                                                                
         L     RF,CALLOV                GET DEMOCON                             
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,WORK),(6,WORK+10),(0,DBLOCK),0                      
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(11),WORK+10                                                 
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 3                                                                
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
*   SUBROUTINE TO FLOAT PERIOD DATES INTO SCRTAB FOR HEADER                     
****************************************************************                
FLTPER   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ATWA                                                          
         USING CONHEADH-64,R5                                                   
*                                                                               
         LA    R3,PER1TAB          POINT PERIODS IN SCRTAB                      
         LA    R6,ARRONEH          POINT TO FIRST PERIOD                        
*                                                                               
FP10     DS    0H                                                               
         LA    R1,PER5TAB          PAST FIFTH PERIOD?                           
         CR    R3,R1                                                            
         BH    FPX                 DONE                                         
         CLI   5(R6),0             INPUT PERIOD BLANK?                          
         BE    FPX                 YES, SHOULD EXIT?                            
* RIGHT ALIGN 'PER1' ETC. IF PER. INPUT BLANK?                                  
*                                                                               
*************************************************                               
* FIND NUM OF DATES INPUT & A LENGTH                                            
         ZIC   R1,5(R6)            GET INPUT LENGTH                             
         LA    R4,8(R6)            POINT TO BEGIN OF PERIOD INPUT               
ND10     CLI   0(R4),C'-'          DASH?                                        
         BE    ND20                TWO DATES, PNT TO SECOND & GET LEN           
         LA    R4,1(R4)                                                         
         BCT   R1,ND10             CHECK NEXT INPUT BYTE                        
*                                                                               
* ONE DATE                                                                      
         LR    R1,R4               GET CURRENT POSITION                         
         LA    R0,8(R6)            GET BEGIN OF INPUT POS                       
         SR    R1,R0               GET DIFFERENCE                               
*        LA    R1,1(R1)            GET LENGTH                                   
         STC   R1,BYTE             STORE LENGTH                                 
*                                                                               
         LA    R4,8(R6)            BEG OF INPUT                                 
         B     NDX                 GET OUT                                      
*                                                                               
ND20     DS    0H                                                               
         LA    R4,1(R4)            ADVANCE ONE BEYOND DASH TO SEC DTE           
         LR    R0,R4               GET BEGIN OF SEC DATE                        
*                                                                               
ND25     LA    R4,1(R4)                                                         
         BCT   R1,ND25             CHECK NEXT INPUT BYTE                        
*                                                                               
* TWO DATES                                                                     
         LR    R1,R4               GET CURRENT POSITION                         
         BCTR  R1,0                DECREM TO GET CORRECT POS                    
         SR    R1,R0               GET DIFFERENCE,                              
         STC   R1,BYTE             STORE LENGTH                                 
*                                                                               
         LR    R4,R0               BEG OF SEC DATE INPUT                        
         MVI   PRANGFLG,C'Y'       IT'S A PERIOD RANGE                          
NDX      DS    0H                                                               
*                                                                               
*******************************************                                     
         XC    0(16,R3),0(R3)       CLEAR TEXT FIELD FOR HEADER                 
*                                                                               
         LR    R2,R3               POINT TO TABLE                               
         ZIC   R0,BYTE             GET LENGTH OF DATE                           
*        LA    R1,8                MAX POSSIBLE OUTPUT                          
         ZIC   R1,18(R3)           GET REQ LEN                                  
         CR    R1,R0                                                            
         BL    *+8                 NO RIGHT ALIGN IF INP LEN > REQ LEN          
         SR    R1,R0               GET DISPL INTO SCRTAB                        
         AR    R2,R1               RIGHT ALIGN THE MVC                          
*                                                                               
         ZIC   R1,18(R3)                                                        
         ZIC   R0,BYTE             GET INPUT LEN                                
         CR    R1,R0               REQ LEN > INPUT LEN                          
         BNH   *+6                 NO, SKIP                                     
         LR    R1,R0               REQ LEN TOO BIG TRUNC TO INPUT LEN           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R4)       MOVE DATE INTO SCRTAB                        
         OC    0(8,R3),SPACES      LEFT SPACE PAD                               
*                                                                               
FP50     DS    0H                                                               
         LA    R3,TABENTQ(R3)                                                   
         ZIC   R1,0(R6)                                                         
         AR    R6,R1               POINT TO NEXT FLD                            
         ZIC   R1,0(R6)                                                         
         AR    R6,R1               POINT TO NEXT PERIOD                         
         B     FP10                                                             
*                                                                               
FPX      B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  SUBROUTINE TO FIND NUMBER OF DATES AND THE LENGTH OF ONE FOR HEADER          
*  -- AT EXIT: R4 SET TO CORRECT DATE AND LEN IS IN BYTE                        
***********************************************************************         
NMDTELN  DS    0H                                                               
*                                                                               
**********************************************************************          
*                                                                               
       ++INCLUDE REPIOBLK                                                       
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REGENACT                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRO                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
       ++INCLUDE REGENOFF2                                                      
         EJECT                                                                  
       ++INCLUDE REGENCTY                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
*        PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDFD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*        PRINT ON                                                               
       ++INCLUDE RESFM38WRK                                                     
         ORG   MYWORKND                                                         
ATSAROFF DS    F                                                                
*                                                                               
THISOFF  DS    CL2                 OFFICE FOR HEADLINE PRINTING                 
THISTEAM DS    CL2                 TEAM FO3 HEADLINE PRINTING                   
THISLSNM DS    CL24                CODE/NAME FOR HEADLINES                      
THISSTA  DS    CL5                 STATION FOR HEADLINES                        
THISMRKT DS    CL20                MARKET NAME                                  
MONDAYDT DS    CL3                 PREVIOUS MONDAY'S DATE                       
MIDSV    DS    CL80                                                             
CONTYPEX DS    CL50                CONTRACT TYPES BASED ON REGENCTY             
*KNUMFLG DS    CL1                 IS K # IN SORTKEY                            
EXDFLG   DS    CL1                 FLAG FOR DATE EXCEPTIONS                     
EXPFLG   DS    CL1                 FLAG FOR PRINT EXCEPTIONS                    
EXNFLG   DS    CL1                 FLAG FOR EXCEPTIONS, NUMERIC                 
COLBKFLG DS    XL1                 FLAG - LAST COLUMN # TO CHECK                
PGBKFLG  DS    CL1                 FLAG - PAGE BREAK?                           
PRANGFLG DS    CL1                 FLAG - PERIOD RANGES INPUT?                  
STATCOL  DS    XL1                 STATUS COL# FLAG                             
LASTSTAT DS    XL1                 STATUS OF LAST RECORD                        
LASTBK   DS    F                   ADDRESS OF LAST BLOCK OF VALID TOTS          
*                                  - FOR KEY FIELD BREAKS                       
ADRWIDE  DS    A                   ADDRESS OF DDWIDE                            
*                                                                               
FLIGHTST DS    H                                                                
FLIGHTND DS    H                                                                
PERTOT   DS    F                   PERIOD DOLRS TOTAL                           
DOLRSDAY DS    F                   $/DAY                                        
STADOLRS DS    F                   STATION BUD DOLLARS                          
FLTDAYS  DS    F                   # DAYS IN FLIGHT                             
PERDAYS  DS    F                   # DAYS IN PERIOD                             
PERVDTS  DS    CL6                 PERVERT PERIOD START DATE                    
PERVDTE  DS    CL6                 PERVERT PERIOD END DATE                      
SEQKEY   DS    CL(L'KEY)                                                        
*MDAY    DS    CL3                 FOR GETDAY                                   
*                                                                               
* 8 SETS OF 7 TOTALS                                                            
* 8 SETS FOR EACH POSSIBLE KEY BREAK                                            
* 7 TOTALS FOR PERIOD TOTALS AND BUDGET TOTALS                                  
* - ORDER OF TOTS IN 7 BYT BLOCK: PER1-PER5, THEN STA BUD, THEN MKT BUD         
*                                                                               
STATTOT  DS    7F                  9NTH BLOCK FOR STATUS, LIKE SRTKEY           
DOLTOTS  DS    0CL224              8 BLOCKS OF 7 FULL WORDS (8*7*4)             
         DS    7F                  BREAK TOTALS                                 
KYBKLN   EQU   *-DOLTOTS           LENGTH OF TOTALS FOR ONE KEY BREAK           
         DS    7F                                                               
         DS    7F                                                               
         DS    7F                                                               
         DS    7F                                                               
         DS    7F                                                               
         DS    7F                                                               
LASTOT   DS    7F                                                               
         EJECT                                                                  
*                                                                               
PREVSPER DS    CL20                                                             
PREVIOUS DS    CL(L'SRTKEY)        COPY OF SRTKEY                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'249RESFM39   02/24/15'                                      
         END                                                                    
