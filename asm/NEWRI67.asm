*          DATA SET NEWRI67    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T32067A                                                                  
*INCLUDE GETBROAD                                                               
         TITLE 'NETPAK CPM PUP TRANSFER'                                        
T32067   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE67*,RA,RR=RE                                               
         LA    RC,2048(RA)           RA,RB,RC BASE REGISTERS                    
         LA    RC,2048(RC)                                                      
         USING T32067+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING GEND,R9                                                          
         L     R8,ATWA                                                          
         USING T320FFD,R8                                                       
         LA    R6,ASYSD                                                         
         USING NETSYSD,R6                                                       
         L     R7,ANETWS2          R7 = ANETWS2 FIXED WORKING STORAGE           
         A     R7,=F'250'                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM           ANETWS1 = CLIENT RECORD                      
         L     R1,=A(MYIO)                                                      
         ST    R1,AMYIO                                                         
         SPACE 2                                                                
         CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,EDITMOD                                                       
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,PRINTMOD                                                      
XIT      XIT1                                                                   
         EJECT                                                                  
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0                                                        
         B     EDT10                                                            
*                                                                               
VALCLI   NTR1                                                                   
         MVI   FTERMFLG,0          REQUIRED                                     
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,,ANETWS1                                              
         B     XIT                                                              
*                                                                               
VALPROD  NTR1                                                                   
         LA    R2,WORK+20          FUDGE  PROD POL                              
         MVI   0(R2),20                                                         
         MVI   5(R2),3                                                          
         MVC   8(3,R2),=C'POL'                                                  
         NETGO NVPRDALL,DMCB                                                    
         B     XIT                                                              
*                                                                               
* - EXPECTS R2 TO POINT TO EST HEADER                                           
VALEST   NTR1                      VALIDATE ESTIMATE                            
         NETGO NVEST,DMCB                                                       
         B     XIT                                                              
*                                                                               
VALNET   NTR1                      VALIDATE NETWORK                             
         MVI   FTERMFLG,0          REQUIRED                                     
         LA    R2,SPLNETH                                                       
         NETGO NVNET,DMCB                                                       
         L     R2,NBAIO                                                         
         USING STAREC,R2                                                        
         PACK  DUB,SMKT            GET 2 BYTE MKT NUMBER                        
         CVB   R1,DUB                                                           
         STH   R1,MKTNUM                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
EDT10    LA    R2,SPLEST1H            POINT TO 1ST DATA LINE                    
         CLI   5(R2),0                MUST HAVE AT LEAST 1 ESTIMATE             
         BE    ERRMISS                                                          
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT1H         VALIDATE PLAN DATA OF LINE 1                 
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK1H         VALIDATE PACKAGE DATA OF LINE 1              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST2H            POINT TO 2ND DATA LINE                    
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT2          VALIDATE PLAN DATA OF LINE 2                 
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK2H         VALIDATE PACKAGE DATA OF LINE 2              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST3H            POINT TO 3D DATA LINE                     
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         EJECT                                                                  
*                                                                               
         LA    R2,SPLDPT3          VALIDATE PLAN DATA OF LINE 3                 
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK3H         VALIDATE PACKAGE DATA OF LINE 3              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST4H            POINT TO 4TH DATA LINE                    
         CLI   5(R2),0                                                          
         BNE   EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT4H         VALIDATE PLAN DATA OF LINE 4                 
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK4H         VALIDATE PACKAGE DATA OF LINE 4              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST5H            POINT TO 5TH DATA LINE                    
         CLI   5(R2),0                MUST HAVE AT LEAST 1 ESTIMATE             
         BE    EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT5H         VALIDATE PLAN DATA                           
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK1H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST6H            POINT TO 6TH DATA LINE                    
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT6          VALIDATE PLAN DATA                           
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK6H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST7H            POINT TO 7TH DATA LINE                    
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT7H         VALIDATE PLAN DATA                           
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         LA    R2,SPLPAK7H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST7H            POINT TO 8TH DATA LINE                    
         CLI   5(R2),0                                                          
         BNE   EDT20                                                            
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT8H         VALIDATE PLAN DATA                           
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK8H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
EDT20    DS    0H                                                               
         LA    R2,SPLPCPMH         PACKAGE CPM                                  
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    EDT25                                                            
         MVI   BYTE,1              USE BYTE FOR PKG CPM REQ SWITCH              
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,SPLPCPM),(R3)                                    
         CLI   0(R1),0                                                          
         BNE   ERREX                                                            
         MVC   PACKCPM,4(R1)                                                    
*                                                                               
EDT25    LA    R2,SPLDEMOH         DEMO CPM                                     
         CLI   5(R2),0             IF NO DEMO CPM REQUEST                       
         BNE   EDT30                                                            
         CLI   BYTE,0              MUST HAVE PACKAGE CPM REQUEST                
         BE    ERRMISS                                                          
         MVI   BYTE,0                                                           
         B     EDT40                                                            
EDT30    GOTO1 NBCALLOV,DMCB,0,X'D9000AD9'           (DEMOVAL)                  
         L     RF,DMCB                                                          
         L     R1,AMYIO                                                         
         USING DBLOCK,R1                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'T'                                                    
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'S'                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,(1,(R2)),(1,WORK)                                      
         CLI   DMCB+4,0                                                         
         BE    ERREX                                                            
         MVC   TARGDEM,WORK                                                     
*                                                                               
         LA    R2,SPLDCPMH            *TARGET CPM FOR DEMO                      
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,SPLDCPM),(R3)                                    
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         MVC   DEMCPM,4(R1)                                                     
*                                                                               
         LA    R2,SPLINTGH            * INTEGRATION                             
         CLI   5(R2),0                                                          
         BE    EDT40                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,SPLINTG),(R3)                                    
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         MVC   INTEGR,4(R1)                                                     
*                                                                               
EDT40    LA    R2,SPLUNIVH             * UNIVERSE                               
         NETGO NVGETFLD,DMCB                                                    
         LTR   R1,R1               ANY INPUT                                    
         BZ    EDT45                                                            
         LTR   R0,R0               TEST FOR NUMERIC INPUT                       
         BE    ERRINV                                                           
         CVD   R0,DUB                                                           
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         MVC   PAKUNCD,DUB+5       TO ISOLATE UNIVERSE CODE PWO.                
         SPACE                                                                  
         XC    KEY,KEY             NOW SEE IF UNIVERSE RECORD IS THERE          
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         MVC   NUNKTYP,=X'0D22'                                                 
         MVC   NUNKAGY,NBSELAGY                                                 
         MVI   NUNKTYPE,1          UNIVERSE CODE                                
         MVC   NUNKCODE,PAKUNCD                                                 
         GOTO1 SPTHI                                                            
         CLC   KEY(L'NUNKEY),KEYSAVE    TEST IF FOUND                           
         BE    EDT45                                                            
         MVI   ERROR,53            RECORD NOT FOUND                             
         GOTO1 ERREX                                                            
         DROP  R3                                                               
*                                                                               
EDT45    LA    R2,SPLMALCH         MASTER ALLOCATION                            
         CLI   5(R2),0                                                          
         BE    EDTX                                                             
         NETGO NVGETFLD,DMCB                                                    
         CLI   FLDH+5,3            PRODUCT CODE IS 3 CHARS OR LESS              
         BH    ERRINV                                                           
         CLC   =C'POL',FLD         TEST FOR POL                                 
         BE    ERRINV              YES-DO NOT ALLOW IT                          
*                                                                               
         LA    R0,220                                                           
         L     RE,ANETWS1          ANETWS1 CONTAINS CLINET RECORD               
         USING CLTHDR,RE                                                        
         LA    RE,CLIST                                                         
EDT47    OC    0(4,RE),0(RE)       TEST FOR E-O-L                               
         BZ    ERREX                                                            
         CLC   FLD(3),0(RE)                                                     
         BE    EDT50               FOUND PRODUCT CODE                           
         LA    RE,4(RE)                                                         
         BCT   R0,EDT47                                                         
EDT50    MVC   MASTPRD,3(RE)      EXTRACT PRODUCT NUMBER                        
*                                                                               
         LA    R4,ESTLIST          NOW VALIDATE BRAND ESTIMATE                  
* - READ THROUGH TABLE OF REQUESTED ESTIMATES                                   
* - EACH ESTIMATE + MASTER ALLOCATION PRODUCT MUST HAVE RECORD                  
EDT55    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
         MVI   EKEYTYPE,X'00'                                                   
         MVC   EKEYAM,NBSELAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,FLD         PRODUCT CODE                                 
         MVC   EKEYEST,0(R4)       SET ESTIMATE FROM LIST                       
         BAS   RE,SPTHI                                                         
         CLC   KEY(13),KEYSAVE      TEST IF ESTIMATE FOUND                      
         BE    EDT60                YES                                         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'ERROR - NO ESTIMATE FOR BRAND'                    
         GOTO1 ERREX2                                                           
EDT60    LA    R4,1(R4)            BUMP LIST                                    
         CLI   0(R4),0                                                          
         BE    EDTX                WE'RE THROUGH                                
         B     EDT55               GET NEXT ESTIMATE FROM LIST                  
*                                                                               
EDTX     B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*        READ PLAN RECORD & EXTRACT NEEDED INFO                                 
*                                                                               
VALPLAN  NTR1                                                                   
* - DAYPART                                                                     
         DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0            TEST FOR ANY INPUT                            
         BNE   *+6                                                              
         DC    H'0'               SHOULD NEVER STOP HERE                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'20'                                                        
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+5(4),NBACTNET                                                
         MVC   KEY+10(1),8(R2)                                                  
* - PLAN                                                                        
         DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R1,R2                                                            
         CLI   5(R2),0            TEST FOR ANY INPUT                            
         BE    ERREX                                                            
         MVC   KEY+11(4),=C'    '                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCPCDE          BUILD KEY - READ RECORD                      
         GOTO1 UNTHI                                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   ERREX                                                            
         MVC   RECIO,NBAIO                                                      
         GOTO1 GETUNT                                                           
         L     R3,NBAIO                                                         
         USING NPLRECD,R3                                                       
         CLI   NPLNPRFL,C'C'                                                    
         BE    *+12                                                             
         CLI   NPLNPRFL,C'B'                                                    
         BNE   *+10                                                             
         MVC   NETPROF+3(1),NPLNPRFL  OVERRIDE TO PROFILE                       
         MVC   TRGDEMO,NPLNDEMS+2                                               
         MVC   LENS,NPLNLENS                                                    
         MVC   PLAN,NPLKPLAN                                                    
         MVC   PLNDPT,NPLKDPT                                                   
         MVC   PLHAVE,NPLNPERT                                                  
         MVC   PLNDEMS,NPLNDEMS                                                 
         OC    NPLNPERT,NPLNPERT                                                
         BNZ   *+10                                                             
         MVC   PLHAVE,NPLNHAVE                                                  
         EJECT                                                                  
* - QUARTER                                                                     
         DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R1,R2                                                            
         CLI   5(R2),0            TEST FOR ANY INPUT                            
         BE    ERREX                                                            
         GOTO1 SCANNER,DMCB,0(R2),(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERREX                                                            
         CLI   WORK,2                                                           
         BNE   ERREX                                                            
         CLI   WORK+12,C'Q'                                                     
         BNE   ERREX                                                            
         CLI   WORK+13,C'1'                                                     
         BL    ERREX                                                            
         SPACE                                                                  
* GET BROADCAST DATES (START/END)                                               
* QUARTER 3 ENDS SUNDAY OF 2ND WEEK OF SEP.                                     
* QUARTER 4 STARTS MONDAY OF 3RD WEEK OF SEP.                                   
* START QUARTER SET UP START AND END DATES                                      
         XC    HALF,HALF           USE HALY AS SWITCHES FOR 3/4/QTR             
         ZIC   RE,NPLNYEAR                                                      
         STC   RE,PLANYR                                                        
         STC   RE,DUB              SET UP BOTH START/END DATES                  
         STC   RE,DUB+3                                                         
         MVI   DUB+2,1             DEFAULT START DAY                            
         MVI   DUB+5,X'1C'         DEFAULT END DAY                              
         CLI   WORK+13,C'4'                                                     
         BH    ERREX                                                            
         BNE   AC100                                                            
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
         STC   RE,DUB+3                                                         
         MVI   HALF,C'Y'                                                        
         B     AC120                                                            
*                                                                               
AC100    CLI   WORK+13,C'3'                                                     
         BNE   AC120                                                            
         MVI   HALF+1,C'Y'                                                      
*                                                                               
AC120    ZIC   RF,WORK+13                                                       
         SH    RF,=H'240'                                                       
         LA    R1,SVQTBLS-1(RF)                                                 
         MVI   0(R1),C'Y'                                                       
         MH    RF,=H'2'                                                         
         LA    RF,QTBL-2(RF)                                                    
         MVC   DUB+1(1),0(RF)                                                   
         MVC   DUB+4(1),1(RF)                                                   
*                                                                               
* TEST FOR MORE THAN 1 QUARTER                                                  
AC200    CLI   WORK+1,0                                                         
         BE    AC240                                                            
         CLI   WORK+1,2                                                         
         BNE   ERREX                                                            
         CLC   WORK+13(1),WORK+23                                               
         BE    AC240                                                            
         CLI   WORK+22,C'Q'                                                     
         BNE   ERREX                                                            
         CLI   WORK+23,C'1'                                                     
         BL    ERREX                                                            
         MVI   HALF+1,C'N'                                                      
         CLI   WORK+23,C'3'                                                     
         BH    ERREX                                                            
         BNE   *+8                                                              
         MVI   HALF+1,C'Y'                                                      
         MVC   DUB+3(1),NPLNYEAR                                                
         ZIC   RF,WORK+23                                                       
         SH    RF,=H'240'                                                       
         LA    R4,SVQTBLS-1(RF)                                                 
         MVI   0(R4),C'Y'                                                       
         MH    RF,=H'2'                                                         
         LA    RF,QTBL-2(RF)                                                    
         MVC   DUB+4(1),1(RF)                                                   
         CLI   SVQTBL4,C'Y'                                                     
         BNE   *+8                                                              
         LA    R1,SVQTBL1-1                                                     
         LA    R0,2                                                             
AC220    LA    R1,1(R1)                                                         
         CR    R1,R4                                                            
         BNL   AC240                                                            
         MVI   0(R1),C'Y'                                                       
         BCT   R0,AC220                                                         
*                                                                               
AC240    CLI   NETPROF+3,C'C'      ARE DATES IN CALANDER FORMAT                 
         BE    AC300                                                            
*--CALCULATE DATES ON A BROADCAST BASIS                                         
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTO1 DATCON,(R1),(3,DUB+3),(0,WORK+6)                                 
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,GETDAY,ADDAY,        X        
               RR=MYRELO                                                        
         CLI   HALF,C'Y'             4TH QTR                                    
         BNE   AC260                                                            
         LA    R0,14                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK+12,(R0)                                  
AC260    GOTO1 DATCON,(R1),(0,WORK+12),(3,DUB)                                  
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 =V(GETBROAD),(R1),(1,WORK+6),WORK+12,GETDAY,ADDAY,      X        
               RR=MYRELO                                                        
         CLI   HALF+1,C'Y'         3RD QTR                                      
         BNE   AC280                                                            
         LA    R0,13                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK+18,(R0)                                  
AC280    GOTO1 DATCON,(R1),(0,WORK+18),(3,DUB+3)                                
         B     AC350                                                            
         SPACE 3                                                                
*--CALCULATE DATES ON A CALANDER BASIS                                          
AC300    GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTO1 GETDAY,DMCB,WORK,THREE                                           
         ZIC   RF,0(R1)                                                         
         SR    R0,R0                                                            
         CH    RF,=H'1'                                                         
         BE    AC310                                                            
         LA    R0,8                                                             
         SR    R0,RF                                                            
*                                                                               
AC310    CLI   HALF,C'Y'             4TH QTR                                    
         BNE   AC320                                                            
         A     R0,=F'14'                                                        
AC320    GOTO1 ADDAY,DMCB,WORK,WORK+12,(R0)                                     
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,DUB)                                  
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 DATCON,DMCB,(3,DUB+3),(0,WORK+6)                                 
         GOTO1 GETDAY,DMCB,WORK+6,THREE                                         
         ZIC   RF,0(R1)                                                         
         SR    R0,R0                                                            
         LA    R0,7                                                             
         SR    R0,RF                                                            
*                                                                               
AC330    CLI   HALF+1,C'Y'           3RD QTR                                    
         BNE   AC340                                                            
         A     R0,=F'13'                                                        
AC340    GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R0)                                   
         GOTO1 DATCON,DMCB,(0,WORK+18),(3,DUB+3)                                
         SPACE                                                                  
*                                                                               
AC350    GOTO1 DATCON,DMCB,(3,DUB),(0,STRTDTE)                                  
         GOTO1 DATCON,(R1),(3,DUB+3),(0,ENDDTE)                                 
         XC    CONHEAD,CONHEAD                                                  
         CLC   NBSELSTR,ENDDTE                                                  
         BNH   AC355                                                            
         MVC   CONHEAD(37),=C'ERROR - ESTIMATE START AFTER PLAN END'            
         GOTO1 ERREX2                                                           
AC355    CLC   NBSELEND,STRTDTE                                                 
         BNL   AC357                                                            
         MVC   CONHEAD(38),=C'ERROR - ESTIMATE END BEFORE PLAN START'           
         GOTO1 ERREX2                                                           
         SPACE                                                                  
AC357    CLC   NBSELSTR,STRTDTE                                                 
         BNH   *+10                                                             
         MVC   STRTDTE,NBSELSTR                                                 
         CLC   NBSELEND,ENDDTE                                                  
         BNL   *+10                                                             
         MVC   ENDDTE,NBSELEND                                                  
         GOTO1 DATCON,(R1),(0,STRTDTE),(3,SVSTDTE)                              
         GOTO1 DATCON,(R1),(0,ENDDTE),(3,SVENDTE)                               
         EJECT                                                                  
* - LENGTH - CHECK FOR LENGTH FILTERS OR 'ALL'                                  
         ZIC   R1,0(R2)                                                         
         AR    R1,R2                                                            
         CLI   5(R2),0            TEST FOR ANY INPUT                            
         BE    ERREX                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    AC400                                                            
         GOTO1 SCANNER,DMCB,0(R2),(1,WORK),C',=,*'                              
         CLI   4(R1),0                                                          
         BE    ERREX               INVALID INPUT                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,WORK                                                        
         BZ    ERREX                                                            
         TM    WORK+2,X'80'        TEST IF NUMERIC                              
         BZ    ERREX                                                            
         MVC   THREE,WORK+12                                                    
         BAS   R3,VALLEN           CHECK INPUT LENGTH 1                         
         STC   R0,SVLEN1                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,WORK+1                                                      
         BZ    AC400                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         BZ    ERROR                                                            
         MVC   THREE,WORK+22                                                    
         BAS   R3,VALLEN           CHECK INPUT LENGTH 2                         
         STC   R0,SVLEN2                                                        
*                                                                               
* - HUT TRANSFER                                                                
AC400    DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R1,R2                                                            
         MVI   CALCHUT,C'N'                                                     
         CLI   5(R2),0            TEST FOR ANY INPUT                            
         BE    AC500                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREX                                                            
         MVI   CALCHUT,C'Y'                                                     
*                                                                               
AC500    DS    0H                                                               
         XMOD1                                                                  
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
MVCPCDE  MVC   KEY+11(0),FLD                                                    
         SPACE                                                                  
VALLEN   LR    R0,R1               SAVE DATA LENGTH                             
         LA    RE,THREE            RE=DATA POINTER                              
VL100    CLI   0(RE),C'0'          TEST FOR NUMERIC DATA                        
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,VL100                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,PACKLEN                                                       
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    ERROR                                                            
         CH    R0,=H'255'          TEST FOR MAXIMUM VALUE                       
         BH    ERROR                                                            
         BR    R3                                                               
PACKLEN  PACK  DUB,THREE(0)                                                     
         B     XIT                                                              
         EJECT                                                                  
* - VALIDATE USER ASSIGNED PACKAGE NUMBER OR GET NEXT PKG NUMBER                
VALPACK  NTR1                                                                   
         CLI   NBUSER1+11,C'Y'     SELF ASSIGN PACKAGE NUMBERS                  
         BNE   *+12                                                             
         BAS   RE,TESTPACK                                                      
         B     *+8                                                              
         BAS   RE,GETPACK                                                       
         ZIC   R1,0(R2)            BUMP TO PACKAGE NAME FIELD                   
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BE    VPERR                                                            
         MVC   PACKNAME,8(R2)      SAVE PACKAGE NAME                            
         B     VPX                                                              
VPERR    MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
VPX      XIT1                                                                   
                                                                                
* -  SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)           
TESTPACK NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,NBACTAM                                                    
         MVC   NPKCLT,NBACTCLI                                                  
         MVC   NPKNET,NBACTNET                                                  
         MVC   NPKEST,NBACTEST                                                  
         MVC   NPKPACK,PACK                                                     
         MVI   BYTE,C'Y'           PASS DELETED RECS                            
         GOTO1 UNTHI                                                            
         MVI   BYTE,0                                                           
         CLC   KEY(20),KEYSAVE                                                  
         BE    DUPPACK                                                          
         B     XIT                                                              
                                                                                
* -  SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)           
GETPACK  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,NBACTAM                                                    
         MVC   NPKCLT,NBACTCLI                                                  
         MVC   NPKNET,NBACTNET                                                  
         MVC   NPKEST,NBACTEST                                                  
         MVI   BYTE,0                                                           
         GOTO1 UNTHI                                                            
         B     GTPK5                                                            
GTPK2    GOTO1 UNTSEQ                                                           
GTPK5    CLC   KEY(NPKPACK-NPKEY),KEYSAVE                                       
         BNE   *+8                                                              
         BE    GTPK2                                                            
         LA    R4,KEYSAVE                                                       
         ZIC   R1,NPKPACK          GET HIGHEST NUMBER SO FAR                    
         LA    R1,1(R1)                                                         
         STC   R1,PACK                                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO CREATE PACKAGE RECORD FROM PLAN RECORD                             
ADDPACK  NTR1                                                                   
         L     R4,ANETWS1                                                       
         LR    RE,R4                                                            
         L     RF,=F'1250'                                                      
         XCEF                                                                   
         USING NPRECD,R4                                                        
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,NBACTAM                                                    
         MVC   NPKCLT,NBACTCLI                                                  
         MVC   NPKNET,NBACTNET                                                  
         MVC   NPKEST,NBACTEST                                                  
         MVC   NPKPACK,PACK        SET PACKAGE NUMBER IN KEY                    
*                                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
         MVI   NPKRLEN,60+27+1                                                  
*                                                                               
         L     R3,NBAIO            R3 = PLANNING RECORD                         
         USING NPLRECD,R3                                                       
*--MOVE OUT GUARANTEE FACTOR INFO                                               
         MVC   NPACKGU,NPLNADJP    NEW PACKAGE GUARANTEE                        
         MVC   NDEMOCAT,NPLNDEMS   DEMO FOR NEW DEMO ADJ (3 BYTES)              
**                                 NEW DEMO ADJ FACTOR=X'05' ELEM               
*--MOVE OUT PACKAGE RECORD INFO                                                 
         MVC   NPAKNAME,NPLNNAME                                                
         MVC   NPAKDP,NPLKDPT                                                   
         MVC   NPAKHUTA,NPLNHAVE                                                
         MVC   NPAKUNCD,NPLNUNIV                                                
         MVC   NPAKHUTS,NPLNHTSC                                                
         MVC   NPAKHTYP,NPLNHTBT                                                
         MVC   NPAKHPCT,NPLNHTPO                                                
         MVC   NPAKHUTF,NPLNHTFL                                                
         MVC   NPAKZONE,NPLNZONE                                                
         LA    R3,NPLNEL-NPLKEY(R3)                                             
PD100    CLI   0(R3),0                                                          
         BE    PD200                                                            
         CLI   0(R3),X'05'             NEW GENERAL ELEMENT                      
         BNE   PD110                                                            
         MVC   NDEMOGU,NPNADJD-NPNELEM(R3) NEW DEMO ADJ                         
         OC    NPNDEMO-NPNELEM(3,R3),NPNDEMO-NPNELEM(R3)   DEMO CODE            
         BZ    *+10                                                             
         MVC   NDEMOCAT,NPNDEMO-NPNELEM(R3)                                     
*                                                                               
PD110    CLI   0(R3),X'04'              BUDGET DOLLARS                          
         BNE   PD180                                                            
PD120    ZIC   RF,3(R3)                                                         
         LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    PD180                                                            
         CLI   SVLEN1,0                                                         
         BE    PD160                                                            
         CLC   SVLEN1,NPBSEC-NPBELD(R3)                                         
         BE    PD160                                                            
         CLI   SVLEN2,0                                                         
         BE    PD160                                                            
         CLC   SVLEN2,NPBSEC-NPBELD(R3)                                         
         BNE   PD180                                                            
PD160    ICM   RE,15,NPAKCOST                                                   
         A     RE,NPBUDGET-NPBELD(R3)                                           
         STCM  RE,15,NPAKCOST                                                   
PD180    ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     PD100                                                            
PD200    DS    0H                                                               
         DROP  R3                                                               
* - HUTS                                                                        
         OI    NPAKHUTL,X'80'      GET HUTS FORM DEMO FILE                      
         CLI   NETPROF+3,C'Y'      TEST FOR 52 WEEK OPTION                      
         BNE   *+8                 NO                                           
         OI    NPAKHUTL,X'40'      YES                                          
* - PACKAGE NAME                                                                
         MVC   NPAKNAME,PACKNAME                                                
         SPACE                                                                  
* - INTEGRATION COST                                                            
         LA    R2,SPLINTGH                                                      
         NETGO NVGETFLD                                                         
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    INTED3                                                           
         CLC   =C'TBL',FLD                                                      
         BE    INTED4                                                           
         GOTO1 SCANNER,DMCB,FLDH,(2,BLOCK),0                                    
         MVI   ERROR,INVALID                                                    
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BNZ   INT10                                                            
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
INT10    LA    R2,BLOCK                                                         
         CLI   1(R2),0                                                          
         BNE   ERROR                                                            
         ZIC   R0,0(R2)                                                         
         GOTO1 CASHVAL,DMCB,12(R2),(R0)                                         
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         MVC   NPAKINT,DMCB+4                                                   
         SH    R3,=H'1'                                                         
         BZ    INTEDX                                                           
         SPACE                                                                  
INTED2   LA    R2,32(R2)                                                        
         CLI   1(R2),0                                                          
         BNE   ERROR                                                            
         CLI   0(R2),1                                                          
         BNE   ERROR                                                            
         CLI   12(R2),C'N'         TEST FOR NON-COMM. INT.                      
         BNE   ERROR                                                            
         OI    NPAKSTAT,X'04'                                                   
         B     INTEDX                                                           
         SPACE                                                                  
INTED3   CLI   NBUSER+15,C'Y'       N0 PROFILE                                  
         BNE   INTEDX                                                           
         CLC   =C'ABC ',NBACTNET                                                
         BE    INTED4                                                           
         CLC   =C'CBS ',NBACTNET                                                
         BE    INTED4                                                           
         CLC   =C'NBC ',NBACTNET                                                
         BNE   INTEDX                                                           
INTED4   OI    NPAKCNTL,X'80'      USE INTG TABLE                               
*                                                                               
INTEDX   DS    0H                                                               
                                                                                
* - UNIVERSE CODE                                                               
         LA    R2,SPLUNIVH                                                      
         NETGO NVGETFLD                                                         
         CLI   FLDH+5,0                                                         
         BE    UCODX                                                            
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BNO   ERRINV                                                           
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         MVC   NPAKUNCD,DUB+5      TO ISOLATE UNIVERSE CODE PWO.                
         SPACE                                                                  
UCODED2  XC    KEY,KEY             NOW SEE IF UNIVERSE RECORD IS THERE          
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         MVC   NUNKTYP,=X'0D22'                                                 
         MVC   NUNKAGY,NBSELAGY                                                 
         MVI   NUNKTYPE,1          UNIVERSE CODE                                
         MVC   NUNKCODE,NPAKUNCD                                                
         GOTO1 SPTHI                                                            
         CLC   KEY(L'NUNKEY),KEYSAVE    TEST IF FOUND                           
         BNE   ERRINV                                                           
UCODX    DS    0H                                                               
* - ADD PACKAGE RECORD                                                          
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'UNTFILE ',KEY,NBAIO,DMWORK            
         B     ADDPX                                                            
         DROP  R3                                                               
ADDPX    B     XIT                                                              
         EJECT                                                                  
PRINTMOD NTR1                                                                   
         LA    R2,SPLEST1H            POINT TO 1ST DATA LINE                    
         CLI   5(R2),0                MUST HAVE AT LEAST 1 ESTIMATE             
         BE    ERRMISS                                                          
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT1H         VALIDATE PLAN DATA OF LINE 1                 
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK1H         VALIDATE PACKAGE DATA OF LINE 1              
         BAS   RE,VALPACK                                                       
         BAS   RE,ADDPACK          CREATE/ADD PACKAGE REC FROM PLAN             
         BAS   RE,ASSREC           CREATE/ADD UNITS FROM ASSIGNMENT REC         
*                                                                               
         LA    R2,SPLEST2H            POINT TO 2ND DATA LINE                    
         CLI   5(R2),0                                                          
         BE    PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT2          VALIDATE PLAN DATA OF LINE 2                 
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK2H         VALIDATE PACKAGE DATA OF LINE 2              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST3H            POINT TO 3D DATA LINE                     
         CLI   5(R2),0                                                          
         BE    PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         EJECT                                                                  
*                                                                               
         LA    R2,SPLDPT3          VALIDATE PLAN DATA OF LINE 3                 
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK3H         VALIDATE PACKAGE DATA OF LINE 3              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST4H            POINT TO 4TH DATA LINE                    
         CLI   5(R2),0                                                          
         BNE   PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT4H         VALIDATE PLAN DATA OF LINE 4                 
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK4H         VALIDATE PACKAGE DATA OF LINE 4              
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST5H            POINT TO 5TH DATA LINE                    
         CLI   5(R2),0                MUST HAVE AT LEAST 1 ESTIMATE             
         BE    PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT5H         VALIDATE PLAN DATA                           
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK1H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST6H            POINT TO 6TH DATA LINE                    
         CLI   5(R2),0                                                          
         BE    PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT6          VALIDATE PLAN DATA                           
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK6H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST7H            POINT TO 7TH DATA LINE                    
         CLI   5(R2),0                                                          
         BE    PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT7H         VALIDATE PLAN DATA                           
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK7H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
         LA    R2,SPLEST7H            POINT TO 8TH DATA LINE                    
         CLI   5(R2),0                                                          
         BNE   PRTX                                                             
         BAS   RE,VALCLI           CLIENT                                       
         BAS   RE,VALPROD          PRODUCT                                      
         BAS   RE,VALEST           ESTIMATE                                     
         BAS   RE,VALNET           NETWORK                                      
         SPACE                                                                  
         LA    R2,SPLDPT8H         VALIDATE PLAN DATA                           
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         BAS   RE,VALPLAN                                                       
         SPACE                                                                  
         LA    R2,SPLPAK8H         VALIDATE PACKAGE DATA                        
         BAS   RE,VALPACK                                                       
*                                                                               
PRTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO READ ASSIGNMENT RECORDS, GET # UNITS & DATES TO ADD            
* NBAIO FOR ASSIGNMENT RECORDS                                                  
* ANETWS1 PACKAGE REC                                                           
* MYIO AREA FOR BUILDING UNITS                                                  
*                                                                               
ASSREC   NTR1                                                                   
*        MVI   PRGSW,C'Y'                                                       
*        MVC   KEY(20),SVUAKEY                                                  
*        OC    SVUAKEY,SVUAKEY                                                  
*        BNZ   AS100                                                            
         MVI   KEY,X'22'                                                        
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(4),NBACTNET                                                
         MVC   KEY+8(1),PLNDPT                                                  
         MVC   KEY+9(4),PLAN                                                    
AS100    GOTO1 UNTHI                                                            
         B     AS125                                                            
                                                                                
AS120    GOTO1 UNTSEQ                                                           
                                                                                
AS125    CLC   KEY(NPUKPROG-NPUKEY),KEYSAVE                                     
         BNE   AS700                                                            
         CLI   PLHAVE,C'W'                                                      
         BNE   AS150                                                            
         ZIC   RF,KEY+19           CHECK IF VALID QUARTER                       
         CLI   KEY+19,0                                                         
         BNE   AS140                                                            
         LA    RF,4                                                             
AS140    LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    AS120                                                            
AS150    DS    0H                                                               
*        CLI   SVUAKEY,0                                                        
*        BE    AS600                                                            
*        CLI   PRGSW,C'Y'                                                       
*        BNE   AS600                                                            
*        MVI   PRGSW,C'N'                                                       
*                                                                               
         MVC   RECIO,NBAIO                                                      
         GOTO1 GETUNT                                                           
         L     R4,NBAIO                                                         
         USING NPURECD,R4                                                       
         MVC   PROG,NPUKPROG                                                    
         DROP  R4                                                               
*                                                                               
AS200    L     R4,NBAIO                                                         
         LA    R4,NPGDEL-NPUKEY(R4)                                             
AS220    CLI   0(R4),0                                                          
         BE    AS500                                                            
         CLI   0(R4),X'02'                                                      
         BE    AS225                                                            
         CLI   0(R4),X'12'                                                      
         BNE   AS400                                                            
* - UNIT ASSIGNMENT ELELENT NEW/OLD                                             
AS225    MVC   BYTE,3(R4)                                                       
         MVC   PUPDEMS,16(R4)                                                   
         CLI   PLHAVE,C'Q'                                                      
         BE    AS260                                                            
         CLI   PLHAVE,C'W'                                                      
         BE    AS270                                                            
         MVC   DUB+1(1),3(R4)                                                   
         MVI   DUB+2,X'01'         START WITH THE 1ST                           
         MVI   BYTE,1                                                           
         CLI   3(R4),4             FIND CORRECT QUATER                          
         BL    AS240                                                            
         MVI   BYTE,2                                                           
         CLI   3(R4),7                                                          
         BL    AS240                                                            
         MVI   BYTE,3                                                           
         CLI   3(R4),9                                                          
         BL    AS240                                                            
         BNE   *+14                                                             
         CLC   PLANYR,2(R4)                                                     
         BE    AS240                                                            
         MVI   BYTE,4                                                           
AS240    ZIC   RE,PLANYR                                                        
         CLI   BYTE,4                                                           
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+6,GETDAY,ADDAY                   
         CLI   3(R4),9             IF SEP CAN BE 4TH OR 3RD QTR                 
         BNE   AS244                                                            
         CLC   PLANYR,2(R4)                                                     
         BE    AS242                                                            
         LA    R0,14                                                            
         GOTO1 ADDAY,(R1),WORK+6,WORK+6,(R0)                                    
         B     AS244                                                            
*                                                                               
AS242    LA    R0,13                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK+12,(R0)                                  
AS244    GOTO1 DATCON,(R1),(0,WORK+6),(3,DUB)                                   
         GOTO1 DATCON,(R1),(0,WORK+12),(3,DUB+3)                                
         B     AS280                                                            
*                                                                               
AS260    ZIC   RF,BYTE                                                          
         MH    RF,=H'2'                                                         
         LA    RF,QTBL1-2(RF)                                                   
         MVC   DUB+1(1),0(RF)                                                   
         MVC   DUB+4(1),1(RF)                                                   
         MVI   DUB+2,1                                                          
         MVI   DUB+5,1                                                          
         ZIC   RE,PLANYR                                                        
         CLI   BYTE,4                                                           
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
         STC   RE,DUB+3                                                         
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTO1 DATCON,(R1),(3,DUB+3),(0,WORK+6)                                 
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,GETDAY,ADDAY                  
         CLI   BYTE,4                4TH QTR                                    
         BNE   AS262                                                            
         LA    R0,14                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK+12,(R0)                                  
AS262    GOTO1 DATCON,(R1),(0,WORK+12),(3,DUB)                                  
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 =V(GETBROAD),(R1),(1,WORK+6),WORK+12,GETDAY,ADDAY                
         CLI   BYTE,3              3RD QTR                                      
         BNE   AS264                                                            
         LA    R0,13                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK+18,(R0)                                  
AS264    GOTO1 DATCON,(R1),(0,WORK+18),(3,DUB+3)                                
         B     AS280                                                            
*                                                                               
AS270    L     RE,NBAIO            FIND PROPER QUARTER                          
         MVI   BYTE,1                                                           
         CLI   19(RE),1                                                         
         BE    AS273                                                            
         MVI   BYTE,2                                                           
         CLI   19(RE),2                                                         
         BE    AS273                                                            
         MVI   BYTE,3                                                           
         CLI   19(RE),3                                                         
         BE    AS273                                                            
         MVI   BYTE,4                                                           
         CLI   19(RE),0                                                         
         BE    AS273                                                            
AS273    GOTO1 DATCON,DMCB,(2,2(R4)),(0,WORK+6)                                 
         LA    R0,6                                                             
         GOTO1 ADDAY,(R1),WORK+6,WORK+12,(R0)                                   
         GOTO1 DATCON,(R1),(0,WORK+6),(3,DUB)                                   
         GOTO1 DATCON,(R1),(0,WORK+12),(3,DUB+3)                                
*                                                                               
AS280    ZIC   RF,BYTE                                                          
         LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    AS400                                                            
         CLI   PLHAVE,C'W'                                                      
         BE    AS290                                                            
         CLC   DUB(3),SVSTDTE                                                   
         BNL   *+10                                                             
         MVC   DUB(3),SVSTDTE                                                   
         CLC   DUB+3(3),SVENDTE                                                 
         BNH   *+10                                                             
         MVC   DUB+3(3),SVENDTE                                                 
AS290    EQU   *                                                                
         CLI   0(R4),X'02'                                                      
         BNE   AS295                                                            
         USING NPUAD,R4                                                         
         MVC   OVSHR,NPUASHR                                                    
         MVC   OVHUT,NPUAHUT                                                    
         MVC   OVRTG,NPUARTG                                                    
         MVC   OVVPH,NPUAVOVR                                                   
         MVC   OVRBITS,NPUAOVRD                                                 
         MVC   UNTNUM,NPUAUNS                                                   
         B     AS300                                                            
*                                                                               
AS295    EQU   *                                                                
         CLI   0(R4),X'12'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPUBD,R4                                                         
         MVC   OVSHR,NPUBSHR                                                    
         MVC   OVHUT,NPUBHUT                                                    
         MVC   OVRTG,NPUBRTG                                                    
         MVC   OVVPH,NPUBVOVR                                                   
         MVC   OVRBITS,NPUBOVRD                                                 
         NI    OVRBITS,X'EF'                                                    
         MVC   UNTNUM,NPUBUNS                                                   
*                                                                               
AS300    CLI   SVLEN1,0                                                         
         BE    AS340                                                            
         LA    R0,4                FIND LENGTH                                  
         LA    RF,LENS                                                          
         LA    R1,UNTNUM                                                        
AS310    CLC   SVLEN1,0(RF)                                                     
         BE    AS320                                                            
         CLI   SVLEN2,0                                                         
         BE    *+14                                                             
         CLC   SVLEN2,0(RF)                                                     
         BE    AS320                                                            
         MVI   0(R1),0             DOESN'T MATCH LENGTH FILTER                  
AS320    LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AS310                                                         
*                                                                               
AS340    OC    UNTNUM,UNTNUM                                                    
         BZ    AS400                                                            
         BAS   RE,GETDTES                                                       
         BAS   RE,ADDUNTS                                                       
         XC    NUMWKS(3),NUMWKS    3 FIELDSNUMWKS/NUMUNAD/MUNPUAD               
*                                                                               
AS360    ZIC   RF,UNITS                                                         
         L     R0,PUNITS                                                        
         AR    R0,RF                                                            
         ST    R0,PUNITS                                                        
AS400    ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     AS220                                                            
         EJECT                                                                  
*                                                                               
AS500    MVC   KEY,SVUAKEY                                                      
         MVI   BYTE,0                                                           
         GOTO1 UNTHI                                                            
         CLC   SVUAKEY,KEY                                                      
         BE    AS120               GET NEXT ASSIGNMENT RECORD                   
         DC    H'0'                                                             
*                                                                               
AS600    DS    0H                                                               
*        LA    R2,PUPNPRGH                                                      
*        OI    6(R2),X'80'                                                      
*        MVC   8(6,R2),KEY+13                                                   
*        LA    R2,PUPYESH                                                       
*        OI    6(R2),X'81'                                                      
*        MVC   8(3,R2),=C'YES'                                                  
*        ST    R2,FADDR                                                         
*        CLI   SVUAKEY,0                                                        
*        BE    *+8                                                              
*        BAS   R3,LISUNIT                                                       
*        MVC   SVUAKEY,KEY                                                      
*        MVI   FERN,PRGUTRNS                                                    
*        B     ERROR                                                            
*                                                                               
AS700    DS    0H                                                               
*        LA    R2,PUPNPRGH                                                      
*        OI    6(R2),X'80'                                                      
*        MVC   8(6,R2),=6CL1'*'                                                 
*        BAS   R3,LISUNIT                                                       
*        XC    SVAREA18,SVAREA18   ALL DONE CLEAR CHANGES                       
ASXIT    B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
* OUTPUT # UNITS PER PROGRAM TO LINE                                            
*LISUNIT  L     R0,PUNITS                                                       
*        LA    R2,PUPSUMH                                                       
*        OI    6(R2),X'80'                                                      
*        EDIT  (R0),(4,8(R2)),ALIGN=LEFT                                        
*        MVC   13(18,R2),=C'UNITS TRANSFER FOR'                                 
*        MVC   32(6,R2),SVUAKEY+13                                              
*        BR    R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH ENTRY IN SCHEDULE LIST            
*                                                                               
GETDTES  NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,DUB),(2,HALF)                                     
         BAS   RE,GETPROG                                                       
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK+6)                                   
         GOTO1 DATCON,(R1),(3,DUB+3),(0,WORK+12)                                
         GOTO1 GETDAY,(R1),WORK+6,THREE                                         
         ZIC   R2,0(R1)                                                         
         L     R3,APROGEL                                                       
         USING NPGEL92,R3                                                       
         ZIC   R0,NPGDAYNO         DAY NUMBER OF UNIT'S DAY                     
         SRL   R0,4                USE THE START DAY'S NUMBER                   
         MVC   FSTDTE,WORK+6       SET EBCDIC AIR DATE                          
         SR    R0,R2               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    GD100               THERE IS NONE                                
         BP    *+8                                                              
         AH    R0,=H'7'                                                         
         GOTO1 ADDAY,(R1),WORK+6,FSTDTE,(R0)                                    
*                                                                               
GD100    CLC   FSTDTE,WORK+12                                                   
         BNH   GD120                                                            
         DC    H'0'                (UNITS ON PUP NOT ALL ADDED)                 
*        MVI   FERN,UNTNOTAD       NO UNITS FOR THIS PERIOD                     
*        B     ERROR                                                            
GD120    LA    R0,4                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R3,UNTNUM                                                        
GD140    IC    RF,0(R3)                                                         
         AR    RE,RF                                                            
         LA    R3,1(R3)                                                         
         BCT   R0,GD140                                                         
         STC   RE,UNITS                                                         
         SR    R4,R4                                                            
         SR    R2,R2                                                            
         B     GD220                                                            
GD200    GOTO1 ADDAY,DMCB,FSTDTE,WORK,(R2)                                      
         CLC   WORK,WORK+12                                                     
         BH    GD240                                                            
GD220    LA    R2,7(R2)                                                         
         LA    R4,1(R4)                                                         
         B     GD200                                                            
*                                                                               
GD240    STC   R4,NUMWKS                                                        
         XC    FULL,FULL                                                        
         STC   R4,FULL+3                                                        
         SR    RE,RE                                                            
         ZIC   RF,UNITS                                                         
         D     RE,FULL                                                          
         STC   RF,NUMUNAD                                                       
         STC   RE,NUMPUAD                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH PROGRAM ASS. RECORD               
*                                                                               
ADDUNTS  NTR1                                                                   
*        OC    VEDIT,VEDIT           *** CALLS NEBUY35                          
*        BNZ   AU100                 *** GENERAL EDIT OVERLAY                   
*        GOTO1 CALLOV,DMCB,(X'35',0),ATWA                                       
*        CLI   DMCB+4,X'FF'                                                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        MVC   VEDIT,DMCB                                                       
*                                                                               
AU100    GOTO1 DATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                                
         GOTO1 DATCON,(R1),(0,ENDDTE),(2,ENDDTE2)                               
*                                                                               
         LA    R4,MYBLOCK            ** R4 = UNIT BLOCK **                      
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA2                                                
         L     RE,ANETWS1          PACKAGE RECORD                               
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST IF INTEGRATION TABLE LOOK UP            
         BNO   AU200                                                            
         DROP  RE                                                               
         LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PRGSW BACK INTG RATES HERE                   
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVI   INTGREAD,0          DO 1ST READ                                  
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,NXTDTE                                                  
         ZIC   RE,UNITS                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'6'                                                         
         AR    RE,R2                                                            
         MVC   INTGEDDT,ENDDTE2                                                 
         SPACE                                                                  
         EJECT                                                                  
AU200    L     R3,AMYIO                                                         
         LR    RE,R3                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING NURECD,R3                                                        
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,NBACTAM                                                    
         MVC   NUKCLT,NBACTCLI                                                  
         MVC   NUKDATE,NXTDTE                                                   
         MVC   NUKNET,NBSELNET                                                  
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,NBSELEST                                                  
*                                                                               
         L     RE,ANETWS1          ANETWS1=PACKAGE REC                          
         USING NPRECD,RE                                                        
         MVC   NUKDP,NPAKDP                                                     
         DROP  RE                                                               
         SPACE                                                                  
****                                                                            
*          DATA SET NEBUY35    AT LEVEL 047 AS OF 08/05/93                      
* INITIALIZATION CODE (COMMAND = C'I')                                          
*                                                                               
IN       DS    0H                                                               
         XC    UNDEMS,UNDEMS       CLEAR ESTIMATED DEMOS                        
         CLI   UNACTION,CM         TEST FOR CHANGE MULTIPLE                     
         BNE   *+16                                                             
         MVC   UNSHR(4),NBESTSHR   SET INPUT VALUE = RECORD VALUES              
         MVC   UNRAT,NBESTHOM+2    SINCE ANY OF 3 MAY BE INPUT                  
         XC    UNAMISS,UNAMISS     AND MISSING RECORD ADCON                     
         XC    UNDATE,UNDATE                                                    
         MVI   UNSQH,0                                                          
         TM    UNBITOPT,X'20'                                                   
         BO    *+12                                                             
         CLI   UNACTION,COPYU                                                   
         BE    IN6                                                              
         CLI   UNACTSW,C'C'                                                     
         BNE   IN2                                                              
IN2      CLI   UNACTION,COPYU                                                   
         BE    IN6                                                              
         MVC   NUPACK,PACK         PACKAGE NUMBER                               
         L     RE,ANETWS1                                                       
         USING NPRECD,RE                                                        
         MVC   NUPACKST,NPAKSTAT   PACKAGE STATUS                               
         MVC   NUPOSTDT,NPAKPDT    PACKAGE METER                                
         L     RF,APROGEL                                                       
         TM    NPGSTAT-NPGELEM(RF),X'80'                                        
         BO    *+10                                                             
         MVC   NUHUTPCT,NPAKHPCT   PACKAGE HUT ADJ                              
         MVC   NUHUTTYP,NPAKHTYP   PACKAGE HUT TYP                              
         NI    NUACTWHY,X'03'      ZERO ALL BITS EXCEPT CONVERSION/COPY         
         MVC   NUMARKET,NETMARK                                                 
         MVC   NUALPHA,AGYALPH                                                  
         OI    NUACTWHY,X'80'      NEW BUY                                      
         TM    NPAKHUTL,X'80'      TEST FOR HUTS FROM DEMO FILE                 
         BZ    *+8                 NO                                           
         OI    NUUNST2,X'80'                                                    
         TM    NPAKHUTL,X'40'      TEST FOR 52 WEEK HUT CALENDAR                
         BZ    *+8                                                              
         OI    NUUNST2,X'40'                                                    
         MVI   NUMAINEL,X'01'                                                   
         MVI   NUMAINLN,80                                                      
         ZIC   R1,NUMAINLN                                                      
         LA    R1,NUMAINEL-NUKEY+1(R1) SET INITIAL RECORD LENGTH                
         STH   R1,NURLEN                                                        
         SPACE                                                                  
IN4      L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   NUPRFILT,NPGFILT    PROGRAM FILTERS                              
         SPACE                                                                  
IN6      DS    0H                                                               
         MVI   ELCODE,X'08'                                                     
         GOTO1 HELLO,DMCB2,(C'D',=C'UNTFILE'),(ELCODE,(R4)),(1,=C'K')           
         ICM   R3,15,APAKFEL                                                    
         BZ    IN8                                                              
         BAS   RE,PUTEL                                                         
         SPACE 1                                                                
IN8      MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   IN10                                                             
         L     RE,12(R1)                                                        
         MVI   5(RE),0             CLEAR THE LAST ACTIVE BYTE                   
         CLC   NUKDATE,=XL2'B125'  DONT TAG IF UNIT BEFORE SEP5/88              
         BL    *+22                                                             
         MVC   6(1,RE),NBSTSTAT    LOAD IN STATION TYPE                         
         MVC   7(1,RE),NBSUBMED    LOAD IN SUB-MEDIA                            
         MVC   12(1,RE),NBSTPSTT   LOAD IN POST-TYPE                            
         B     IN11                                                             
*                                                                               
IN10     XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVC   0(2,R3),=X'0214'                                                 
         CLC   NUKDATE,=XL2'B125'  DONT TAG IF UNIT BEFORE SEP5/88              
         BL    *+22                                                             
         MVC   6(1,R3),NBSTSTAT    LOAD IN STATION TYPE                         
         MVC   7(1,R3),NBSUBMED    LOAD IN SUB-MEDIA                            
         MVC   12(1,R3),NBPOSTYP   LOAD IN POST-TYPE                            
         BAS   RE,PUTEL                                                         
         SPACE                                                                  
*--SET POSTING TYPE IN THE STATUS FIELD                                         
IN11     LR    RF,R4                                                            
         BAS   RE,SETSTAT                                                       
*                                                                               
INX      L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
         SPACE                                                                  
         MVC   HALF,NUKDATE                                                     
         BAS   RE,GETPROG                                                       
         MVC   INTGSTDT,NUKDATE                                                 
* GET DEFAULT PROCESSING (COMMAND = 'N')                                        
*                                                                               
         LA    R1,TYPTAB           POINT TO TYPE TABLE                          
         LA    R2,L'TYPTAB         SET BXLE INCREMENT                           
DEF2     TM    4(R1),SETDEF        TEST IF DEFAULT AVAILABLE                    
         BZ    DEF4                NO                                           
         CLI   UNEDATA,0           TEST FOR DATA TYPE FILTER                    
         BE    DEF3                NO                                           
         CLC   UNEDATA,0(R1)       TEST IF RIGHT ENTRY LOCATED                  
         BNE   DEF4                NO-BUMP TO NEXT ENTRY                        
         SPACE                                                                  
DEF3     SR    RF,RF                                                            
         ICM   RF,7,1(R1)          ROUTINE ADDRESS                              
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         SPACE                                                                  
DEF4     AR    R1,R2                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   DEF2                                                             
         L     RE,SAVEREG                                                       
         BR    RE                                                               
****     GOTO1 VEDIT,DMCB,(C'N',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+22                TEMP FIX TO BYPASS BAD PLANS***              
         CLI   UNERROR,STERR                                   ***              
         BE    EXXMOD                                          ***              
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*****                                                                           
*****    GOTO1 VEDIT,DMCB,(C'D',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+22                TEMP FIX TO BYPASS BAD PLANS***              
         CLI   UNERROR,STERR                                   ***              
         BE    EXXMOD                                          ***              
         MVC   FERN,UNERROR                                                     
         B     AUERR                                                            
         GOTO1 (RF),(R1),(C'F',(R4))                                            
         SPACE                                                                  
*--CREATE SECONDARY 01 ELEMENT SET PUP CREATION INDICATOR                       
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'02',0(R3))                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         ICM   RE,15,12(R1)                                                     
         OI    2(RE),X'10'                                                      
*--SET POSTING TYPE CONTROL BIT                                                 
         MVI   CTPOSTYP,0                                                       
         CLI   12(RE),C'N'       NETWORK                                        
         BE    AU280                                                            
         CLI   12(RE),C'C'       CABLE                                          
         BNE   *+12                                                             
         OI    CTPOSTYP,X'01'                                                   
         B     AU280                                                            
         CLI   12(RE),C'S'       SYNDICATION                                    
         BNE   *+12                                                             
         OI    CTPOSTYP,X'02'                                                   
         B     AU280                                                            
         OI    CTPOSTYP,X'03'      OTHER                                        
         SPACE                                                                  
AU280    TM    OVRBITS,X'F0'                                                    
         BZ    AU390                                                            
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         TM    OVRBITS,X'80'                                                    
         BZ    AU300                                                            
         MVI   WORK+4,C'S'                                                      
         MVC   WORK+10(2),OVSHR                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU300    TM    OVRBITS,X'40'                                                    
         BZ    AU320                                                            
         MVI   WORK+4,C'P'                                                      
         MVC   WORK+10(2),OVHUT                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU320    TM    OVRBITS,X'20'                                                    
         BZ    AU340                                                            
         MVI   WORK+4,C'R'                                                      
         MVC   WORK+10(2),OVRTG                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU340    TM    OVRBITS,X'10'                                                    
         BZ    AU390                                                            
         MVI   WORK+4,C'V'                                                      
         MVC   WORK+10(2),OVVPH                                                 
         ZIC   R0,ESTNDEMS                                                      
         LA    RE,ESTDEMS                                                       
AU360    CLC   2(1,RE),TRGDEMO                                                  
         BE    AU380                                                            
         LA    RE,3(RE)                                                         
         BCT   R0,AU360                                                         
         B     AU390                                                            
*                                                                               
AU380    MVC   WORK+5(1),TRGDEMO                                                
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
*                                                                               
AU390    TM    OVRBITS,X'40'                                                    
         BNZ   AU400                                                            
         CLI   CALCHUT,C'N'                                                     
         BE    AU400                                                            
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     R2,12(R1)                                                        
         ICM   RE,3,OVHUT                                                       
         SRDA  RE,32                                                            
         M     RE,=F'10'                                                        
         ST    RF,WORK                                                          
         MVC   5(2,R2),WORK+2                                                   
         MVC   3(2,R2),OVRTG                                                    
         MVC   7(2,R2),OVSHR                                                    
         OC    OVSHR,OVSHR                                                      
         BNZ   AU400                                                            
         BAS   RE,CALCSHAR                                                      
         MVC   7(2,R2),WORK                                                     
         SPACE                                                                  
AU400    BAS   RE,CALSHRAT         CALC SHARE OR RATING OVERRIDE                
*                                                                               
         OC    NPACKGU,NPACKGU     NEW PACKAGE GUARANTEE                        
         BZ    AU405                                                            
         XC    WORK(8),WORK                                                     
         MVC   WORK(2),=XL2'B308'                                               
         MVC   WORK+2(4),NPACKGU                                                
         BAS   RE,PUTEL                                                         
AU405    OC    NDEMOGU,NDEMOGU                                                  
         BZ    AU406                                                            
         OC    NDEMOCAT,NDEMOCAT                                                
         BZ    AU406                                                            
         XC    WORK(11),WORK                                                    
         MVC   WORK(2),=XL2'B40B'                                               
         MVC   WORK+2(3),NDEMOCAT                                               
         MVC   WORK+5(4),NDEMOGU                                                
         BAS   RE,PUTEL                                                         
*  ADD DEMO OVERRIDE INFORMATION                                                
AU406    LA    R3,PUPDEMS                                                       
         LA    R2,PLNDEMS                                                       
*                                                                               
         LA    R0,6                                                             
         XC    WORK(20),WORK                                                    
         MVC   WORK(3),=XL3'DD0C00'                                             
AU407    OC    0(2,R3),0(R3)                                                    
         BZ    AU409                                                            
         MVC   WORK+3(3),0(R2)                                                  
         MVI   WORK+4,C'V'                                                      
         MVC   WORK+10(2),0(R3)                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK                          
AU409    LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,AU407                                                         
         L     R3,AIOAREA1                                                      
*                                                                               
AU415    BAS   RE,GETSUB           ASSIGN SUB-LINE                              
         OI    NUUNST2,X'02'                                                    
         ZIC   R2,NUMUNAD                                                       
         SR    RE,RE                                                            
         ICM   RE,1,NUMPUAD                                                     
         BZ    AU420                                                            
         LA    R2,1(R2)                                                         
         BCTR  RE,0                                                             
         STC   RE,NUMPUAD                                                       
*                                                                               
* LOOP TO ADD RECORDS UPDATEING LINE # ONLY                                     
AU420    STC   R2,LOOP1                                                         
*                                                                               
         LA    R0,4                FIND LENGTH                                  
         LA    RF,LENS                                                          
         LA    R1,UNTNUM                                                        
         SR    RE,RE                                                            
AU422    ICM   RE,1,0(R1)                                                       
         BNZ   AU424                                                            
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AU422                                                         
         DC    H'0'                                                             
AU424    BCTR  RE,0                                                             
         STC   RE,0(R1)                                                         
         ZIC   RE,0(RF)                                                         
         STC   RE,NULEN                                                         
         OC    NURSTAT,CTPOSTYP    SET STATUS BIT                               
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
         MVC   UNITDA,NDXDA        SAVE RETURNED DISK ADDRESS                   
         GOTO1 VEDIT,DMCB,(C'P',(R4)),WORK                                      
         LA    R2,WORK                                                          
         LA    R1,NDIRPTRS         N'PRGSWIVE POINTERS                          
         L     RE,NPTRS            N'POINTERS IN TABLE                          
         LR    RF,RE                                                            
         AR    RF,R1               UPDATE POINTER COUNT                         
         ST    RF,NPTRS                                                         
         MH    RE,=Y(NDIRLEN)      INDEX INTO POINTER TABLE                     
         L     R0,AIOAREA4         POINT TO NEXT ENTRY POSITION                 
         AR    RE,R0                                                            
         SPACE                                                                  
AU440    MVC   0(NDIRLEN,RE),0(R2) EXTRACT PRGSWIVE POINTER                     
         MVI   NDIRCTL(RE),0       ZERO CONTROL BYTE                            
         LA    RE,NDIRLEN(RE)      NEXT TABLE POSITION                          
         LA    R2,NDIRLEN(R2)      NEXT POINTER                                 
         BCT   R1,AU440                                                         
         CH    RF,=H'80'           TEST FOR TABLE OVERFLOW                      
         BL    *+8                                                              
         BAS   RE,SRTPTRS                                                       
         SPACE                                                                  
         CLI   NUKSUB,2                                                         
         BNE   *+8                                                              
         BAS   RE,SUBPRT                                                        
*                                                                               
         ZIC   R1,NUKSUB           SAME DATE SO INCREMENT THE                   
         LA    R1,1(R1)            LAST SUB-LINE FOR DATE                       
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,NUKSUB                                                        
         ZIC   R2,LOOP1                                                         
         BCT   R2,AU420                                                         
*                                                                               
AU500    GOTO1 DATCON,DMCB,(2,NUKDATE),(0,FSTDTE)                               
         LA    R2,7                                                             
         GOTO1 =V(ADDAY),DMCB,FSTDTE,FSTDTE,(R2)                                
         GOTO1 DATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                                
AU520    OC    UNTNUM,UNTNUM                                                    
         BNZ   AU200                                                            
         SPACE                                                                  
         BAS   RE,SRTPTRS                                                       
         B     EXXMOD                                                           
         SPACE                                                                  
SRTPTRS  NTR1                                                                   
         L     R2,NPTRS            SORT POINTERS IN DESCENDING SEQUENCE         
         LTR   R2,R2               0 IF POINTERS JUST = 80                      
         BZ    SP100                                                            
         L     R3,AIOAREA4         R3=POINTS TO PRGSWIVE POINTER TABLE          
         GOTO1 VXSORT,DMCB,(X'FF',0(R3)),(R2),NDIRLEN,L'NUKEY,0                 
         BAS   RE,NEWPTR           ADD NEW POINTER                              
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         BCT   R2,*-8                                                           
SP100    XC    NPTRS,NPTRS                                                      
         L     RE,AIOAREA4                                                      
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE OR RATING VALUE                                
*                                                                               
CALSHRAT NTR1                                                                   
         TM    OVRBITS,X'A0'       IS BOTH SHARE AND RATING OVERRIDDEN          
         BO    EXXMOD                                                           
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     RF,12(R1)                                                        
         SR    R2,R2                                                            
         ICM   R2,3,5(RF)          SHARE = RATING / HUT                         
*                                                                               
         TM    OVRBITS,X'40'       IS HUT OVERRIDDEN                            
         BZ    CALSR020                                                         
         SR    RF,RF                                                            
         ICM   RF,3,OVHUT          MULT OVERRIDDEN HUT VALUE BY 10              
         M     RE,=F'10'                                                        
         LR    R2,RF                                                            
CALSR020 TM    OVRBITS,X'80'       IS SHARE OVERRIDDEN                          
         BO    CALSR050                                                         
         TM    OVRBITS,X'20'       IS RATING OVERRIDDEN                         
         BZ    EXXMOD                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         LTR   RE,R2               GET HUT VALUE                                
         BZ    EXXMOD              ZERO DIVISOR                                 
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'5'            ROUND BACK UP TO 1                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MVI   WORK+4,C'S'                                                      
         STCM  R1,3,WORK+10                                                     
         B     CALSR100                                                         
         SPACE 1                                                                
CALSR050 SR    R0,R0               RATING = SHARE X HUT                         
         ICM   R0,3,OVSHR                                                       
         LR    R1,R2               GET HUT VALE                                 
         MR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO ONE DECIMAL PLACE                   
         D     R0,=F'10000'                                                     
         MVI   WORK+4,C'R'                                                      
         STCM  R1,3,WORK+10                                                     
*                                                                               
CALSR100 BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
         B     EXXMOD                                                           
*                                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE                                                
*                                                                               
CALCSHAR NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         OC    OVHUT,OVHUT                                                      
         BZ    EXXMOD              ZERO DIVISOR                                 
         SR    RE,RE                                                            
         ICM   RE,3,OVHUT          GET HUT VALUE                                
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'50'            ROUND BACK UP TO 1                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         STCM  R1,3,WORK                                                        
         B     EXXMOD                                                           
         SPACE 1                                                                
*                                                                               
* ERROR EXIT                                                                    
*                                                                               
AUERR    LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,AIOAREA1                                                      
         MVI   NUKPTYPE,X'84'      USE PASSIVE KEY TO FIND NEXT NUMBER          
         MVC   NUKPAM,NUKAM-NUKEY(R1)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R1)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R1)                                         
         MVC   NUKPPROG,NUKPROG-NUKEY(R1)                                       
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         LA    R0,UPDATE+PASSDEL+UNT+DIR+HIGH                                   
         SPACE                                                                  
GETSUB2  GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NUKPSUB-NUKPKEY),KEYSAVE                                     
         BNE   GETSUB4                                                          
         LA    R0,UPDATE+PASSDEL+UNT+DIR+SEQ                                    
         B     GETSUB2                                                          
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         L     R4,AIOAREA1         POINT BACK TO RECORD                         
         USING NURECD,R4                                                        
         STC   R1,NUKSUB                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(NDIRLEN),0(R3)  RESET ENTIRE POINTER                         
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVC   KEY(NDIRLEN),0(R3)                                               
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         MVC   NUKPPROG,PROG                                                    
         MVC   NUKPDATE,NXTDTE                                                  
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1            SUB-LINE 1                                  
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA2         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
*        CLI   NPROGS,1                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R4 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R4,WORK             ELEMENT AREA                                 
         LA    R5,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
*                                                                               
SETP20   CLC   0(1,R5),4(R4)                                                    
         BE    SETP40                                                           
         LA    R5,2(R5)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R4),1(R5)                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
PUTEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND SUB-ROUTINE EXIT                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
*                                                                               
PRINCOMP CLC   FLD(0),=C'PRINT'                                                 
ZONECOMP CLC   FLD(0),=C'ZONE'                                                  
         SPACE 2                                                                
* TABLE OF DAYPART CODES AND NAMES                                              
*                                                                               
DPTTAB   DS    0CL9                                                             
         DC    C'D',CL8'DAYTIME'                                                
         DC    C'U',CL8'UNWIRED'                                                
         DC    C'F',CL8'FRINGE'                                                 
         DC    C'P',CL8'PRIME'                                                  
         DC    C'K',CL8'KIDS'                                                   
         DC    C'T',CL8'TEENS'                                                  
         DC    C'Y',CL8'YOUTH'                                                  
         DC    C'S',CL8'SPORTS'                                                 
         DC    C'N',CL8'NEWS'                                                   
         DC    C'E',CL8'EARLY'                                                  
         DC    C'L',CL8'LATE'                                                   
         DC    C'C',CL8'CABLE'                                                  
         DC    C'O',CL8'OLYMPICS'                                               
         DC    C'R',CL8'RADIO'                                                  
         DC    C'X',CL8'SYND.'                                                  
         DC    C'X',CL8'X'                                                      
         DC    C'I',CL8'SPECIAL'                                                
         DC    C'I',CL8'I'                                                      
         DC    C'V',CL8'OVERNITE'                                               
         DC    C'V',CL8'V'                                                      
         DC    C'W',CL8'WKNDPM'                                                 
         DC    C'M',CL8'WKNDAM'                                                 
         DC    C'M',CL8'M'                                                      
         DC    C'A',CL8'ACCESS'                                                 
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         SPACE 2                                                                
* TABLE OF STATUS ACTIONS (COVERED BY STATABD)                                  
*              BYTES 0-7 = STATUS KEYWORD                                       
*              BYTE  8   = MINIMUM COMPARE LENGTH                               
*              BYTE  9   = OR MASK FOR STATUS                                   
*              BYTE 10   = AND MASK FOR STATUS                                  
*              BYTE 11   = STATUS INDICATORS                                    
*                                                                               
STATAB   DS    0CL(STATABL)                                                     
         DC    CL8'LOCKED',AL1(1),AL1(LOCKED)                                   
         DC    X'FF',AL1(UNITFIX)                                               
*                                                                               
         DC    CL8'UNLOCKED',AL1(3),X'00'                                       
         DC    AL1(UNLOCKED),AL1(UNITFIX)                                       
*                                                                               
         DC    CL8'FROZEN',AL1(1),AL1(FROZEN)                                   
         DC    X'FF',X'00'                                                      
*                                                                               
         DC    CL8'UNFROZEN',AL1(3),X'00'                                       
         DC    AL1(UNFROZEN),X'00'                                              
*                                                                               
STAENT   EQU   (*-STATAB)/L'STATAB                                              
UNTFILE  DC    CL8'UNTFILE'                                                     
*                                                                               
QTBL     DC    0CL8' '                                                          
QTBL1    DC    XL1'01',XL1'03'                                                  
QTBL2    DC    XL1'04',XL1'06'                                                  
QTBL3    DC    XL1'07',XL1'09'                                                  
QTBL4    DC    XL1'09',XL1'0C'                                                  
*                                                                               
MAXPTRS  EQU   80                                                               
         EJECT                                                                  
*              DATAMGR INTERFACE                                                
         SPACE 3                                                                
UNTHI    NTR1                                                                   
         MVC   FILE,=C'UNTDIR  '                                                
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     DIRALL                                                           
SPTHI    NTR1                                                                   
         MVC   FILE,=C'SPTDIR  '                                                
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     DIRALL                                                           
STAHI    NTR1                                                                   
         MVC   FILE,=C'STATION '                                                
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
UNTSEQ   NTR1                                                                   
         MVC   FILE,=C'UNTDIR  '                                                
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
SPTSEQ   NTR1                                                                   
         MVC   FILE,=C'SPTDIR  '                                                
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
READ     NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         SPACE 1                                                                
DIRALL   DS    0H                                                               
         SR    R4,R4                                                            
         CLI   BYTE,C'Y'           PASS DELETED RECS                            
         BNE   *+8                                                              
         LA    R4,8                                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 NBDM,DMCB,((R4),COMMAND),FILE,KEY,KEY,0                          
         B     XIT                                                              
*        SPACE 1                                                                
GETUNT   NTR1                                                                   
         MVC   FILE,=C'UNTFILE '                                                
         MVC   NBDTADSP,=H'27'                                                  
         LA    R3,KEY+21                                                        
         CLI   BYTE,C'Y'           SEE IF WRITE ON                              
         BNE   GETREC              NO, DONT READ FOR UPDATE                     
         LA    R4,128(R4)          YES,(TURN ON X'80' BIT)                      
         B     GETREC                                                           
*                                                                               
GETSPT   NTR1                                                                   
         MVC   FILE,=C'SPTFILE '                                                
         MVC   NBDTADSP,=H'24'                                                  
         LA    R3,KEY+14                                                        
         B     GETREC                                                           
*                                                                               
GETREC   DS    0H                                                               
         L     R2,RECIO                                                         
         GOTO1 NBDM,DMCB,((R4),=C'GETREC'),FILE,(R3),(R2),DMWORK                
         L     R2,NBADMWRK         DID USER LET US SAVE DMWORK                  
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         MVC   0(96,R2),DMWORK     THEN SAVE VALUES FOR FUTURE PUTREC           
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
ERRMISS  DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
ERRINV   DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
DUPPACK  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** ERROR  DUPLICATE PACKAGE NUMBER"              
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*                                                                               
* GETPROG - GET APPLICABLE PROGRAM RECORD IF NECESSARY                          
*                                                                               
* AT ENTRY HALF CONTAINS COMPRESSED DATE FOR READ                               
*                                                                               
GETPROG  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,MKTNUM                                                   
         MVC   NPGKPROG,PROG                                                    
         MVC   NPGKEND,HALF                                                     
         L     R2,APROGREC                                                      
         CLC   NPGKEY,0(R2)        TEST IF KEY CHANGED                          
         BE    GETPROGX            NO-ALREADY HAVE RIGHT RECORD                 
         GOTO1 SPTHI                                                            
*                                                                               
GETPRG1  CLC   KEY(NPGKEND-NPGKEY),KEYSAVE TEST IF SAME PROGRAM                 
         BE    GETPRG3                                                          
*                                                                               
         MVI   FERN,PROGERR                                                     
         B     GETPROGR                                                         
*                                                                               
GETPRG2  GOTO1 SPTSEQ                                                           
         B     GETPRG1                                                          
         SPACE                                                                  
GETPRG3  GOTO1 GETSPT                                                           
         SPACE                                                                  
         MVI   ELCODE,X'93'        BOOK ELEMENT SEARCH                          
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                        
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BNE   GETPRG4                                                          
         ICM   RE,15,12(R1)                                                     
         USING NPG2ELEM,RE                                                      
         OC    NPG2STD,NPG2STD                                                  
         BZ    GETPRG4                                                          
         CLC   HALF,NPG2STD      MAKE SURE INPUT DATE > PRG START DATE          
         BL    GETPRG2                                                          
         DROP  RE                                                               
*                                                                               
GETPRG4  MVI   ELCODE,X'5D'        BOOK ELEMENT SEARCH                          
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                        
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABOOKEL,12(R1)      SAVE ELEMENT ADCON                           
*                                                                               
         MVI   ELCODE,X'92'        PROGRAM ELEMENT SEARCH                       
         GOTO1 HELLO,DMCB,,(ELCODE,(R2)),0                                      
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
         MVC   APROGEL,12(R1)      SAVE ELEMENT ADCON                           
         B     GETPROGX                                                         
         SPACE                                                                  
GETPROGR L     R3,AACTNTRY                                                      
         USING ACTTABD,R3                                                       
         TM    ACTIND,SETPROGR     TEST FOR HANDLING PROGRAM ERROR              
         BZ    GETPROGX            NO                                           
         LA    R2,BUYPRGH          YES-SET CURSOR POSITION AND EXIT             
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
GETPROGX B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
MYIO     DS    CL2000                                                           
IOAREA1  DS    CL1000                                                           
IOAREA2  DS    CL1000                                                           
         EJECT                                                                  
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDED                                                       
         EJECT                                                                  
         ORG                                                                    
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
AMYIO    DS    A                                                                
AIOAREA1 DS    A                                                                
AIOAREA2 DS    A                                                                
RECIO    DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    A                                                                
UNITDA   DS    A                                                                
APROGEL  DS    A                                                                
NPTRS    DS    F                   NUMBER OF POINTERS                           
PUNITS   DS    F                   PROGRAM UNITS                                
*                                                                               
STATSW   DS    C                   Y=UPDATE STATUS FIELD ON UNITS               
ORMASK   DS    X                                                                
ANDMASK  DS    X                                                                
OLDSTAT  DS    X                                                                
NEWSTAT  DS    X                                                                
*                                                                               
PACKCPM  DS    XL4                 REQUESTED PACKAGE CPM                        
DEMCPM   DS    XL4                 REQUESTED DEMO CPM                           
TARGDEM  DS    CL3                 TARGET DEMO                                  
INTEGR   DS    XL4                 INTEGRATION                                  
PAKUNCD  DS    CL2                 PWO UNIVERSE CODE                            
MASTPRD  DS    CL1                 MASTER ALLOCATION PROD NUMBER                
THREE    DS    CL3                                                              
FILE     DS    CL8                                                              
PACK     DS    CL1                                                              
PACKNAME DS    CL16                                                             
NETPROF  DS    CL16                N0 PROFILE                                   
ESTLIST  DS    CL9                 LIST OR REQUESTED ESTIMATES                  
PROG     DS    CL6                 LIST OR REQUESTED ESTIMATES                  
MKTNUM   DS    CL2                 NETWORK MARKET NUMBER                        
*                                                                               
         DS    0F                                                               
MYBLOCK  DS    CL150                                                            
OVRBITS  DS    XL1                 BITS FOR OVERRIDES                           
OVSHR    DS    XL2                 SHARE FROM ASSIGNMENT RECORD                 
OVHUT    DS    XL2                 HUT FROM ASSIGNMENT RECORD                   
OVRTG    DS    XL2                 RATING FROM ASSIGNMENT RECORD                
OVVPH    DS    XL2                 VPH FROM ASSIGNMENT RECORD                   
UNTNUM   DS    XL4                 UNITS/LENGTH ASS. RECORD                     
UNITS    DS    XL1                 TOTAL UNITS                                  
COUNT    DS    XL1                 TEMPORARY # WEEKS COUNTER                    
FSTDTE   DS    XL6                 FIRST DATE OF PROGRAM                        
NXTDTE   DS    XL2                 NEXT UNIT DATE - COMPRESSED                  
ENDDTE2  DS    XL2                 END DATE - COMPRESSED                        
NUMWKS   DS    XL1                 NUMBER OF WEEKS IN PERIOD                    
NUMUNAD  DS    XL1                 NUMBER OF UNITS                              
NUMPUAD  DS    XL1                 NUMBER OF PARTIAL WEEKS TO ADD UNIT          
CTPOSTYP DS    XL1                 POSTING TYPE CONTROL BIT SETTING             
PRGSW    DS    XL1                 USED TO CALCULATE # IO'S                     
STLEN    DS    XL1                 UNIT LENGTH                                  
LOOP1    DS    XL1                 NUMBER OF UNITS TO ADD FOR DATE              
NPROGS   DS    XL1                 NUMBER OF PROGRAMS PROCESSED                 
PUPDEMS  DS    XL20                OVERRIDE PUP DEMO AMOUNTS                    
         SPACE 2                                                                
SVAREA18 DS    0CL256                                                           
SVUAKEY  DS    CL20                SAVED UNIT ASS. KEY                          
SVACTLIN DS    CL61                LAST DPT,PLAN,DATES,LEN                      
PLAN     DS    CL4                 PLAN CODE                                    
PLNDPT   DS    CL1                 PLAN DAYPART                                 
STRTDTE  DS    XL6                 START DATE                                   
ENDDTE   DS    XL6                 END DATE                                     
TRGDEMO  DS    XL1                 TARGET DEMO                                  
LENS     DS    XL4                 LENGTHS FROM PUP RECORD                      
SVLEN1   DS    XL1                 FILTER LENGTHS 1                             
SVLEN2   DS    XL1                 FILTER LENGTHS 2                             
SVQTBLS  DS    0XL4                WHICH QUARTERS ARE IN USE                    
SVQTBL1  DS    XL1                 QUARTERS 1                                   
SVQTBL2  DS    XL1                 QUARTERS 2                                   
SVQTBL3  DS    XL1                 QUARTERS 3                                   
SVQTBL4  DS    XL1                 QUARTERS 4                                   
PLANYR   DS    XL1                 CURRENT PLAN YEAR                            
PLHAVE   DS    XL1                 PLANS HUT AVERAGE                            
SVACTION DS    XL1                 LAST ACTION #                                
SVSTDTE  DS    XL3                 DATE TO START ADDING UNITS                   
SVENDTE  DS    XL3                 LAST DATE TO END ADDDING UNITS               
PACKGU   DS    XL2                 PACKAGE GUARENTEE FACTOR                     
NPACKGU  DS    XL4                 NEW PACKAGE GUARENTEE FACTOR                 
NDEMOGU  DS    XL4                 NEW DEMO GUARENTEE FACTOR                    
NDEMOCAT DS    XL3                 NEW DEMO CATEGORY FOR GUARENTEE              
DEMOGU   DS    XL2                 DEMO GUARENTEE FACTOR                        
DEMOCAT  DS    XL1                 DEMO CATEGORY FOR GUARENTEE                  
CALCHUT  DS    XL1                 IF C'Y' RECALCULATE PUP HUT                  
PLNDEMS  DS    XL18                                                             
*                                                                               
INTGTBL  DS    0CL17               INTG INF FOR EDIT MODULE                     
INTGREAD DS    X                   INTEGRATION ALREADY READ IN                  
INTGAIO  DS    A                   A(IO AREA (RATES RETURNED HERE ALSO)         
INTGSTDT DS    H                   START DATE                                   
INTGEDDT DS    H                   END DATE                                     
INTHLDIT DS    A                   (ADDRESS OF INT TABLE LOOKUP VALUES)         
INTDKA   DS    A                   LAST DISK ADDRESS READ FOR INTEG             
         DS    CL5                 SPARE                                        
         SPACE 2                                                                
SVINTTBL DS    1900C                                                            
IOAREA5  DS    2000X               READ IN ASS. RECORD                          
         SPACE                                                                  
* DSECT TO COVER STATUS TABLE                                                   
*                                                                               
STATABD  DSECT                                                                  
STANAME  DS    CL8                 NAME                                         
STAMIN   DS    X                   MINIMUM COMPARE LENGTH                       
STAOR    DS    X                   OR MASK                                      
STAND    DS    X                   AND MASK                                     
STACTL   DS    X                   CONTROL VALUES (X'80'=CHANGE UNITS)          
STATABL  EQU   *-STATABD                                                        
         SPACE 2                                                                
UNBLOCKD DSECT                                                                  
UNBLOCK  DS    0CL80                                                            
UNAGLOB  DS    A                   A(GLOBAL WORKING STORAGE)                    
UNFLDH   DS    A                   A(FIELD HEADER TO BE EDITED)                 
UNAREC   DS    A                   A(UNIT RECORD)                               
UNALOCAL DS    A                   A(LOCAL WORKING STORAGE FOR MODULE)          
UNAMISS  DS    A                   A(MISSING UNIT RECORD)                       
UNINGTBL DS    A                   A(INTG TABLE)                                
UNLUPVPH DS    CL1                 FORCE LOOK UP OF NEW DEMO'S                  
UNBITOPT DS    CL1                 USE BITS FOR OPTIONS                         
*                                  BIT 0=ALLOW CHANGE IF PAYED                  
*                                  BIT 1=PACKAGE GUAR. EXITS                    
*                                  BIT 2=COPYU HUY TYPE LOOK UP DEMOS           
         DS    CL2                 SPARE                                        
*                                                                               
UNEDATA  DS    X                   DATA TYPE TO BE EDITED                       
UNERROR  DS    X                   ERROR NUMBER                                 
UNACTSW  DS    C                   ACTION SWITCH (A=ADD, C=CHANGE)              
UNESTLK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS              
*                                  ON CHANGE (Y/N)                              
UNNOLOOK DS    C                   SUPPRESS ESTIMATED LOOKUP ON CHANGE          
*                                  (Y/N) - FOR MISSING UNITS                    
UNBILLSW DS    C                   BILLED UNIT SWITCH (Y/N)                     
UNPAYSW  DS    C                   PAID UNIT SWITCH (Y/N)                       
UNACTION DS    X                   ACTION NUMBER (SEE EQUATES)                  
*                                                                               
UNDEMS   DS    0XL6                ESTIMATED HOMES DEMOS                        
UNSHR    DS    XL2                 SHARE                                        
UNHUT    DS    XL2                 HUT                                          
UNRAT    DS    XL2                 RATING                                       
*                                                                               
UNDATE   DS    XL2                 COMPRESSED AIR DATE                          
UNSQH    DS    X                   START QUARTER HOUR                           
*                                                                               
UNOVALS  DS    0XL5                ORIGINAL RECORD VALUES (POINTERS)            
UNODATE  DS    XL2                 ORIGINAL DATE                                
UNOSUB   DS    X                   ORIGINAL SUB-LINE                            
UNOTIME  DS    X                   ORIGINAL START QH                            
UNODAY   DS    X                   ORIGINAL DAY                                 
*                                                                               
         DS    CL(L'UNBLOCK-(*-UNBLOCK)) SPARE                                  
         SPACE                                                                  
* MODULE EQUATES                                                                
*                                                                               
UNITFIX  EQU   X'80'               UNITS MUST BE FIXED FOR THIS STATUS          
FROZEN   EQU   X'80'                                                            
LOCKED   EQU   X'20'                                                            
NOPRINT  EQU   X'10'                                                            
UNFROZEN EQU   X'FF'-X'80'                                                      
UNLOCKED EQU   X'FF'-X'20'                                                      
PRINT    EQU   X'FF'-X'10'                                                      
EQUAL    EQU   C'='                                                             
         SPACE 2                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENPLAN                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGENPLAN                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENPUA                                                                      
         PRINT ON                                                               
       ++INCLUDE NEGENPUA                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENUNIV                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENUNIV                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENMKG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENREP (REPRECD)                                                            
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NEWRI67   05/01/02'                                      
         END                                                                    
