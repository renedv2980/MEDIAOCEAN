*          DATA SET DELNK01    AT LEVEL 010 AS OF 01/30/13                      
*PHASE TF2F01A,*                                                                
*INCLUDE WRKIO                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE TWANG                                                                  
*INCLUDE DUMPOUT                                                                
*INCLUDE NSIWEEK                                                                
*                                                                               
* THIS VERISON IS A TEST VERION- DELETE WHE DONE                                
DELNK01  TITLE '- DEMO SYSTEM SERVER SUPPORT ROUTINES 1'                        
DELNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LK01**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
*        L     RA,ATWA                                                          
*        USING TWAD,RA                                                          
*        LA    R8,TWAD                                                          
         L     R8,=A(DBLOCK1-WORKD)    SET ADDRESSES OF I/O AREAS               
         LA    R8,WORKD(R8)                                                     
         USING DBLOCKD,R8                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
*        DROP  RA                                                               
                                                                                
         ST    RE,ROU1RELO         SAVE MY RELOCATION FACTOR                    
*&&DO                                                                           
         L     RE,=V(DUMPOUT)                                                   
         A     RE,ROU1RELO                                                      
         ST    RE,VDUMPOUT                                                      
*&&                                                                             
*                                                                               
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(WRKINI-DELNK01,0)                                            
         DC    AL2(VALPURE-DELNK01,0)                                           
         DC    AL2(VALBOOK-DELNK01,0)                                           
         DC    AL2(VALDAY-DELNK01,0)                                            
         DC    AL2(VALDEMO-DELNK01,0)                                           
         DC    AL2(VALTIME-DELNK01,0)                                           
         DC    AL2(VALBTYP-DELNK01,0)                                           
         DC    AL2(VAL1WK-DELNK01,0)                                            
         DC    AL2(VALWKN-DELNK01,0)                                            
         DC    AL2(VALPRINT-DELNK01,0)                                          
         DC    AL2(VALEFILT-DELNK01,0)                                          
         DC    AL2(IUNFILT-DELNK01,0)                                           
         DC    AL2(VALFUNCT-DELNK01,0)                                          
         DC    AL2(VALFILE-DELNK01,0)                                           
         DC    AL2(VALMED-DELNK01,0)                                            
         DC    AL2(VALSRC-DELNK01,0)                                            
         DC    AL2(VALTPTT-DELNK01,0)                                           
         DC    AL2(VALSTA-DELNK01,0)                                            
         DC    AL2(VALSTATP-DELNK01,0)                                          
         DC    AL2(VALAMKT-DELNK01,0)                                           
         DC    AL2(VALRMKT-DELNK01,0)                                           
         DC    AL2(VALKMKT-DELNK01,0)                                           
         DC    AL2(VALUMKT-DELNK01,0)                                           
         DC    AL2(VALPROG-DELNK01,0)                                           
         DC    AL2(VALINV-DELNK01,0)                                            
         DC    AL2(VALAGY-DELNK01,0)                                            
         DC    AL2(VALCLI-DELNK01,0)                                            
         DC    AL2(VALBEST-DELNK01,0)                                           
         DC    AL2(VALTAPE-DELNK01,0)                                           
         DC    AL2(VALPTT-DELNK01,0)                                            
         DC    AL2(SETUSRID-DELNK01,0)                                          
         DC    AL2(VALDYTIM-DELNK01,0)                                          
         DC    AL2(MULDYTIM-DELNK01,0)                                          
         DC    AL2(INITTSAR-DELNK01,0)                                          
         DC    AL2(READTSAR-DELNK01,0)                                          
         DC    AL2(GETTSAR-DELNK01,0)                                           
         DC    AL2(WRITTSAR-DELNK01,0)                                          
         DC    AL2(GETPROFL-DELNK01,0)                                          
         DC    AL2(PARSEUPG-DELNK01,0)                                          
         DC    AL2(VALUPGD-DELNK01,0)                                           
         DC    AL2(MULTBOOK-DELNK01,0)                                          
         DC    AL2(VALWEEK-DELNK01,0)                                           
         DC    AL2(TRNSBKT-DELNK01,0)                                           
         DC    AL2(VALRDEM-DELNK01,0)                                           
         DC    AL2(VALRCAT-DELNK01,0)                                           
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALISE WORKING STORAGE VARIABLES                                *         
***********************************************************************         
                                                                                
WRKINI   LR    RB,RF                                                            
         USING WRKINI,RB                                                        
         LA    R0,SVALUES          MOVE LITERALS TO W/S                         
         LHI   R1,SVALUESL                                                      
         L     RE,ALVALUES                                                      
         A     RE,ROU1RELO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,RELOLST          RELOCATE ADCONS                              
         LA    R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,ROU1RELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IOAREA1-WORKD    SET ADDRESSES OF I/O AREAS                   
         LA    RF,WORKD(RF)                                                     
         LA    R0,AIONM                                                         
         LA    R1,AIO1                                                          
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AHI   RF,IOLENQ                                                        
         AHI   R1,L'AIO1                                                        
         BCTR  R0,RE                                                            
                                                                                
         L     R1,AFACTAB                                                       
         USING FACTABD,R1          EXTRACT FACILITES LIST ADCONS                
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         SR    RF,RF                                                            
         SR    R2,R2                                                            
         ICM   R2,3,FACTDOUT                                                    
         LA    R2,WORKD(R2)                                                     
         SR    R3,R3                                                            
         ICM   R3,3,FACTDIN                                                     
         IC    RF,FACTFLST                                                      
         L     RF,FACLISTS(RF)                                                  
         AR    R3,RF                                                            
         LTR   RF,RF                                                            
         JZ    *+10                                                             
         MVC   0(4,R2),0(R3)                                                    
         AHI   R1,FACTABL                                                       
         BCTR  R0,RE                                                            
                                                                                
         L     R2,ACORPHS          R2=A(CORE PHASE LIST)                        
         LA    R3,APHASES          R3=A(CORE PHASE ADDRESS LIST)                
         LA    R4,CORPHSN          R4=CORE PHASE COUNT                          
         SR    R0,R0                                                            
         ICM   R0,14,T00A                                                       
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
WRKINI02 ICM   R0,1,0(R2)          TEST PHASE                                   
         BZ    WRKINI04            NONE, SKIP TO THE NEXT ENTRY                 
                                                                                
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
WRKINI04 AHI   R2,1                BUMP TO THE NEXT ENTRY                       
         AHI   R3,L'APHASES                                                     
         BCT   R4,WRKINI02                                                      
* MY OWN INITIALIZATION                                                         
         MVC   MAXIO,=F'500'       SET MAXIMUM DEMO TRACE COUNT                 
         L     RE,=A(SP00)                                                      
         L     RF,ROU1RELO         SAVE MY RELOCATION FACTOR                    
         AR    RE,RF                                                            
         ST    RE,ASP00                                                         
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         MVC   USERID,TWAUSRID                                                  
         MVC   AGYALPH,TWAAGY                                                   
         DROP  RE                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         MVC   OVSYS,FAOVSYS-FACTSD(R1)                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
ALVALUES DC    A(LVALUES)                                                       
         EJECT                                                                  
***************************************                                         
* VALIDATE PURE NUMBER                *                                         
***************************************                                         
VALPURE  LR    RB,RF                                                            
         USING VALPURE,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         MVC   DUB(4),=C'0000'                                                  
         MVZ   DUB(4),0(R2)                                                     
         CLC   DUB(4),=C'0000'                                                  
         JNE   EXITN                                                            
                                                                                
         PACK  DUB,0(2,R2)                                                      
         CVB   R1,DUB                                                           
         PACK  DUB,2(1,R2)                                                      
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         PACK  DUB,3(1,R2)                                                      
         CVB   RE,DUB                                                           
         STC   R1,0(R4)                                                         
         STC   R0,1(R4)                                                         
         STC   RE,2(R4)                                                         
         OC    1(1,R4),2(R4)                                                    
                                                                                
         J     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A BOOK REQUEST                                             *         
*  OUTPUT RECORD =BOOKTYPE(1),WEEK(1),VALID FLAG(1)                   *         
*                 BOOKVAL BK(3), MAX 40 BYTES INPUT BOOK STRING       *         
***********************************************************************         
                                                                                
VALBOOK  LR    RB,RF                                                            
         USING VALBOOK,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R4),0(R2)       RETURN INPUT BOOK STRING                     
                                                                                
         XC    OPTUPSBT,OPTUPSBT                                                
         XC    OPTWEEK,OPTWEEK                                                  
         XC    OPTUPBK,OPTUPBK                                                  
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=C'LATEST'  LATEST BOOK REQUEST                          
         JNE   VALBK02             YES - VALIDATE MULTI                         
VALBK01  MVC   3(3,R4),=X'000000'                                               
         J     VALBKY                                                           
VALBK02  CLC   0(3,R2),=C'LATEST'  LATEST BOOK REQUEST                          
         JNE   VALBK04             YES - VALIDATE MULTI                         
         CH    R3,=H'3'                                                         
         BNE   VALBK02A            IF ONLY  "LAT"                               
         B     VALBK01             THEN IT IS LATEST                            
                                                                                
VALBK02A XC    DUB(2),DUB          VALIDATE LATEST BOOK EXPRESSIONS             
         TM    3(R2),X'F0'         MULTI BOOK AVERAGE                           
         JNO   VALBKN                                                           
*&&DO                                                                           
         CLI   3(R2),C'1'          MUST BE AT LEAST 1 BUT                       
         JL    VALBKN                                                           
         CLI   3(R2),C'4'          NO MORE THAN 4                               
         JH    VALBKY                                                           
*&&                                                                             
***************************************************                             
*                                                                               
         LA    RF,1                 MASK OF 1                                   
         CHI   R3,4                                                             
         JE    *+16                                                             
         CHI   R3,5                                                             
         JH    EXITN                                                            
         LA    RF,3                 MASK OF 3                                   
         SR    RE,RE                                                            
         EX    RF,*+8                                                           
         J     *+8                                                              
         ICM   RE,0,3(R2)                                                       
         XC    DUB,DUB                                                          
         STCM  RE,15,DUB+8                                                      
         PACK  DUB,DUB+8(4)                                                     
         CVB   RE,DUB                                                           
         CHI   RE,1                                                             
         JL    EXITN                                                            
         CHI   RE,12                                                            
         JH    EXITN                                                            
*                                                                               
         MVI   4(R4),X'FF'         INDICATE LATEST N                            
**       MVC   5(1,R4),3(R2)                                                    
         STC   RE,5(R4)                                                         
         J     VALBKY                                                           
VALBK04  DS    0H                                                               
         XC    DUB(3),DUB                                                       
         DS    0H                  "STRIP OFF" ANY BOOKTYPE OR WEEK             
         LR    R0,R3                                                            
         OR    R0,R0                                                            
         BZ    VALBKN                                                           
         LR    RF,R2               INPUT STRING                                 
         SR    R1,R1                                                            
                                                                                
VALBK10  DS    0H                                                               
****     CLI   0(RF),C'('          INDICATES START OF BOOKTYPE INPUT            
****     JNE   VALBK11                                                          
****     MVC   0(1,R4),1(RF)         SAVE BOOKTYPE                              
****     MVC   OPTUPSBT,1(RF)                                                   
VALBK11  CLI   0(RF),C'-'          INDICATES START OF WEEK     INPUT            
         JNE   VALBK14                                                          
         MVC   1(1,R4),1(RF)         SAVE WEEK                                  
         MVC   OPTWEEK,1(RF)                                                    
         CLI   1(RF),C'W'                                                       
         BE    VALBK14                                                          
         CLI   1(RF),C'1'          WEEK NUMBER MUST BE 1-5                      
         BL    VALBKN                                                           
         CLI   1(RF),C'4'                                                       
         BH    VALBKN                                                           
                                                                                
VALBK14  CLC   0(2,R4),=X'0000'    ONCE WE HI '(', OR '-"                       
         JNE   VALBK15             STOP COUNTING -JUST PROCESS STRING           
         LA    R1,1(R1)            LENGTH OF BOOK STRIPPED                      
VALBK15  LA    RF,1(RF)                                                         
         BCT   R0,VALBK10                                                       
                                                                                
VALBK16  DS    0H                  R1 = L(PLAIN BOOK INPUT)                     
         XC    DUMBOOKH(L'DUMBOOKH+L'DUMBOOK),DUMBOOKH                          
         OR    R1,R1                                                            
         JZ    VALBKN                                                           
         LR    RE,R1                                                            
         STC   RE,DUMBOOKH+5                                                    
         MVI   DUMBOOKH,L'DUMBOOKH+L'DUMBOOK                                    
         LR    RE,R1                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMBOOK(0),0(R2)                                                 
         LA    R1,DUMBOOKH                                                      
         ST    R1,DMCB                                                          
         MVC   DMCB(1),DBSRC                                                    
         MVI   DMCB,C'N'           SET PROPER SOURCE - NIELSEN                  
         GOTOR VBOOKVAL,DMCB,,(1,DUB),(C'B',VSCANNER),DUB2,            +        
               (C'C',ACOMFACS)                                                  
         CLI   4(R1),1             TEST BOOK IS VALID                           
         JNE   VALBKN                                                           
* MOVE IN BOOKTYPE  FROM BOOKVAL                                                
         MVC   0(1,R4),DUB2      SAVE BOOKTYPE                                  
         MVC   OPTUPSBT,DUB2     BOOKTYPE FOR UPGRADE                           
         MVC   3(3,R4),DUB       SET VALUE IN OUTPUT FIELD                      
         MVC   OPTUPBK,DUB+1     DONT CARE SET UPGRADE BOOK                     
         J     VALBKY                                                           
VALBKY   DS    0H                                                               
         MVI   2(R4),C'Y'        RETURN VALID FLAG                              
         J     EXITY                                                            
                                                                                
VALBKN   DS    0H                                                               
         MVI   2(R4),C'N'        RETURN INVALID FLAG                            
         J     EXITY                                                            
                                                                                
         DROP  RB                                                               
         LTORG                                                                  
DUMBOOKH DS    CL8                                                              
DUMBOOK  DS    CL40                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE A WEEK REQUEST                                             *         
*  OUTPUT RECORD =BOOKTYPE(1),WEEK(1),VALID FLAG(1)                   *         
*                 BOOKVAL BK(3), MAX 40 BYTES INPUT BOOK STRING       *         
***********************************************************************         
                                                                                
VALWEEK  LR    RB,RF                                                            
         USING VALWEEK,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R4),0(R2)       RETURN INPUT BOOK STRING                     
                                                                                
         XC    OPTUPSBT,OPTUPSBT                                                
         XC    OPTWEEK,OPTWEEK                                                  
         XC    OPTUPBK,OPTUPBK                                                  
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=C'LATEST'  LATEST BOOK REQUEST                          
         JNE   VALWK02             YES - VALIDATE MULTI                         
VALWK01  MVC   3(3,R4),=X'000000'                                               
         J     VALWKY                                                           
VALWK02  CLC   0(3,R2),=C'LATEST'  LATEST BOOK REQUEST                          
         JNE   VALWK04             YES - VALIDATE MULTI                         
         CH    R3,=H'3'                                                         
         BNE   VALWK02A            IF ONLY  "LAT"                               
         B     VALWK01             THEN IT IS LATEST                            
                                                                                
VALWK02A XC    DUB(2),DUB          VALIDATE LATEST BOOK EXPRESSIONS             
         TM    3(R2),X'F0'         MULTI BOOK AVERAGE                           
         JNO   VALWKN                                                           
         CLI   3(R2),C'1'          MUST BE AT LEAST 1 BUT                       
         JL    VALWKN                                                           
         CLI   3(R2),C'4'          NO MORE THAN 4                               
         JH    VALWKY                                                           
         MVI   4(R4),X'FF'         INDICATE LATEST N                            
         MVC   5(1,R4),3(R2)                                                    
         J     VALWKY                                                           
VALWK04  DS    0H                                                               
         XC    DUB(3),DUB                                                       
         DS    0H                  "STRIP OFF" ANY BOOKTYPE OR WEEK             
         LR    R0,R3                                                            
         OR    R0,R0                                                            
         BZ    VALWKN                                                           
         LR    RF,R2               INPUT STRING                                 
         SR    R1,R1                                                            
                                                                                
VALWK10  DS    0H                                                               
         CLI   0(RF),C'('          INDICATES START OF BOOKTYPE INPUT            
         JNE   VALWK11                                                          
         MVC   0(1,R4),1(RF)         SAVE BOOKTYPE                              
         MVC   OPTUPSBT,1(RF)                                                   
VALWK11  CLI   0(RF),C'-'          INDICATES START OF WEEK     INPUT            
         JNE   VALWK14                                                          
         MVC   1(1,R4),1(RF)         SAVE WEEK                                  
         MVC   OPTWEEK,1(RF)                                                    
         CLI   1(RF),C'W'                                                       
         BE    VALWK14                                                          
         CLI   1(RF),C'1'          WEEK NUMBER MUST BE 1-5                      
         BL    VALWKN                                                           
         CLI   1(RF),C'4'                                                       
         BH    VALWKN                                                           
                                                                                
VALWK14  CLC   0(2,R4),=X'0000'    ONCE WE HI '(', OR '-"                       
         JNE   VALWK15             STOP COUNTING -JUST PROCESS STRING           
         LA    R1,1(R1)            LENGTH OF BOOK STRIPPED                      
VALWK15  LA    RF,1(RF)                                                         
         BCT   R0,VALWK10                                                       
                                                                                
VALWK16  DS    0H                  R1 = L(PLAIN BOOK INPUT)                     
         XC    DUMWEEKH(L'DUMWEEKH+L'DUMWEEK),DUMWEEKH                          
         OR    R1,R1                                                            
         JZ    VALWKNO                                                          
         LR    RE,R1                                                            
         STC   RE,DUMWEEKH+5                                                    
         MVI   DUMWEEKH,L'DUMWEEKH+L'DUMWEEK                                    
         LR    RE,R1                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMWEEK(0),0(R2)                                                 
         LA    R1,DUMWEEKH                                                      
         ST    R1,DMCB                                                          
* GET DATE IN CORRECT FORMAT FOR NSIWEEK CALL                                   
         GOTO1 VDATVAL,DMCB,(0,DUMWEEK),DUB2                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    VALWKNO                                                          
* CALL NSIWEEK TO VALIDATE DATE                                                 
         MVI   BYTE1,0                 SET START DAY TO DEFAULT                 
         CLC   =C'WTP',INPFIL                                                   
         BE    *+8                                                              
         MVI   BYTE1,1                 DEFAULT TO MONDAY                        
         GOTO1 VNSIWEEK,DMCB,DUB2,(BYTE1,VGETDAY),VADDAY,VDATCON                
         MVC   5(1,R4),0(R1)          WEEK                                      
         MVC   4(1,R4),4(R1)          YEAR                                      
                                                                                
VALWKY   MVI   2(R4),C'Y'        RETURN VALID FLAG                              
         J     EXITY                                                            
                                                                                
VALWKNO  MVI   2(R4),C'N'        RETURN INVALID FLAG                            
         J     EXITY                                                            
                                                                                
         DROP  RB                                                               
         LTORG                                                                  
DUMWEEKH DS    CL8                                                              
DUMWEEK  DS    CL40                                                             
         EJECT                                                                  
***********************************************************************         
* TRANSLATE BOOKTYPE ROUTINE                                                    
* INPUT: INPUT STRING                                                           
*        LENGTH                                                                 
*        OUTPUT AREA                                                            
*        INDICATOR: 12= CONVERT FROM 1 CHAR TO 2 CHAR BOOKTYPE                  
***********************************************************************         
                                                                                
TRNSBKT  LR    RB,RF                                                            
         USING TRNSBKT,RB                                                       
         LM    R2,R5,0(R1)                                                      
         CHI   R3,2                                                             
         JH    EXITN                                                            
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   R6,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            LENGTH OF EACH ENTRY                         
         CHI   R5,12               TRANSLATE 1 TO 2 CHAR BOOKTYPE?              
         JE    TRNSBT40                                                         
         USING SPBKTYPD,R6                                                      
                                                                                
TRNSBT10 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT20                                                         
         MVI   0(R4),X'FF'         IF INVALID BOOKTYPE THEN RETURN              
         J     TRNSBTX             PASS BACK INVALID INDICATOR                  
                                                                                
TRNSBT20 OC    0(2,R2),=X'4040'    PAD INPUT WITH SPACES                        
         CLC   SPBKTYPA,0(R2)      IS BOOKTYPE IN TABLE?                        
         JE    TRNSBT30                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT10                                                         
TRNSBT30 MVC   0(L'SPBKTYPN,R4),SPBKTYPN  MOVE INTERNAL DEMO BKTYP              
         J     TRNSBTX                                                          
                                                                                
                                                                                
*************************************************************                   
* TRANSLATE FROM 1 CHARACTER INTERNAL TO 2 CHARCTER FORMAT  *                   
*************************************************************                   
TRNSBT40 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT50                                                         
         MVI   0(R4),X'FF'         IF INVALID BOOKTYPE THEN RETURN              
         J     TRNSBTX             INVALID INDICATOR                            
                                                                                
TRNSBT50 CLC   SPBKTYPN,0(R2)      IS 1 CHARACTER BOOKTYPE IN TABLE?            
         JE    TRNSBT60                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT40                                                         
TRNSBT60 MVC   0(L'SPBKTYPA,R4),SPBKTYPA  MOVE 2 CHARCTER BOOKTYPE              
*                                                                               
TRNSBTX  J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
***********************************************************************         
* VALIDATE A DEMO FOR RADIO                                           *         
* OUTPUT = 3 BYTE DEMO CODE                                           *         
*          1 BYTE VALIDATION FLAG (Y/N)                               *         
*          20 BYTE INPUT FIELD RETURNED                               *         
***********************************************************************         
                                                                                
VALRDEM  LR    RB,RF                                                            
         USING VALRDEM,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         BCTR  R4,0                                                             
                                                                                
*   MOVE FIELD INTO DUMMY SCREEN FIELD                                          
         XC    RDUMDEMH(L'RDUMDEMH+L'RDUMDEM),RDUMDEMH                          
         LA    RE,L'RDUMDEMH+L'RDUMDEM                                          
         STC   RE,RDUMDEMH                                                      
         STC   R3,RDUMDEMH+5                                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RDUMDEM(0),0(R2)                                                 
VALRDEM6 MVI   DBDEMTYP,0                                                       
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
         GOTO1 VDEMOVAL,DMCB,RDUMDEMH,(1,(R4)),DBLOCK                           
         CLI   4(R1),0                                                          
         JNE   VALRDEMY                                                         
         J     VALRDEMN                                                         
         SPACE 1                                                                
                                                                                
VALRDEMY MVI   3(R4),C'Y'                                                       
         MVI   BYTE1,C'S'                 SPOTPAK VALIDATION                    
         XC    DBLOCK,DBLOCK              SET DBLOCK FOR DEMOCON                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
***      MVI   DBSELMED,C'T'                                                    
***      MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
         LA    R7,0(R4)                                                         
         LA    R0,4(R4)                                                         
         GOTO1 VDEMOCON,DMCB,(1,(R7)),(6,(R0)),(BYTE1,DBLOCKD),0                
         J     EXITY                                                            
                                                                                
VALRDEMN MVI   3(R4),C'N'                                                       
         LR    RE,R3               PASS BACK INPUT STRING                       
         AHI   RE,-1               IF INPUT IS INVALID                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),0(R2)                                                    
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
                                                                                
RDUMDEMH DS    CL8                 NEED DUMMY FIELDS FOR DEMOVAL                
RDUMDEM  DS    CL50                                                             
***********************************************************************         
*  VALIDATE MSA,ADI,TSA CATEGORY FOR RADIO                                      
***********************************************************************         
                                                                                
VALRCAT  LR    RB,RF                                                            
         USING VALRCAT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         USING VDIOUTD,R4                                                       
         USING VDIINPD,R2                                                       
         CHI   R3,3                MUST BE LENGTH OF 3!!                        
         JNE   EXITN                                                            
         CLC   0(3,R2),=C'MSA'                                                  
         JNE   *+8                                                              
         MVI   VDIODMIN,X'01'                                                   
         CLC   0(3,R2),=C'TSA'                                                  
         JNE   *+8                                                              
         MVI   VDIODMIN,X'02'                                                   
         CLC   0(3,R2),=C'ADI'                                                  
         JNE   *+8                                                              
         MVI   VDIODMIN,X'03'                                                   
*                                                                               
         CLI   VDIODMIN,0          DID THEY ENTER SOMETHING VALID?              
         JE    EXITN               NO!!                                         
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
*                                                                               
VDIINPD  DSECT                                                                  
VDIIDMIN DS    CL3                                                              
                                                                                
VDIOUTD  DSECT                                                                  
VDIODMIN DS    X                                                                
VDIOUTL  EQU   *-VDIOUTD                                                        
                                                                                
DELNK01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DEMO FOR USTV                                            *         
* OUTPUT = 3 BYTE DEMO CODE                                           *         
*          1 BYTE VALIDATION FLAG (Y/N)                               *         
*          20 BYTE INPUT FIELD RETURNED                               *         
***********************************************************************         
                                                                                
VALDEMO  LR    RB,RF                                                            
         USING VALDEMO,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
                                                                                
*   MOVE FIELD INTO DUMMY SCREEN FIELD                                          
         XC    DUMDEMOH(L'DUMDEMOH+L'DUMDEMO),DUMDEMOH                          
         LA    RE,L'DUMDEMOH+L'DUMDEMO                                          
         STC   RE,DUMDEMOH                                                      
         STC   R3,DUMDEMOH+5                                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMDEMO(0),0(R2)                                                 
VALDEMO6 MVI   DBDEMTYP,0                                                       
         GOTO1 VDEMOVAL,DMCB,DUMDEMOH,(1,(R4)),DBLOCK                           
         CLI   4(R1),0                                                          
         JNE   VALDEMY                                                          
         J     VALDEMN                                                          
         SPACE 1                                                                
                                                                                
VALDEMY  MVI   3(R4),C'Y'                                                       
*                                                                               
         CLI   1(R4),C'R'                ONLY RATINGS, IMPRESSIONS              
         BE    VALDEMY5                  ARE ALLOWED                            
         CLI   1(R4),C'D'                                                       
         BE    VALDEMY5                                                         
         CLI   1(R4),C'T'                                                       
         BE    VALDEMY5                                                         
         J      VALDEMN                                                         
*                                                                               
VALDEMY5 CLI   1(R4),C'T'                REPLACE T'S WITH I                     
         JNE   *+8                       BEFORE DEMOCON                         
         MVI   1(R4),C'I'                                                       
         MVI   BYTE1,C'S'                 SPOTPAK VALIDATION                    
* PROPSER WANTS THE DEMO NAME TO BE AS INPUTTED                                 
         L     RE,ALP                                                           
         USING LP_D,RE                                                          
         CLC   LP_VRSN1,=AL4(PPVER30)                                           
         BNL   VALDEM8                                                          
         DROP  RE                                                               
         XC    DBLOCK,DBLOCK              SET DBLOCK FOR DEMOCON                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
         LA    R7,0(R4)                                                         
         LA    R0,4(R4)                                                         
         GOTO1 VDEMOCON,DMCB,(1,(R7)),(6,(R0)),(BYTE1,DBLOCKD),0                
         J     EXITY                                                            
                                                                                
VALDEMN  MVI   3(R4),C'N'                                                       
VALDEM8  LR    RE,R3               PASS BACK INPUT STRING                       
         AHI   RE,-1               IF INPUT IS INVALID                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),0(R2)                                                    
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
                                                                                
PPVER30  EQU   X'03000000'                                                      
DUMDEMOH DS    CL8                 NEED DUMMY FIELDS FOR DEMOVAL                
DUMDEMO  DS    CL50                                                             
*                                  LIST OF DEMOS FOR XALL                       
ALLDEMOS DC    AL1(001,002,003,025,028,029,030,031,032,033)                     
         DC    AL1(040,041,042,043,044,045,046,048,049,050)                     
         DC    AL1(051,052,053,054,055,056,057,058,059,060)                     
         DC    AL1(065,075,078,079,080,081,082,083,090,091)                     
         DC    AL1(092,093,094,095,096,097,098,099,100,101)                     
         DC    AL1(102,103,104,105,106,107,108,109,110,121)                     
         DC    AL1(122,123,125,127,128,129,130,131,132,133)                     
         DC    AL1(140,141,142,143,144,145,146,147,148,149)                     
         DC    AL1(150,151,152,153,154,155,156,157,158,159)                     
         DC    AL1(160,172,173,174,175,176,177,179,180,181)                     
         DC    AL1(182,183,184,185,255,000,000,000,000,000)                     
         DC    AL1(000,000,000,000,000,000,000,000,000,000)                     
         SPACE 1                                                                
*****************************************                                       
* VALIDATE DAY EXPRESSION               *                                       
*****************************************                                       
                                                                                
VALDAY   LR    RB,RF                                                            
         USING VALDAY,RB                                                        
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         J     EXITY                                                            
         SPACE 1                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*****************************************                                       
*  VALIDATE TIME(S)                     *                                       
*****************************************                                       
VALTIME  LR    RB,RF                                                            
         USING VALTIME,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
                                                                                
         J     EXITY                                                            
         SPACE 1                                                                
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*****************************************                                       
* VALIDATE BOOKTYPE                     *                                       
*****************************************                                       
VALBTYP  LR    RB,RF                                                            
         USING VALBTYP,RB                                                       
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         CHI   R3,1                                                             
         BE    VBT1CH                                                           
         CHI   R3,2                                                             
         BE    VBT2CH                                                           
         J     EXITN                                                            
                                                                                
VBT1CH   DS    0H                  VALIDATE FOR 1-CHAR INPUT                    
         MVC   0(1,R4),0(R2)         TAKE INPUT AS IS                           
         B     VBTX                                                             
                                                                                
VBT2CH   DS    0H                  VALIDATE FOR 2-CHAR INPUT                    
         LR    RE,R2                                                            
         LA    RF,2                                                             
VBT05    CLI   0(RE),X'C1'          VALID HEXIDECIMAL ?                         
         JL    EXITN                                                            
         CLI   0(RE),X'C6'                                                      
         BNH   VBT06                A-F                                         
                                                                                
         CLI   0(RE),X'F0'                                                      
         JL    EXITN                                                            
         CLI   0(RE),X'F9'                                                      
         JH    EXITN                                                            
VBT06    AHI   RE,1                                                             
         BCT   RF,VBT05                                                         
VBT10    DS    0H                                                               
         GOTO1 VHEXIN,DMCB,0(R2),(R4),2,0                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     VBTX                                                             
VBTX     DS    0H                                                               
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*****************************************                                       
* VALIDATE ACTUAL DATE (1WEEK OPTION)   *                                       
*****************************************                                       
VAL1WK   LR    RB,RF                                                            
         USING VAL1WK,RB                                                        
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         XC    MYEFFDTH,MYEFFDTH                                                
         XC    MYEFFDT,MYEFFDT                                                  
         LR    RE,R3                                                            
         STC   R3,12(R4)            SAVE LENGTH OF INPUT STRING                 
         STC   RE,MYEFFDTH+5                                                    
         MVI   MYEFFDTH,L'MYEFFDTH+L'MYEFFDT                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   MYEFFDT(0),0(R2)                                                 
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO2                                                          
         GOTO1 VSCANNER,DMCB,MYEFFDTH,(R0),C',=,-'                              
         L     R6,AIO2                                                          
         USING SCANBLKD,R6                                                      
                                                                                
         ZIC   RE,SC1STLEN                                                      
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MYEFFDT(0),SC1STFLD                                              
         ZIC   R0,SC1STLEN                                                      
                                                                                
         LA    R5,SC1STFLD                                                      
         XC    0(4,R4),0(R4)                                                    
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(0,(R5)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         JZ    EXITY                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(2,(R4))                                   
         LA    R5,SC2NDFLD                                                      
         GOTO1 VDATVAL,DMCB,(0,(R5)),WORK+6                                     
         OC    0(4,R1),0(R1)                                                    
         JZ    EXITY                                                            
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,2(R4))                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
MYEFFDTH DS    CL8                                                              
MYEFFDT  DS    CL20                                                             
         EJECT                                                                  
         SPACE 1                                                                
         DROP  R6                                                               
*****************************************                                       
* VALIDATE ACTUAL WEEK (1 THRU 4 OR ALL)*                                       
*****************************************                                       
VALWKN   LR    RB,RF                                                            
         USING VALWKN,RB                                                        
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
         CLC   0(2,R2),=C'ALL'                                                  
         BNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     EXITY                                                            
         MVI   DUB,C'0'        VALID NUMERIC                                    
         MVZ   DUB(1),0(R2)                                                     
         CLI   DUB,C'0'                                                         
         JNE   EXITN                                                            
                                                                                
         CHI   R3,1                                                             
         JNE   EXITN                                                            
         MVC   0(1,R4),0(R2)                                                    
         NI    0(R4),X'0F'                                                      
         J     EXITY                                                            
         SPACE 1                                                                
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
         SPACE 1                                                                
***********************************                                             
* VALIDATE PRINT OPTION           *                                             
***********************************                                             
VALPRINT LR    RB,RF                                                            
         USING VALPRINT,RB                                                      
         LM    R2,R4,0(R1)                                                      
                                                                                
         CLI   0(R2),PKEYONLY        KEYS ONLY                                  
         BE    *+12                                                             
         CLI   0(R2),PDEFAULT        DEFAULT TO PRINT ENTIRE RECORD             
         JNE   EXITN                                                            
         MVC   PCTRL2,0(R2)                                                     
         J     EXITY                                                            
         SPACE 1                                                                
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************                                             
* VALIDATE ELEMENT FILTER LIST    *                                             
***********************************                                             
VALEFILT LR    RB,RF                                                            
         USING VALEFILT,RB                                                      
         LM    R2,R4,0(R1)                                                      
                                                                                
         CLI   PCTRL2,C' '         CAN NOT HAVE BOTH PRINT?                     
         JNE   EXITN                AND ELEM FILTER FIELD                       
                                                                                
         L     RE,AIO1             USE IO FIELD AS A SCAN BLOCK                 
         LA    RF,2000                                                          
         XCEF                                                                   
         LR    RE,R3                                                            
         STC   RE,DUMEFLTH+5                                                    
         MVI   DUMEFLTH,L'DUMEFLTH+L'DUMEFLT                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMEFLT(0),0(R2)                                                 
                                                                                
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMEFLTH,(7,(R0)),C',=,-'                          
         CLI   4(R1),0                                                          
         JE    EXITN                                                            
         L     R5,AIO1                                                          
VALEFLT2 CLI   0(R5),0             MUST HAVE SOMETHING IN 1ST FIELD             
         JE    EXITN                                                            
         CLI   0(R5),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         JH    EXITN                                                            
         TM    2(R5),X'20'          MUST BE VALID HEX                           
         JZ    EXITN                                                            
         ZIC   R6,0(R5)            R4 = L(ENTRY)                                
         GOTO1 VHEXIN,DMCB,12(R5),0(R4),(R6),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         JNZ   EXITN                                                            
         MVC   1(1,R4),0(R4)       FILL IN DEFAULT END OF RANGE                 
                                                                                
         CLI   1(R5),0             IS THERE A SECOND HALF?                      
         BE    VALEFLT4             NOPE                                        
         CLI   1(R5),2              YEP                                         
         JH    EXITN                                                            
         TM    3(R5),X'20'         MUST BE VALID HEX                            
         JZ    EXITN                                                            
         ZIC   R6,1(R5)                                                         
         GOTO1 VHEXIN,DMCB,22(R5),1(R4),(R6),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         JNZ   EXITN                                                            
         CLC   0(1,R4),1(R4)       MAKE SURE LO <= HI                           
         JH    EXITN                                                            
                                                                                
VALEFLT4 LA    R5,32(R5)           DO NEXT ENTRY                                
         J     EXITY                                                            
         DROP  RB                                                               
DUMEFLTH DS    CL8                                                              
DUMEFLT  DS    CL20                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
************************************                                            
* VALIDATE IUN FILTER              *                                            
************************************                                            
IUNFILT  LR    RB,RF                                                            
         USING IUNFILT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         MVI   IUNFLG,C'Y'                                                      
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
                                                                                
         LR    RE,R3                                                            
         STC   RE,DUMIUNFH+5                                                    
         MVI   DUMIUNFH,L'DUMIUNFH+L'DUMIUNF                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMIUNF(0),0(R2)                                                 
                                                                                
         CLI   DUMIUNFH+5,1                                                     
         BNE   IUNFLT1                                                          
         CLI   DUMIUNF,C'K'        ONLY PRINT KEY?                              
         BNE   *+12                                                             
         MVI   PCTRLI,C'K'                                                      
         J     EXITY                                                            
         CLI   DUMIUNF,C'Y'        CONVERT TO IUN FORMAT?                       
         JE    EXITY                                                            
                                                                                
IUNFLT1  L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMIUNFH,(7,(R0)),C',=,-'                          
         CLI   4(R1),0                                                          
         JE    EXITN                                                            
         ZIC   R0,4(R1)            R0 = # OF ENTRIES                            
         L     R7,AIO1                                                          
         LA    R5,IUNFLST+1                                                     
                                                                                
IUNFLT2  CLI   0(R7),0             MUST HAVE SOMETHING IN 1ST FIELD             
         JE    EXITN                                                            
         CLI   0(R7),2              MAX OF 2 HEX DIGITS IN ELEM CODE            
         JH    EXITN                                                            
         TM    2(R7),X'20'          MUST BE VALID HEX                           
         JZ    EXITN                                                            
         ZIC   R0,0(R7)            R4 = L(ENTRY)                                
         GOTO1 VHEXIN,DMCB,12(R7),0(R4),(R0),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         JNZ   EXITN                                                            
         MVC   1(1,R4),0(R4)       FILL IN DEFAULT END OF RANGE                 
                                                                                
         CLI   1(R7),0             IS THERE A SECOND HALF?                      
         BE    IUNFLT4              NOPE                                        
         CLI   1(R7),2              YEP                                         
         JH    EXITN                                                            
         TM    3(R7),X'20'         MUST BE VALID HEX                            
         JZ    EXITN                                                            
         ZIC   R0,1(R7)                                                         
         GOTO1 VHEXIN,DMCB,22(R7),1(R4),(R0),0                                  
         ICM   RE,15,12(R1)        RE = L(OUTPUT)                               
         BCTR  RE,0                 AND IT SHOULD BE 1                          
         LTR   RE,RE                                                            
         JNZ   EXITN                                                            
         CLC   0(1,R4),1(R4)       MAKE SURE LO <= HI                           
         JH    EXITN                                                            
                                                                                
IUNFLT4  DS    0H                                                               
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
DUMIUNFH DS    CL8                                                              
DUMIUNF  DS    CL20                                                             
         EJECT                                                                  
***********************************************************************         
*   PARSE OUT STRING OF BOOKS AND CALL BOOKS VALIDATION ROUTINE                 
*  OUTPUT RECORD =BOOKTYPE(1),WEEK(1),VALID FLAG(1)                   *         
*                 BOOKVAL BK(3), 3 MORE BOOKS FOR MULTIBOOK(6 BYTES)  *         
*                 MAX 40 BYTES INPUT BOOK STRING                      *         
*                                                                               
***********************************************************************         
MULTBOOK LR    RB,RF                                                            
         USING MULTBOOK,RB                                                      
         LM    R2,R4,0(R1)                                                      
         LR    R7,R4                SAVE ADDRESS OF BEGINING OF OUTPUT          
         XC    MYBOOKSH,MYBOOKSH    BUFFER                                      
         XC    MYBOOKS,MYBOOKS                                                  
         XC    SVBKTYP,SVBKTYP                                                  
         MVI   NUMBKS,1                                                         
         LR    RE,R3                                                            
         STC   R3,12(R4)            SAVE LENGTH OF INPUT STRING                 
         STC   RE,MYBOOKSH+5                                                    
******   MVI   MYBOOKSH,L'MYBOOKSH+L'MYBOOKS                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   MYBOOKS(0),0(R2)                                                 
         LA    RE,8+1(RE)                                                       
         STC   RE,MYBOOKSH+0                                                    
*                                                                               
         MVC   12(40,R4),MYBOOKS     PASS BACK INPUT STRING                     
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO2                                                          
         GOTO1 VSCANNER,DMCB,MYBOOKSH,(R0),C',=+ '                              
         L     R6,AIO2                                                          
         USING SCANBLKD,R6                                                      
                                                                                
MBOOK10  ZIC   RE,SC1STLEN                                                      
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   MYBOOKS(0),SC1STFLD                                              
                                                                                
         ZIC   R0,SC1STLEN                                                      
                                                                                
         LA    RF,SC1STFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC1STLEN                                                      
         OR    RF,RF                                                            
         JZ    MBOOK20                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
                                                                                
         CLC   =C'WTP',INPFIL                    OVERNIGHTS                     
         JE    MBOOK11                                                          
         CLC   =C'OPA',INPFIL                                                   
         JE    MBOOK11                                                          
         CLC   =C'OTP',INPFIL                                                   
         JNE   MBOOK12                                                          
MBOOK11  GOTOR (#VALWEEK,AVALWEEK),DMCB                                         
         J     MBOOK13                                                          
                                                                                
*                                                                               
MBOOK12  GOTOR (#VALBOOK,AVALBOOK),DMCB                                         
                                                                                
MBOOK13  CLI   NUMBKS,1                           IF MULTIBOOKS THEN            
         JH    MBOOK15                            WE CANT HAVE WEEKS            
         MVC   0(6,R4),WORK                                                     
*                                                                               
         MVC   SVBKTYP,WORK                                                     
MBOOK14  AHI   R4,6                                                             
         AHI   R6,L'SCLINE                                                      
         ZIC   RE,NUMBKS                                                        
         AHI   RE,1                                                             
         STC   RE,NUMBKS                                                        
         CLI   0(R6),0                            IF NO MORE LINES              
         JE    MBOOK20                                                          
         J     MBOOK10                                                          
                                                                                
*  MULTIBOOK CODE                                                               
MBOOK15  MVC   0(2,R4),WORK+4                     2 BYTE BOOKS                  
         OC    WORK+1(1),WORK+1                   CANT HAVE WEEKS               
         JZ    MBOOK16                                                          
         MVI   2(R7),C'N'                         FOR MULTIBOOK                 
         J     EXITY                                                            
MBOOK16  DS    0H                                                               
* COMMENT OUT- BOOKTYPE FOR MULTIBOOK AVG WILL ALWAYS BE 1ST BOOKTYPE           
* IN STRING                                                                     
***      CLC   SVBKTYP,WORK                       BOOKTYPE HAS TO               
***      JE    MBOOK18                            BE CONSISTENT                 
***      MVI   2(R7),C'N'                         FOR MULTIBOOK                 
***      J     EXITY                                                            
MBOOK18  AHI   R4,2                               REST OF BOOKS                 
         AHI   R6,L'SCLINE                        FOR MULTIBOOK                 
         ZIC   RE,NUMBKS                          ONLY NEEDS                    
         AHI   RE,1                               3 BYTE BOOK                   
         STC   RE,NUMBKS                                                        
         CLI   0(R6),0                                                          
         JE    MBOOK20                                                          
         J     MBOOK10                                                          
                                                                                
MBOOK20  J     EXITY                                                            
         LTORG                                                                  
         DROP  RB,R6                                                            
MYBOOKSH DS    CL8                                                              
MYBOOKS  DS    CL100                                                            
SVBKTYP  DS    CL1                                                              
SVWEEK   DS    CL1                                                              
NUMBKS   DS    XL1                                                              
                                                                                
********************************************************************            
*    PARSE OUT STRING OF DAYTIMES AND CALL DAYTIME VALIDATION ROUTINE           
*    OUTPUT= (STRING OF 5 BYTE DAY/TIME)                                        
********************************************************************            
MULDYTIM LR    RB,RF                                                            
         USING MULDYTIM,RB                                                      
         LM    R2,R4,0(R1)                                                      
         XC    MYDYTIMH,MYDYTIMH                                                
         XC    MYDYTIM,MYDYTIM                                                  
         LR    RE,R3                                                            
         STC   R3,12(R4)            SAVE LENGTH OF INPUT STRING                 
         STC   RE,MYDYTIMH+5                                                    
         MVI   MYDYTIMH,L'MYDYTIMH+L'MYDYTIM                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   MYDYTIM(0),0(R2)                                                 
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO2                                                          
         GOTO1 VSCANNER,DMCB,MYDYTIMH,(R0),C',=+'                               
         L     R6,AIO2                                                          
         USING SCANBLKD,R6                                                      
                                                                                
MDYTM10  ZIC   RE,SC1STLEN                                                      
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MYDYTIM(0),SC1STFLD                                              
         ZIC   R0,SC1STLEN                                                      
                                                                                
         LA    RF,SC1STFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC1STLEN                                                      
         OR    RF,RF                                                            
         BZ    MDYTM20                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
                                                                                
         GOTOR (#VALDYTM,AVALDYTM),DMCB                                         
                                                                                
         MVC   0(5,R4),WORK+25                                                  
         AHI   R4,5                                                             
         AHI   R6,L'SCLINE                                                      
         J     MDYTM10                                                          
MDYTM20  MVI   0(R4),X'FF'                                                      
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB,R6                                                            
MYDYTIMH DS    CL8                                                              
MYDYTIM  DS    CL230                                                            
         EJECT                                                                  
***********************************************************************         
VALFUNCT LR    RB,RF                                                            
         USING VALFUNCT,RB                                                      
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALFUNCT-DELNK01)        PASS SP00 DISPLACEMENT            
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE UPGRADE EXPRESSION                                                   
* ENTRY R2=STRING TO PASS TO UPVAL                                              
*                                                                               
* EXIT  1 BYTE Y/N FOR UPGRADE VALIDATION STATUS                                
*       AND UPGRADE VALUES PASSED TO WORK AREA STORAGE                          
***********************************************************************         
VALUPGD  LR    RB,RF                                                            
         USING VALUPGD,RB                                                       
         LM    R2,R4,0(R1)                                                      
         XC    MYPROJH,MYPROJH                                                  
         XC    MYPROJ,MYPROJ                                                    
         LR    RE,R3                                                            
         STC   RE,MYPROJH+5                                                     
         AHI   RE,8                                                             
         STC   RE,MYPROJH                                                       
         LR    RE,R3                                                            
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MYPROJ(0),0(R2)                                                  
         GOTO1 VUPVAL,DMCB,MYPROJH,WORK+20,(C'/',ACOMFACS)                      
         CLI   0(R1),0             TEST FOR ERRORS, SHOULD NEVER HAVE           
*****    BNE   *+6                 ERRORS BECAUSE USER CANT TYPE IN             
*****    DC    H'0'                PROJECTIONS                                  
         BE    VALUP30                                                          
***      MVC   0(1,R4),WORK+23                                                  
***      MVC   1(8,R4),WORK+24                                                  
         MVC   OPTUPBT,WORK+23                                                  
         MVC   OPTUPGD,WORK+24                                                  
         MVI   OPTVALID,C'Y'                                                    
         J     EXITY                                                            
VALUP30  MVI   OPTVALID,C'N'                                                    
         J     EXITY                                                            
         LTORG                                                                  
MYPROJH  DS    CL8                                                              
MYPROJ   DS    CL40                                                             
         DROP  RB                                                               
***********************************************************************         
*    PARSE OUT STRING OF UPGRADE AND FILL IN OUTPUT BUFFER                      
*    FOR UPGRADE INPUT                                                          
*  CALLS APPROPRIATE ROUTINES AND PASSES BACK INTO R4 OUTPUT                    
* EXIT  R4= (1 BYTE BOOKTYPE)                                                   
*           (8 BYTES UPGRADE VALUES)                                            
*           (2 BYTES SHR BOOK)                                                  
*           (5 BYTES ALT DAY TIME)                                              
*           (C'Y' FOR 2 YEAR PUT)                                               
*           (C'Y' FOR 2 YEAR RATING YEARS)                                      
*           (Y/N  STATUS FLAG            )                                      
*                                                                               
***********************************************************************         
PARSEUPG LR    RB,RF                                                            
         USING PARSEUPG,RB                                                      
         LM    R2,R4,0(R1)                                                      
                                                                                
         XC    MYUPGRH,MYUPGRH                                                  
         XC    MYUPGR,MYUPGR                                                    
         XC    OPTUPGD,OPTUPGD                                                  
         XC    OPTUPBK,OPTUPBK                                                  
         XC    OPTUPSBT,OPTUPSBT                                                
         XC    OPTUPBT,OPTUPBT                                                  
         XC    OPTUPDT,OPTUPDT                                                  
         XC    OPT2YRR,OPT2YRR                                                  
         XC    OPT2YRP,OPT2YRP                                                  
         XC    OPTVALID,OPTVALID                                                
         LR    RE,R3                                                            
         STC   R3,12(R4)            SAVE LENGTH OF INPUT STRING                 
         STC   RE,MYUPGRH+5                                                     
         MVI   MYUPGRH,L'MYUPGRH+L'MYUPGR                                       
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   MYUPGR(0),0(R2)                                                  
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO2                                                          
         GOTO1 VSCANNER,DMCB,(30,MYUPGRH),(R0),C',=,='                          
         L     R6,AIO2                                                          
         USING SCANBLKD,R6                                                      
* NOW CHECK UOGRADE TABLE TO FIND KEYWORDS                                      
PARS08   L     R7,APROJTAB                                                      
         USING PROJTABD,R7                                                      
PARS10   CLI   0(R7),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROJKEYW,SC1STFLD                                                
         JNE   PARSE30                                                          
         J     PARSE40                                                          
PARSE30  AHI   R7,L'PROJTAB                                                     
         J     PARS10                                                           
* PROCESS THE KEYWORD                                                           
PARSE40  DS    0H                                                               
* IF THERE IS A ROUTINE TO GO TO THEN SET UP PARAMETERS TO THE ROUTINES         
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                            GET RID OF  UPP=              
         ZIC   RF,SC2NDLEN                                                      
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         MVI   DMCB+12,C'P'                       SET FLAG WHERE                
*                                                 THE CALL IS FROM              
         XC    WORK,WORK                                                        
         LA    R1,DMCB                                                          
         L     RF,AVALUPGD                                                      
         ICM   RF,8,PROJADDR                                                    
         CLI   PROJADDR,0                                                       
         BE    PARSE50                                                          
         BASR  RE,RF                                                            
         B     PARSE60                                                          
PARSE50  ZICM  RE,PROJSDIS,(3)                    IF RTG OR PUT                 
         LA    RE,WORKD(RE)                                                     
         MVI   0(RE),C'Y'                                                       
                                                                                
                                                                                
PARSE60  AHI   R6,52                                                            
         CLI   0(R6),0                                                          
         JNE   PARS08                                                           
         DROP  R7                                                               
         MVC   0(L'OPTUPBT,R4),OPTUPBT                                          
         OC    OPTUPBT,OPTUPBT                                                  
         BNZ   *+10                                                             
         MVC   0(L'OPTUPSBT,R4),OPTUPSBT                                        
         MVC   1(L'OPTUPGD,R4),OPTUPGD                                          
         MVC   9(L'OPTUPBK,R4),OPTUPBK                                          
         MVC   11(L'OPTUPDT,R4),OPTUPDT                                         
         MVC   16(L'OPT2YRP,R4),OPT2YRP                                         
         MVC   17(L'OPT2YRR,R4),OPT2YRR                                         
         MVC   18(L'OPTWEEK,R4),OPTWEEK                                         
         MVC   19(L'OPTVALID,R4),OPTVALID               STATUS FLAG             
         J     EXITY                                                            
         LTORG                                                                  
MYUPGRH  DS    CL8                                                              
MYUPGR   DS    CL230                                                            
         DROP  RB,R6                                                            
***********************************************************************         
*    OUTPUT = 3 BYTES OF FILE CODE - C'TT ' C'PAV' C'T4 '                       
***********************************************************************         
VALFILE  LR    RB,RF                                                            
         USING VALFILE,RB                                                       
         LM    R2,R4,0(R1)                                                      
         XC    0(3,R4),0(R4)                                                    
         LR    RE,R3                                                            
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                 3 BYTES MAX                        
                                                                                
         L     RF,ASP00                                                         
         L     R0,=A(VALFILE-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         XC    0(3,R4),0(R4)                                                    
         MVC   0(3,R4),0(R2)                 3 BYTES MAX                        
         MVC   INPFIL,0(R2)                                                     
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALMED   LR    RB,RF                                                            
         USING VALMED,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALMED-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALSRC   LR    RB,RF                                                            
         USING VALSRC,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALSRC-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALTPTT  LR    RB,RF                                                            
         USING VALTPTT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALTPTT-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE  STATION AND PASS BACK IN THE STATION WORK AREA                      
*11 BYTES MAX STATION FROM INPUT AND 1 BYTE C'Y/N' FOR STATUS                   
* 1 BYTES VALID STRING STATUS                                                   
* 1 BYTE LENGTH OF THE STRING INPUTTED                                          
* 1 BYTE SPILL MARKET INDICATOR  (Y/N)                                          
***********************************************************************         
VALSTA   LR    RB,RF                                                            
         USING VALSTA,RB                                                        
         LM    R2,R4,0(R1)                                                      
         XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         LR    RE,R3                                                            
         STC   R3,12(R4)            SAVE LENGTH OF INPUT STRING                 
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),0(R2)                                                 
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   0(11,R4),DUMSTAT                                                 
                                                                                
         MVI   11(R4),C'N'                   DEFAULT TO INVALID                 
* VERIFY THE STATION INPUT SYNTAX IS CORRECT                                    
                                                                                
         CLI   SC1STVAL,X'80'               VALIDITY SETTING OF FIELD 1         
         BNE   VALSTA10                     NUMERIC FIELD 1-4                   
         MVI   MINLEN,1                                                         
         MVI   MAXLEN,4                                                         
         B     VALSTA20                                                         
VALSTA10 MVI   MINLEN,2                                                         
         MVI   MAXLEN,5                                                         
                                                                                
VALSTA20 CLC   SC1STLEN,MINLEN                                                  
         BL    EXITY                                                            
         CLC   SC1STLEN,MAXLEN                                                  
         BH    EXITY                                                            
*  SPILL MARKET  HAS TO BE EITHER NUMERIC OR ALPHA NOT A MIXTURE                
         MVI   13(R4),C'N'               DEFUALT NO SPILL MARKET                
         CLI   SC2NDLEN,0                                                       
         BE    VALSTA40                                                         
         MVI   13(R4),C'Y'               YES THERE WAS A SPILL ENTERED          
         CLI   SC2NDVAL,X'80'+X'20'      NUMERIC /HEX                           
         BE    VALSTA40                                                         
         CLI   SC2NDVAL,X'40'            APLHA ONLY                             
         BE    VALSTA40                                                         
         CLI   SC2NDVAL,X'40'+X'20'      APLHA /HEX                             
         BE    VALSTA40                                                         
         J     EXITY                                                            
                                                                                
VALSTA40 MVI   11(R4),C'Y'                                                      
         J     EXITY                                                            
         DROP  R1                                                               
         DROP  RB                                                               
         LTORG                                                                  
DUMSTATH DS    CL8                                                              
DUMSTAT  DS    CL20                                                             
MAXLEN   DS    X                                                                
MINLEN   DS    X                                                                
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALSTATP LR    RB,RF                                                            
         USING VALSTATP,RB                                                      
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALSTATP-DELNK01)        PASS SP00 DISPLACEMENT            
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*VALIDATE MARKET AND RETURN NUMERIC MARKET CODE TO CALLER                       
*PASS BACK 2 BYTE MARKET CODE IN NUMERIC FORM REGARDLESS OF VALIDITY            
*        + THE MARKET INPUT TO CALLER                                           
***********************************************************************         
VALAMKT  LR    RB,RF                                                            
         USING VALAMKT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         LA    RE,5                          MAX MARKET SIZE 65535              
         CR    R3,RE                                                            
         JH    EXITN                                                            
                                                                                
         MVC   DUB(5),=C'00000'        VALID NUMERIC                            
         LR    RE,R3                         LENGTH                             
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(R2)                                                     
         LR    RE,R3                         LENGTH                             
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),=C'00000'              DONT COVERT TO NUMERIC             
         JNE   VALAM20                       IF NOT VALID NUMERIC               
                                                                                
         XC    DUB,DUB                                                          
         LR    RE,R3                         LENGTH                             
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   RE,DUB                                                           
         L     RF,=F'65535'                                                     
         CR    RE,RF                                                            
         BH    EXITN                                                            
         STCM  RE,3,0(R4)                                                       
                                                                                
VALAM20  LR    RE,R3                         LENGTH                             
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(R2)                 JUST MOVE IN CODE                  
         OC    2(3,R4),SPACES                                                   
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALRMKT  LR    RB,RF                                                            
         USING VALRMKT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALRMKT-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALKMKT  LR    RB,RF                                                            
         USING VALKMKT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALKMKT-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALUMKT  LR    RB,RF                                                            
         USING VALUMKT,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALUMKT-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALPROG  LR    RB,RF                                                            
         USING VALPROG,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALPROG-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALINV   LR    RB,RF                                                            
         USING VALINV,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALINV-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALAGY   LR    RB,RF                                                            
         USING VALAGY,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALAGY-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALCLI   LR    RB,RF                                                            
         USING VALCLI,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALCLI-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALBEST  LR    RB,RF                                                            
         USING VALBEST,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALBEST-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALTAPE  LR    RB,RF                                                            
         USING VALTAPE,RB                                                       
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALTAPE-DELNK01)        PASS SP00 DISPLACEMENT             
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
VALPTT   LR    RB,RF                                                            
         USING VALPTT,RB                                                        
         LM    R2,R4,0(R1)                                                      
         L     RF,ASP00                                                         
         L     R0,=A(VALPTT-DELNK01)        PASS SP00 DISPLACEMENT              
         ST    R0,12(R1)                                                        
         BASR  RE,RF                                                            
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE DAY TIME STRING                                                      
* PASS BACK   25 BYTE DAY/TIME STRING   +                                       
*             1 BYTE DAYCODE...IF ZERO ITS INVALID                              
*             4 BYTES TIME CODE                                                 
***********************************************************************         
VALDYTIM LR    RB,RF                                                            
         USING VALDYTIM,RB                                                      
         LM    R2,R4,0(R1)         R2=A(INPUT),R3=L'INPUT,R4=A(OUTPUT)          
                                                                                
         XC    DUMDYTMH,DUMDYTMH                                                
         XC    DUMDYTM,DUMDYTM                                                  
         STC   R3,DUMDYTMH+5                                                    
         MVI   DUMDYTMH,L'DUMDYTMH+L'DUMDYTM                                    
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMDYTM(0),0(R2)                                                 
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMDYTMH,(R0),C',=/ '                              
         L     R7,AIO1                                                          
         USING SCANBLKD,R7                                                      
         LR    RE,R3                  PASS BACK INPUT STRING                    
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
                                                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'                                       
         MVC   VDAYPAK,0(R1)                                                    
VALDYT10 CLC   =C'VAR',SC1STFLD                                                 
         BNE   *+12                                                             
         MVI   DUB,X'90'                                                        
         B     VDAYTMXY                                                         
         CLC   =C'ALL',SC1STFLD                                                 
         BNE   *+14                                                             
         MVC   25(5,R4),=X'FFFFFFFFFF'                                          
         J     EXITY                                                            
                                                                                
VALDYT18 DS    0H                  VALIDATE FOR "AVN" OR "AVGN"                 
         ZIC   RF,SC1STLEN                                                      
         SHI   RF,2                                                             
         EXCLC RF,SC1STFLD,=C'AVG'       LOOK FOR "AV" OR "AVG"                 
         BNE   VDYTAVGX              NOPE                                       
         LA    RE,SC1STFLD                                                      
         LA    RE,1(RE,RF)                                                      
         CLI   0(RE),C'2'           SHOULD BE BETWEEN 2                         
         BL    VDYTAVGX                                                         
         CLI   0(RE),C'6'            AND 6 DAYS                                 
         BH    VDYTAVGX                                                         
         MVC   DUB(1),0(RE)                                                     
         NI    DUB,X'9F'            X'90' BITS ==> VAR OR AVG-N                 
         B     VDAYTMXY                                                         
VDYTAVGX EQU   *                                                                
                                                                                
         ZIC   R0,SC1STLEN                                                      
         GOTOR VDAYPAK,DMCB,((R0),SC1STFLD),DUB,=X'17'                          
VDAYTMXY DS    0H                                                               
VDAYTMXN DS    0H                                                               
         MVC   25(1,R4),DUB                                                     
         MVC   OPTUPDT(1),DUB                                                   
         GOTOR VCALLOV,DMCB,0,X'D9000A0E'                                       
         MVC   VTIMVAL,0(R1)                                                    
                                                                                
         AHI   R7,L'SCLINE                                                      
         ZIC   R0,SC1STLEN                                                      
         GOTOR VTIMVAL,DMCB,((R0),SC1STFLD),DUB                                 
         CLI   0(R1),X'FF'                                                      
         JE    EXITY                                                            
         MVC   26(L'DBSELTIM,R4),DUB                                            
         MVC   OPTUPDT+1(4),DUB                                                 
         J     EXITY                                                            
         SPACE 1                                                                
         SPACE 1                                                                
VDAYPAK  DS    A                                                                
VTIMVAL  DS    A                                                                
DUMDYTMH DS   CL8                                                               
DUMDYTM  DS   CL30                                                              
*DUMDYTMH DS    8C                                                              
*DUMDYTM  DS    30C                                                             
         DROP  RB                                                               
         DROP  R7                                                               
                                                                                
         LTORG                                                                  
***********************************************************************         
SETUSRID LR    RB,RF                                                            
         USING SETUSRID,RB                                                      
* FIRST CHECK TO SEE IF UID LINK ALREADY ESTABLISHED                            
         L     RF,DBEXTEND                                                      
         CLC   =C'UID',0(RF)                                                    
         JE    SETUSRX                                                          
                                                                                
         LHI   RF,DBXUID-WORKD                                                  
         LA    RF,WORKD(RF)                                                     
         MVC   4(4,RF),DBEXTEND                                                 
         ST    RF,DBEXTEND                                                      
         USING DBXUIID,RF                                                       
         MVC   DBXUIID,=C'UID  '                                                
         MVC   DBXUUID,USERID                                                   
         DROP  RF                                                               
SETUSRX  J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ENTRY  DMCB+0(1)= TSARKEY LENGTH                                              
*        DMCB+1(2)= TSREC   LENGTH                                              
*        ATSIOREC= TSARREC                                                      
***********************************************************************         
INITTSAR LR    RB,RF                                                            
         USING INITTSAR,RB                                                      
         L     R4,=A(TSARBLCK-WORKD)                                            
         LA    R4,WORKD(R4)                                                     
         USING TSARD,R4                                                         
         XC    0(L'TSARBLCK,R4),0(R4)                                           
         MVC   TSKEYL,DMCB+0                                                    
         MVC   TSRECL,DMCB+1                                                    
         MVI   TSACTN,TSAINI       TSAR INITIALIZATION ACTION                   
         MVC   TSAREC,ATSIOREC                                                  
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,4            START WITH PAGE 1 OF TEMPSTR                 
         MVI   TSPAGN,6            USE THREE PAGES OF TEMPSTR                   
         OI    TSINDS,TSIXTTWA                                                  
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSINDS,TSIINIOK                                                  
         BO    *+6                                                              
         DC    H'0'                                                             
                                                                                
         J     EXITY                                                            
         DROP  RB,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ENTRY  ATSIOREC HAS KEY TO LOOK UP                                            
* EXIT   DMCB+0=Y/N   Y=FOUND -   N=NOT FOUND                                   
*                                                                               
***********************************************************************         
READTSAR LR    RB,RF                                                            
         USING READTSAR,RB                                                      
         L     R4,=A(TSARBLCK-WORKD)                                            
         LA    R4,WORKD(R4)                                                     
         USING TSARD,R4                                                         
         MVI   TSACTN,TSARDH       READ HIGH                                    
         MVC   TSAREC,ATSIOREC                                                  
         XC    TSRNUM,TSRNUM                                                    
         GOTO1 VTSAR,TSARD                                                      
         TM    TSERRS,TSERNF                                                    
         BO    READTN                                                           
         TM    TSERRS,TSEEOF                                                    
         BO    READTN                                                           
READTY   MVI   DMCB,C'Y'                                                        
         B     *+8                                                              
READTN   MVI   DMCB,C'N'                                                        
         J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ENTRY DMCB+0(2) =RECORD NUMBER                                                
* EXIT  DMCB+4(1),= TSERRS                                                      
*                                                                               
***********************************************************************         
GETTSAR  LR    RB,RF                                                            
         USING GETTSAR,RB                                                       
         L     R4,=A(TSARBLCK-WORKD)                                            
         LA    R4,WORKD(R4)                                                     
         USING TSARD,R4                                                         
         MVI   TSACTN,TSAGET                                                    
         MVC   TSAREC,ATSIOREC                                                  
         MVC   TSRNUM,DMCB                                                      
         GOTO1 VTSAR,TSARD                                                      
         MVC   DMCB+4(1),TSERRS                                                 
         J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
***********************************************************************         
* ENTRY  ATSIOREC HAS KEY TO LOOK UP                                            
*                                                                               
***********************************************************************         
WRITTSAR LR    RB,RF                                                            
         USING WRITTSAR,RB                                                      
         L     R4,=A(TSARBLCK-WORKD)                                            
         LA    R4,WORKD(R4)                                                     
         USING TSARD,R4                                                         
         MVI   TSACTN,TSAADD       WRITE RECORD TO TSAR                         
         MVC   TSAREC,ATSIOREC                                                  
         GOTO1 VTSAR,TSARD                                                      
         J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
***********************************************************************         
*  ROUTINE TO READ PROFILE RECORDS                                              
* ENTRY R2 POINTS TO 4 BYTE PROFILE ID                                          
***********************************************************************         
GETPROFL LR    RB,RF                                                            
         USING GETPROFL,RB                                                      
         L     R2,0(R1)            R2=INPUT PROF ID                             
         LA    RE,PROFTAB          FIND ENTRY FOR PROFILE                       
         USING PROFTAB,RE                                                       
GPRF010  CLI   0(RE),EOT                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PRFTBID,0(R2)                                                    
         JE    *+12                                                             
         LA    RE,PRFTBQ(RE)                                                    
         J     GPRF010                                                          
                                                                                
         ZICM  RF,PRFTBASV,(3)                                                  
         LA    RF,WORKD(RF)         RF-->AREA TO HOLD PROFILE                   
         ST    RF,APROFILE                                                      
         ZIC   R1,PRFTBLSV                                                      
         STC   R1,LPROFILE          R1 = L(AREA TO HOLD PROFILE)                
                                                                                
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         XC    0(0,RF),0(RF)        CLEAR AREA USED TO HOLD PROFILE             
                                                                                
         ZICM  RF,PRFTBRTN,(3)                                                  
         LA    RF,GETPROFL(RF)                                                  
         BR    RF                   GO TO APPROPRIATE GET-PROFILE RTN           
         DC    H'0'                                                             
         DROP  RE                                                               
                                                                                
GPRFRRMP00 DS  0H                   REP RMP PROFILE                             
         XC    WORK,WORK            BUILD KEY OF REP RECD IN WORK               
         LA    RE,WORK                                                          
         USING RREPREC,RE                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGYALPH                                                 
         DROP  RE                                                               
                                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+08,X'10'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',WORK+28,AIO1,MYDMWORK           
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  LOOK FOR PROFILE IN RECORD                   
         L     R3,AIO1              R3-->REP RECORD                             
         MVI   MYELCODE,X'04'                                                   
         LA    R0,RREPELEM-RREPREC                                              
         STH   R0,MYDATDSP                                                      
         BAS   RE,GETEL                                                         
         BNE   GPRFRRMPX                                                        
         ZIC   R0,(RREPPGM#-RREPPGMP)(R3)  R0 = # OF PROGRAM PROFILES           
         LA    RE,(RREPPGM1-RREPPGMP)(R3)  R6-->PROGRAM PROFILES LIST           
                                                                                
GPRFRRMP40 DS  0H                                                               
         CLC   =C'SELW',0(R2)       WHAT PROFILE ARE WE LOOKING                 
         BE    GPRFRRMP42           FOR?                                        
         CLC   =C'RRMP',0(R2)                                                   
         BE    GPRFRRMP44                                                       
         DC    H'0'                                                             
GPRFRRMP42 DS  0H                                                               
         CLI   0(RE),RREPQSEL       LOOK FOR SEL PROGRAM PROFILE                
         BE    GPRFRRMP46                                                       
         LA    RE,RREPPGML(RE)                                                  
         BCT   R0,GPRFRRMP40                                                    
         B     GPRFRRMPX                                                        
                                                                                
GPRFRRMP44 DS  0H                                                               
         CLI   0(RE),RREPQRMP       LOOK FOR RMP PROGRAM PROFILE                
         BE    GPRFRRMP46                                                       
         LA    RE,RREPPGML(RE)                                                  
         BCT   R0,GPRFRRMP40                                                    
         B     GPRFRRMPX                                                        
                                                                                
GPRFRRMP46 DS  0H                                                               
         L     RF,APROFILE                                                      
         ZIC   R1,LPROFILE                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),2(RE)       MOVE PROFILE INTO STORAGE AREA              
                                                                                
GPRFRRMPX EQU  *                                                                
         B     GETPRFLX                                                         
                                                                                
GETPRFLX DS    0H                                                               
         J     EXITY                             TABLE OF PROFILE INFO          
         GETEL R3,MYDATDSP,MYELCODE                                             
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
MYELCODE DS    C                                                                
MYDMWORK DS    12D                                                              
MYDATDSP DS    H                                                                
EOT      EQU   0                                                                
PROFTAB  DS    0X                                PROFILE IF                     
PRFTBID  DS    CL4                               ROUTINE TO GET PROFILE         
PRFTBRTN DS    AL2                               A(FIELD) TO STORE PROF         
PRFTBASV DS    AL2                               L(FIELD) TO STORE PROF         
PRFTBLSV DS    XL1                               L(PROFILE INFO ENTRY)          
PRFTBQ   EQU   *-PROFTAB                                                        
         ORG   PROFTAB                                                          
         DC    C'SELW',AL2(GPRFRRMP00-GETPROFL),AL2(PROFSELW-WORKD)             
         DC    AL1(L'PROFSELW)                                                  
         DC    C'RRMP',AL2(GPRFRRMP00-GETPROFL),AL2(PROFRRMP-WORKD)             
         DC    AL1(L'PROFRRMP)                                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
*SP00 -  REGULAR DEMT PROCESSING                                                
***********************************************************************         
SP00     LR    RB,RF                                                            
         USING SP00,RB                                                          
         ST    RE,SAVERE                                                        
         LM    R2,R5,0(R1)                                                      
                                                                                
         L     R6,=A(INPTBL)                                                    
         L     RF,ROU1RELO         SAVE MY RELOCATION FACTOR                    
         AR    R6,RF                                                            
         J     SP2                                                              
         EJECT                                                                  
* VALIDATE ALL SCREEN FIELDS                                                    
*                                                                               
         USING INPTBLD,R6          R2=A(INPUT FIELD TABLE)                      
SP2      CLI   0(R6),X'FF'         TEST E-O-T                                   
         JE    EXITN                                                            
         LH    R1,INPIFLD          FIND THE CORRECT TABLE ENTRY                 
         CR    R5,R1                                                            
         BE    SP3                                                              
         LA    R6,L'INPTBL(R6)                                                  
         B     SP2                                                              
SP3      DS    0H                                                               
SP4      CHI   R3,0            TEST IF FIELD INPUT                              
         BNE   *+16                                                             
         TM    INPINDS,X'01'       NO - TEST IF REQUIRED                        
         JNZ   EXITN                                                            
         B     SP14                                                             
         ZIC   R0,INPIMIN          TEST INPUT LENGTH                            
         CR    R3,R0               TEST INPUT LENGTH                            
         JL    EXITN                                                            
         ZIC   R0,INPIMAX          TEST INPUT LENGTH                            
         CR    R3,R0                                                            
         JH    EXITN                                                            
         TM    INPINDS,X'F0'                                                    
         BZ    SP11                                                             
         TM    INPINDS,X'80'       TEST FIELD NUMERIC                           
         BZ    SP6                                                              
         MVI   DUB,C'0'                                                         
         LR    RE,R3                                                            
         AHI   RE,-2                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUB2(0),DUB+1                                                    
         MVC   DUB,DUB2                                                         
                                                                                
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(R2)                                                     
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),DUB2                                                      
         JNE   EXITN                                                            
                                                                                
         OC    0(4,R2),0(R2)                                                    
         JZ    EXITN                                                            
         CLC   0(4,R2),=F'99999'                                                
         JH    EXITN                                                            
         XC    DUB,DUB                                                          
SP6      SR    RF,RF                                                            
         ICM   RF,7,INPADDR        A(TABLE)                                     
         A     RF,ROU1RELO                                                      
         TM    INPINDS,X'40'       TEST IF TABLE SEARCH                         
         BZ    SP11                                                             
         SR    R1,R1                                                            
         ICM   R1,1,INPIMAX                                                     
         SR    RE,RE                                                            
         ICM   RE,1,INPOLEN                                                     
         LA    R0,0(R1,RE)         R0=TOTAL ENTRY LENGTH                        
         BCTR  R1,0                R1=PART 1 TABLE LENGTH                       
         LR    RE,R3                                                            
         BCTR  RE,0                RE=L'INPUT-1                                 
SP8      CLI   0(RF),X'FF'                                                      
         JE    EXITN                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),0(R2)                                                    
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     SP8                                                              
         LA    RE,1(RF,R1)                                                      
         MVC   DUB,0(RE)                                                        
         B     SP12                                                             
                                                                                
SP10     DS    0H                                                               
SP11     DS    0H                  STRAIGHT FROM INPUT FIELD                    
         OC    DUB,SPACES                                                       
         LR    RE,R3                                                            
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R2)                                                     
                                                                                
SP12     DS    0H                                                               
         ZIC   RE,INPOLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DUB                                                      
         DROP  R6                                                               
                                                                                
SP14     L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
INPTBL   DS    0XL11                                                            
         DC    AL2(VALFUNCT-DELNK01,DBFUNCT-DBLOCK)                             
         DC    AL1(4,8,L'DBFUNCT),X'41',AL3(FUNCTAB)                            
         DC    AL2(VALFILE-DELNK01,DBFILE-DBLOCK)                               
         DC    AL1(1,3,L'DBFILE),X'40',AL3(FILETAB)                             
         DC    AL2(VALMED-DELNK01,DBSELMED-DBLOCK)                              
         DC    AL1(1,5,L'DBSELMED),X'40',AL3(MEDTAB)                            
         DC    AL2(VALSRC-DELNK01,DBSELSRC-DBLOCK)                              
         DC    AL1(1,3,L'DBSELSRC),X'40',AL3(SRCTAB)                            
         DC    AL2(VALPTT-DELNK01,DBSELPTT-DBLOCK)                              
         DC    AL1(1,1,L'DBSELPTT),X'00',AL3(PTTTAB)                            
         DC    AL2(VALSTA-DELNK01,DBSELSTA-DBLOCK)                              
         DC    AL1(3,5,L'DBSELSTA),X'00',AL3(0)                                 
         DC    AL2(VALSTATP-DELNK01,DBSTYPE-DBLOCK)                             
         DC    AL1(1,1,L'DBSTYPE),X'00',AL3(0)                                  
         DC    AL2(VALAMKT-DELNK01,DBSELALF-DBLOCK)                             
         DC    AL1(1,3,L'DBSELALF),X'00',AL3(0)                                 
         DC    AL2(VALRMKT-DELNK01,DBSELRMK-DBLOCK)                             
         DC    AL1(1,4,L'DBSELRMK),X'80',AL3(0)                                 
         DC    AL2(VALKMKT-DELNK01,DBSELMK-DBLOCK)                              
         DC    AL1(1,5,L'DBSELMK),X'80',AL3(0)                                  
         DC    AL2(VALUMKT-DELNK01,DBSELUMK-DBLOCK)                             
         DC    AL1(1,4,L'DBSELUMK),X'80',AL3(0)                                 
         DC    AL2(VALPROG-DELNK01,DBSELPRG-DBLOCK)                             
         DC    AL1(1,5,L'DBSELPRG),X'80',AL3(0)                                 
         DC    AL2(VALINV-DELNK01,DBSELINV-DBLOCK)                              
         DC    AL1(1,4,L'DBSELINV),X'00',AL3(0)                                 
         DC    AL2(VALAGY-DELNK01,DBSELAGY-DBLOCK)                              
         DC    AL1(2,2,L'DBSELAGY),X'00',AL3(0)                                 
         DC    AL2(VALCLI-DELNK01,DBSELCLI-DBLOCK)                              
         DC    AL1(2,3,L'DBSELCLI),X'00',AL3(0)                                 
         DC    AL2(VALBEST-DELNK01,DBBEST-DBLOCK)                               
         DC    AL1(1,1,L'DBBEST),X'00',AL3(0)                                   
         DC    AL2(VALTPTT-DELNK01,DBTPTT-DBLOCK)                               
         DC    AL1(1,1,L'DBTPTT),X'00',AL3(0)                                   
         DC    AL2(VALTAPE-DELNK01,DBTAPEP-DBLOCK)                              
         DC    AL1(1,1,L'DBTAPEP),X'00',AL3(0)                                  
                                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         DROP  R8                                                               
**********************************************************                      
*                                                                               
FUNCTAB  DS    0X                                                               
         DC    C'VALST   ',AL1(DBVLST)                                          
         DC    C'VALSTBK ',AL1(DBVLSTBK)                                        
         DC    C'GETSM   ',AL1(DBGETSM)                                         
         DC    C'GETMS   ',AL1(DBGETMS)                                         
         DC    C'GETMB   ',AL1(DBGETMB)                                         
         DC    C'GETMK   ',AL1(DBGETMK)                                         
         DC    C'GETMKB  ',AL1(DBGETMKB)   MARKETS FOR A BOOK                   
         DC    C'GETMKN  ',AL1(DBGETMKN)   MARKETS FOR A SERVICE                
         DC    C'GETUNV  ',AL1(DBGETUNV)                                        
         DC    C'GETNTI  ',AL1(DBGETNTI)                                        
         DC    C'GETNET  ',AL1(DBGETDEM)                                        
         DC    C'GETTOT  ',AL1(DBGETTOT)                                        
         DC    C'GETDEM  ',AL1(DBGETDEM)                                        
         DC    C'GETPUR  ',AL1(DBGETPUR)                                        
         DC    C'GETTLB  ',AL1(DBGETTLB)                                        
         DC    C'VALNBK  ',AL1(DBVLNBK)                                         
         DC    C'GETDIR  ',AL1(0)                                               
         DC    C'GETFIL  ',AL1(0)                                               
         DC    C'CLRTAB  ',AL1(0)                                               
         DC    C'TSTACS  ',AL1(DBTSTACS)                                        
         DC    C'CNVA2N  ',AL1(DBCNVA2N)                                        
         DC    C'CNVN2A  ',AL1(DBCNVN2A)                                        
         DC    C'GETAMB  ',AL1(DBGETAMB)                                        
         DC    C'GETASB  ',AL1(DBGETASB)                                        
         DC    C'GETAB   ',AL1(DBGETAB)                                         
         DC    C'GETASM  ',AL1(DBGETASM)                                        
         DC    C'ACSALL  ',AL1(DBACSALL)                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
*                                                                               
* FILE TABLE                                                                    
*                                                                               
FILETAB  DS    0X                                                               
         DC    C'TT ',C'TP '                                                    
         DC    C'PAV',C'PAV'                                                    
         DC    C'INV',C'INV'                                                    
         DC    C'T4 ',C'T4 '                                                    
         DC    C'WTP',C'WTP'                                                    
         DC    C'OTP',C'OTP'                                                    
         DC    C'OPA',C'OPA'                                                    
         DC    C'TF ',C'TF '                                                    
         DC    C'RTP',C'RTP'                                                    
         DC    C'RDP',C'RDP'                                                    
         DC    C'TDP',C'TDP'                                                    
         DC    C'TRT',C'RTP'         TRITON TP                                  
         DC    C'TRD',C'RDP'         TRITON DPT                                 
         DC    X'FF'                                                            
         SPACE 1                                                                
* MEDIA TABLE                                                                   
                                                                                
MEDTAB   DS    0X                                                               
         DC    C'USTV ',C'T'                                                    
         DC    C'CANTV',C'C'                                                    
         DC    C'NETTV',C'N'                                                    
         DC    C'RADIO',C'R'                                                    
         DC    C'DPT  ',C'D'                                                    
         DC    C'WTP  ',C'W'                                                    
         DC    C'CABLE',C'C'                                                    
         DC    C'HISPW',C'W'                                                    
         DC    C'UPGRD',C'U'                                                    
         DC    X'FF'                                                            
         SPACE 1                                                                
* SOURCE TABLE                                                                  
                                                                                
SRCTAB   DS    0X                                                               
         DC    C'ARB',C'A'                                                      
         DC    C'NSI',C'N'                                                      
         DC    C'SRC',C'S'                                                      
         DC    C'BBM',C'M'                                                      
         DC    C'MFX',C'M'                                                      
         DC    C'NHT',C'H'         REP REQST FOR NHTI DATA                      
         DC    C'NTI',C'K'               "       NTI                            
         DC    C'NAD',C'D'               "       NAD                            
         DC    C'CBL',C'C'               "       CBL                            
         DC    C'RAR',C'R'         NET RADIO RADAR                              
         DC    X'FF'                                                            
                                                                                
*                                                                               
* SELPTT TABLE - CABLE PROGRAMS FILTER OPTIONS                                  
*                                                                               
PTTTAB   DS    0X                                                               
         DC    C'P',C'P'           PROGRAM ONLY                                 
         DC    C'T',C'T'           TRACK ONLY                                   
         DC    C'E',C'E'           EPISODE (TELECAST) ONLY                      
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
**********************************************************                      
         SPACE 1                                                                
*                                                                               
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    MVI   DUB1,0              SET CC LOW                                   
         J     EXITCC                                                           
EXITH    MVI   DUB1,2              SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    MVI   DUB1,1              SET CC EQUAL                                 
EXITCC   CLI   DUB1,1                                                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    V(WRKIO)                                                         
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(PROJTAB)                                                       
         DC    V(NSIWEEK)                                                       
         DC    9A(0)                                                            
         DC    CL132' '                                                         
         DC    X'D9000A'                                                        
         DC    C'BL00'                                                          
         DC    C'BL01'                                                          
         DC    C'BL02'                                                          
         DC    C'BL03'                                                          
         DC    C'BL04'                                                          
         DC    C'BL05'                                                          
*                                                                               
LVALUESL EQU   *-LVALUES                                                        
                                                                                
FACTAB   DS    0XL(FACTABL)        ** EXTRACTED FACILITIES ADDRESSES **         
         DC    AL1(FACTCOMQ),AL2(VCALLOV-WORKD,CCALLOV-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDDLINK-WORKD,CDDLINK-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VDATCON-WORKD,CDATCON-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATVAL-WORKD,CDATVAL-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDEMOVAL-WORKD,CDEMOVAL-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VSCANNER-WORKD,CSCANNER-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDEMAND-WORKD,CDEMAND-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATAMGR-WORKD,CDATAMGR-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDEMOUT-WORKD,CDEMOUT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VHELLO-WORKD,CHELLO-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VADDAY-WORKD,CADDAY-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VDEFINE-WORKD,CDEFINE-COMFACSD)                
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
PROJTAB  DS    0XL6                                                             
         DC    C'UPT',AL1(#VALUPGD),AL1(4),AL1(0)                               
         DC    C'UPP',AL1(#VALUPGD),AL1(4),AL1(0)                               
         DC    C'UPX',AL1(#VALUPGD),AL1(4),AL1(0)                               
         DC    C'BK ',AL1(#VALBOOK),AL1(3),AL1(0)                               
         DC    C'DT ',AL1(#VALDYTM),AL1(3),AL1(0)                               
         DC    C'RTG',AL1(0),AL2(OPT2YRR-WORKD)                                 
         DC    C'PUT',AL1(0),AL2(OPT2YRP-WORKD)                                 
         DC    AL1(0)                                                           
PROJTABD DSECT                                                                  
PROJKEYW DS    CL3                 KEYWORD                                      
PROJADDR DS    AL1                 ROUTINE ADDRESS                              
PROJSDIS DS    AL1                 DISPLACEMENT TO INPUT STRING,OR C'Y'         
         DS    XL1                                                              
                                                                                
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTFLST DS    XL1                 FACILITIES LIST DISPLACEMENT                 
FACTCOMQ EQU   ACOMFACS-FACLISTS   COMFACS                                      
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
                                                                                
                                                                                
                                                                                
*                                                                               
INPTBLD  DSECT                                                                  
INPIFLD  DS    AL2                 DISP. TO INPUT FIELD                         
INPOFLD  DS    AL2                 DISP. TO OUTPUT FIELD                        
INPIMIN  DS    AL1                 MIN L'INPUT                                  
INPIMAX  DS    AL1                 MAX L'INPUT                                  
INPOLEN  DS    AL1                 L'OUTPUT FIELD                               
INPINDS  DS    XL1                 X'80'=NUMERIC FIELD                          
*                                  X'40'=INPADDR IS A(TABLE)                    
*                                  X'20'=INPADDR IS A(ROUTINE)                  
*                                  X'10'=VALIDATE DEMOS                         
*                                  X'01'=REQUIRED FIELD                         
INPADDR  DS    AL3                 A(TABLE) OR A (ROUTINE)                      
         EJECT                                                                  
                                                                                
DELNK01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QFALINK)                                                     
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QUPVAL)                                                      
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DELNKWRK                                                       
       ++INCLUDE DEDEMEQUS                                                      
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPERVALD                                                      
                                                                                
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DELNK01   01/30/13'                                      
         END                                                                    
