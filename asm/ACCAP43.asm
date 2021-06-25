*          DATA SET ACCAP43    AT LEVEL 018 AS OF 07/26/17                      
*PHASE T61D43C                                                                  
***********************************************************************         
*INCLUDE NUMVAL                                                                 
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP43 -- TIME/REPORT                               *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS ACCAP30,              *         
*                WHICH CALLS THIS.                                    *         
*                                                                     *         
*  CALLS:        ROUTINES ARE IN ACCAP31/32/33                        *         
*                                                                     *         
*  OUTPUTS:      REPORT ON PRINT Q                                    *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- 2ND BASE                                       *         
*                R5 -- PRINT LINE                                     *         
*                R6 -- WORK                                           *         
*                R7 -- SPOOLD                                         *         
*                R8 -- SYSD - BASE SAVED STORAGE                      *         
*                R9 -- TIME GLOBAL WORKING STORAGE                    *         
*                RA -- GEND                                           *         
*                RB -- FIRST BASE                                     *         
*                RC -- ATWA                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61D43 - TIMESHEET REPORT'                                      
T61D43   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D43**,R4,RR=R3                                              
         USING TIMEGWSD,R9         R9=A(TIME GLOBAL WORKING STORAGE)            
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(ADDRS-GENCON STUFF)                     
         L     RA,ATWA                                                          
         USING T61DFFD,RA          RA=A(TWA)                                    
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(BASE SAVED STORAGE)                     
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7           R7=A(SPOOL)                                  
         ST    R3,RELO                                                          
         L     R1,=A(VALTAB)       ADDRESS FOR VALIDATION TAB                   
         A     R1,RELO                                                          
         ST    R1,AVALTAB                                                       
         L     R1,=A(FLTTAB)       ADDRESS FOR FILTER TAB                       
         A     R1,RELO                                                          
         ST    R1,AFLTTAB                                                       
*                                                                               
         LA    R2,CONRECH                                                       
         TM    BCCPYST7,CPYSTMSY   TIME MANAGEMENT SYSTEM IN USE                
         BNO   ENATMS              NOT AUTHORIZED FOR TMS                       
*                                                                               
MAIN10   MVC   AIO,AIO1            DEFAULT AIO                                  
         MVI   TSARSTAT,0                                                       
         LA    R2,PFTABLE                                                       
         LA    R3,REPPFKYH                                                      
         GOTO1 INITIAL,BCDMCB,(X'40',(R2)),(R3) INITIALIZE THE PFKEYS           
*                                                                               
         LA    R2,CONOPTH                                                       
         XC    BCFLTS,BCFLTS       CLEAR SAVED AREA FOR FILTERS                 
         MVC   HD6LINE,BCSPACES    CLEAR OPTIONS                                
         MVC   HD6TITLE,=C'OPTIONS'                                             
         MVC   HD6FIELD,8(R2)                                                   
*                                                                               
         GOTO1 AVALOPTS,(R2)       EXTRACT OPTIONS - BCOPTS SET                 
         BNE   ACCERRX                                                          
         MVC   SVOPT1,BCOPT1                                                    
         OI    4(R2),X'20'         VALIDATED OPTIONS                            
*                                                                               
MAIN20   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ACTION = VALIDATE KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
VK       DS    0H                                                               
         LA    RE,SVVKBLCK                                                      
         LA    RF,SVVKBLKQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RF,TOTALS                                                        
         LA    R0,TOTALQ                                                        
         ZAP   0(L'TOTALS,RF),=P'0'                                             
         LA    RF,L'TOTALS(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVC   HD1LINE,BCSPACES                                                 
         MVC   HD2LINE,BCSPACES                                                 
         MVC   HD3LINE,BCSPACES                                                 
         MVC   HD4LINE,BCSPACES                                                 
         MVC   HD5LINE,BCSPACES                                                 
*                                                                               
*              VALIDATE PERSON CODE                                             
*                                                                               
VKPRSN   DS    0H                                                               
         LA    R2,REPPERH          PERIOD IS FIRST FIELD ON SCREEN              
         CLI   5(R2),0             BUT CAN'T VALIDATE WITHOUT PERSON            
         BH    VKPRSN10                                                         
         OC    SVPERDTE,SVPERDTE   DO WE HAVE A PERIOD SAVED AROUND?            
         BNZ   VKPRSN10                                                         
         MVI   GERROR1,2           PLEASE INPUT REQUIRED FIELDS                 
         B     GENERRX                                                          
*                                                                               
VKPRSN10 LA    R2,REPCODEH                                                      
         MVI   BCIFMIN,1           MINIMUN LENGTH - PERSON REQUIRED             
         MVC   BCIFMAX,BC1RLEV4    MAXIMUM LENGTH                               
         GOTO1 AFVAL,REPCODEH                                                   
         BH    ACCERRX                                                          
         GOTO1 AVALPRSN,REPCODEH   VALIDATE PERSON                              
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIVPER)    INVALID PERSON CODE                     
         B     ACCERRX                                                          
*                                                                               
         MVC   SV1RPRSN,BCIFLD     SAVE PERSON CODE                             
         OI    REPCODEH+6,X'80'                                                 
         MVC   HD2TITLE,=C'PERSON'                                              
         MVC   HD2FIELD,BCIFLD                                                  
*                                                                               
*&&US                                                                           
         MVI   BCBYTE1,NAMEFLDQ    CHECK ACCESS TO VIEW NAME                    
         BAS   RE,FLDSEC           SECURITY TO VIEW NAME                        
         BL    VKPRSNX                                                          
*&&                                                                             
         MVC   REPNAME,BCWORK      DISPLAY PERSON NAME                          
         OI    REPNAMEH+6,X'80'                                                 
         MVC   HD2DESC,BCWORK                                                   
VKPRSNX  LA    R2,REPCODEH                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VKPERD   DS    0H                                                               
         LA    R2,REPPERH          ANYTHING IN PERIOD FIELD                     
         CLI   5(R2),0                                                          
         BNE   VKPERD10                                                         
         OC    SVPERDTE,SVPERDTE   DO WE HAVE A PERIOD SAVED AROUND?            
         BNZ   *+12                                                             
         MVI   GERROR1,2           PLEASE INPUT REQUIRED FIELDS                 
         B     GENERRX                                                          
         GOTO1 DATCON,BCDMCB,(2,SVPERDTE),(5,8(R2))                             
         MVI   5(R2),8             SET APPROPRIATE LENGTH                       
VKPERD10 XC    BCFLAG4,BCFLAG4                                                  
*                                                                               
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK+L'PVALOUTB                                              
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,BCDMCB,(R2),(2,BLOCK+L'PVALOUTB),C',=-='                 
         CLI   BCDMCB+4,1                                                       
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     ACCERRX                                                          
         CLI   5(R2),3             L'INPUT>3 MEANS DATE  EX/'051594'            
         BH    *+12                                                             
         TM    SC1STVAL,SCNUMQ     DID THEY ENTER A PERIOD NUMBER               
         BO    VKPERD20                                                         
*                                                                               
*              USER ENTERED A DATE                                              
*                                                                               
         OI    BCFLAG4,BCFL4DTE    FLAG THAT USER ENTERED A DATE                
         USING PERVALD,R6                                                       
         LA    R6,BLOCK                                                         
         GOTO1 PERVAL,BCDMCB,(SC1STLEN,SC1STFLD),(X'20',BLOCK)                  
         CLI   BCDMCB+4,PVRCMISS                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     ACCERRX                                                          
         CLI   BCDMCB+4,PVRCINV1                                                
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ACEINV) INVALID PERIOD                               
         B     ACCERRX                                                          
         MVC   YYMMDD,PVALPSTA                                                  
         BAS   RE,GETLOC           GET OFFICE FOR THIS DATE                     
         BE    *+14                NO LOCATION FOR THIS PERIOD                  
         MVC   GERROR,=AL2(ACEINV)                                              
         B     ACCERRX                                                          
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPYMD,YYMMDD                                                   
         OI    CALSTAT,CALYMDQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
         B     VKPERD30                                                         
*                                                                               
*              USER ENTERED A PERIOD NUMBER                                     
*                                                                               
VKPERD20 OI    BCFLAG4,BCFL4PER    FLAG THAT USER ENTERED A PERIOD#             
         MVC   YYMMDD,BCTODAY3                                                  
         BAS   RE,GETLOC           GET LOCATION FOR TODAY                       
         USING CALD,R1                                                          
         LA    R1,WORK2            FILL IN CALENDAR BLOCK                       
         XC    WORK2(CALDQ),WORK2                                               
         MVC   CALPNUM,SC1STNUM+3                                               
         OI    CALSTAT,CALNUMQ                                                  
         MVC   CALOFF,SV1ROFFC                                                  
*                                                                               
*              CALL GETCAL TO GET PERIOD INFO                                   
*                                                                               
VKPERD30 MVC   AIO,AIO2                                                         
         GOTO1 AGETCAL,WORK2                                                    
         BNE   ACCERRX                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         USING CALD,R1                                                          
         LA    R1,WORK2            FINAL CHECK TO MAKE SURE OFFICE IS           
         MVC   PRDSTDTE,CALRSTRT   PERIOD START DATE                            
         MVC   PRDENDTE,CALREND    CORRECT FOR THAT DATE                        
         MVC   PRDNUM,CALRNUM                                                   
         MVC   PRDMON,CALRMTH                                                   
         MVC   SVYYMMDD,YYMMDD                                                  
         TM    BCFLAG4,BCFL4DTE    USER ENTERED A DATE?                         
         BO    VKPERD40                                                         
         MVC   YYMMDD,PRDENDTE                                                  
         MVC   SVYYMMDD,PRDENDTE                                                
         BAS   RE,GETLOC           DO OFFICE AND DATE MATCH                     
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEENTDT)                                            
         B     ACCERRX             MUST ENTER A DATE                            
*                                                                               
VKPERD40 MVC   YYMMDD,SVYYMMDD                                                  
         GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(2,SVPERDTE)  PERIOD END DTE          
         BAS   RE,GETLOC           REREAD TO GET ACTUAL OFFC/DPT/SUBD           
         DROP  R1,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY MULTIPLE LOCATIONS                                          *         
***********************************************************************         
         SPACE 1                                                                
VKLOC    MVC   REPLOCS,BCSPACES                                                 
         GOTO1 ALOCLIST,BCDMCB,AIO,PRDSTDTE,PRDENDTE,SVYYMMDD                   
         BH    VKLOCX                                                           
         MVC   REPLOCS,BCWORK                                                   
         OI    REPLOCSH+1,X'08'    HIGH INTENSITY                               
         OI    REPLOCSH+6,X'80'                                                 
         TM    BCFLAG4,BCFL4PER                                                 
         BNO   *+14                                                             
         MVC   GERROR,=AL2(ACEENTDT)                                            
         B     ACCERRX             MUST ENTER A DATE                            
         CLI   BCFLAG3,BCFL3OK     WAS DATE ENTERED A LOCATION ENDDATE          
         BE    *+14                                                             
         MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX             INVALID DATE                                 
*                                                                               
VKLOCX   DS    0H                                                               
         OI    REPLOCSH+6,X'80'                                                 
         TM    BCFLAG4,BCFL4DTE                                                 
         BO    *+10                                                             
         MVC   SVYYMMDD,PRDENDTE   IF PERIOD # THEN USE PERIOD ENDDATE          
         EJECT                                                                  
***********************************************************************         
* DISPLAY PERIOD NUMBER - DATE - MONTH                                *         
***********************************************************************         
         SPACE 1                                                                
         MVC   BCWORK,BCSPACES                                                  
         LA    R3,BCWORK                                                        
         MVI   0(R3),C'#'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (B1,PRDNUM),(2,(R3)),FILL=0                                      
         LA    R3,3(R3)                                                         
         GOTO1 DATCON,BCDMCB,(1,PRDENDTE),(11,(R3))                             
         LA    R3,9(R3)                                                         
         MVC   YYMMDD,PRDMON                                                    
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,(R3))                                
         MVC   REPPDES,BCWORK                                                   
         OI    REPPDESH+6,X'80'                                                 
         MVC   HD1TITLE,=C'PERIOD'                                              
         MVC   HD1FIELD,REPPER                                                  
         MVC   HD1DESC,BCWORK                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD 1R ACCOUNT                                                    *         
***********************************************************************         
         SPACE 1                                                                
VKOFFC   DS    0H                                                               
         MVC   SV1RACT,BCSPACES                                                 
         MVC   SV1RCPY,CMPY                                                     
         MVC   SV1RUL,=C'1R'                                                    
         LA    R3,SV1RCODE                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLEV1         LENGTH OF LEVEL A                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1ROFFC                                                 
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
*                                                                               
*                                                                               
         LA    R2,REPOFFCH        DISPLAY OFFICE                                
         EX    R1,*+4                                                           
         MVC   REPOFFC(0),SV1ROFFC                                              
         OC    REPOFFC,BCSPACES                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPOFNMH         DISPLAY OFFICE NAME                          
         MVC   REPOFNM,BCACNAME                                                 
         OI    6(R2),X'80'                                                      
         MVC   HD3TITLE,=C'OFFICE'                                              
         MVC   HD3FIELD(L'SV1ROFFC),SV1ROFFC                                    
         MVC   HD3DESC,BCACNAME                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R DEPARTMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
VKDEPT   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV2         LENGTH OF LEVEL B                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RDPT                                                  
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
*                                                                               
         LA    R2,REPDEPTH         DISPLAY DEPARTMENT                           
         EX    R1,*+4                                                           
         MVC   REPDEPT(0),SV1RDPT                                               
         OC    REPDEPT,BCSPACES                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPDPNMH         DISPLAY DEPARTMENT NAME                      
         MVC   REPDPNM,BCACNAME                                                 
         OI    6(R2),X'80'                                                      
         MVC   HD4TITLE(4),=C'DEPT'                                             
         MVC   HD4FIELD(L'SV1RDPT),SV1RDPT                                      
         MVC   HD4DESC,BCACNAME                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBDEPARTMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
VKSUBD   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV3                                                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RSDPT                                                 
         LA    R3,1(R1,R3)                                                      
         BAS   RE,VAL1R                                                         
*                                                                               
         LA    R2,REPSDPTH         DISPLAY SUBDEPARTMENT                        
         EX    R1,*+4                                                           
         MVC   REPSDPT(0),SV1RSDPT                                              
         OC    REPSDPT,BCSPACES                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPSDNMH         DISPLAY SUBDEPARTMENT NAME                   
         MVC   REPSDNM,BCACNAME                                                 
         OI    6(R2),X'80'                                                      
         MVC   HD5TITLE,=C'SUBDPT'                                              
         MVC   HD5FIELD(L'SV1RSDPT),SV1RSDPT                                    
         MVC   HD5DESC,BCACNAME                                                 
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE ON 1R LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
VKPERS   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,BC1RLEV4         LENGTH OF LEVEL D                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),SV1RPRSN                                                 
         LA    R2,REPCODEH                                                      
         BAS   RE,VAL1R                                                         
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET PERSON'S LOCATION WITH DATE IN YYMMDD                           *         
***********************************************************************         
         SPACE 1                                                                
GETLOC   NTR1                                                                   
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
*                                                                               
         USING LOCELD,R6                                                        
GETLOC10 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    ROUTH                                                            
         CLI   0(R6),LOCELQ        LOCATION ELEMENT X'83'                       
         BNE   GETLOC10                                                         
         CLC   YYMMDD,LOCSTART                                                  
         BL    GETLOC10                                                         
         OC    LOCEND,LOCEND       ANY END DATE                                 
         BZ    *+14                                                             
         CLC   YYMMDD,LOCEND                                                    
         BH    GETLOC10                                                         
*                                                                               
         MVC   SV1ROFFC,LOCOFF                                                  
         MVC   SV1RDPT,LOCDEPT                                                  
         MVC   SV1RSDPT,LOCSUB                                                  
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE 1R ACCOUNT - CALL GETACT WITH EACH LEVEL OF 1R             *         
***********************************************************************         
         SPACE 1                                                                
VAL1R    NTR1                                                                   
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(L'SV1RACT),SV1RACT                                       
         GOTO1 AGETACT,0                                                        
         BNE   ROUTH                                                            
*                                                                               
*&&US                                                                           
         LA    RE,ACTKACT          RE=A(ACCOUNT)                                
         SR    RF,RF                                                            
         IC    RF,BC1RLNQ3         BUMP PAST 1ST 3 LEVELS                       
         AR    RE,RF                                                            
         SR    R1,R1                                                            
         IC    R1,BC1RLEV4         LENGTH OF LEVEL D                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+10                                                          
         CLC   0(0,RE),BCSPACES                                                 
         BNH   ROUTE                                                            
         GOTO1 TSTSEC,0                                                         
*&&                                                                             
*                                                                               
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PR       DS    0H                                                               
*&&US                                                                           
         LA    R2,CONWHENH                                                      
         CLC   REMUSER,=C'***'     HAS USER PROVIDED AN ID?                     
         BH    *+12                YES                                          
         STCM  R2,15,ACURFORC      FORCE THE CURSOR ON PRINT FIELD              
         B     ERRINIT                                                          
*&&                                                                             
         MVC   AIO,AIO1                                                         
         USING PLINED,R5                                                        
         LA    R5,P                R5=A(PRINT LINE)                             
         LA    RF,HEDSPEC                                                       
         ST    RF,SPECS            SET A(HEADLINE SPECS)                        
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK         SET A(HEADLINE HOOK)                         
*                                                                               
         OI    TSARSTAT,TSARSAVE                                                
         MVC   BCACCODE,SV1RACT                                                 
         MVC   BCYYMMDD,PRDENDTE                                                
         GOTO1 ABLDTSAR,BCDMCB,SVOPT1,0,1                                       
         BNE   ROUTE                                                            
*                                                                               
*&&US*&& BAS   RE,CHKSEC           CHECK SECURITY                               
*                                                                               
         USING TSARRECD,R6                                                      
         L     R6,AIO1                                                          
         LA    R2,TSARDH                                                        
PR10     GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R2),1                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    PR60                                                             
         LA    R2,TSANXT                                                        
*                                                                               
         XC    AINPELEM,AINPELEM   CLEAR A(8B ELEMENTS)                         
         XC    ATAXELEM,ATAXELEM                                                
         XC    ANARELEM,ANARELEM                                                
         XC    ACOMELEM,ACOMELEM                                                
*                                                                               
         USING TIMELD,R3                                                        
         LA    R3,TRDATA                                                        
PR20     CLI   0(R3),TIMELQ        X'8B' ELEM                                   
         BNE   PR40                                                             
         CLI   TIMETYP,TIMEINP                                                  
         BNE   *+8                                                              
         ST    R3,AINPELEM         A(INPUT DETAILS ELEMENT)                     
         CLI   TIMETYP,TIMETAX                                                  
         BNE   *+8                                                              
         ST    R3,ATAXELEM         A(TAX DETAIL ELEMENT)                        
         CLI   TIMETYP,TIMENAR                                                  
         BNE   *+8                                                              
         ST    R3,ANARELEM         A(NARRATIVE ELEMENT)                         
         CLI   TIMETYP,TIMEFLD                                                  
         BNE   PR30                                                             
         ST    R3,ACOMELEM         A(COMMENT ELEMENT)                           
         BAS   RE,VALCM            VALIDATE COMMENTED FIELDS                    
         B     PR10                                                             
*                                                                               
PR30     SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PR20                                                             
*                                                                               
PR40     OC    AINPELEM,AINPELEM                                                
         BZ    PR10                                                             
         OC    BCFLTS,BCFLTS                                                    
         BZ    PR50                                                             
         GOTO1 AFILTER,BCDMCB,AINPELEM,(R6)                                     
         BNE   PR10                                                             
PR50     MVC   P,BCSPACES                                                       
         BAS   RE,DISTYPE          DISPLAY TYPE                                 
         BAS   RE,DISHRS           DISPLAY HOURS                                
         BAS   RE,DISCPJT          DISPLAY CLIENT/PRODUCT/JOB                   
         BAS   RE,DISMOA           DISPLAY MOA                                  
         BAS   RE,DISRATE          DISPLAY RATE                                 
         BAS   RE,DISAMT           DISPLAY AMOUNT                               
         BAS   RE,DISINC           DISPLAY INCOME ACCOUNT                       
         BAS   RE,DISCOST          DISPLAY COST ACCOUNT                         
*        BAS   RE,DISNAME          DISPLAY ACCOUNT NAME                         
*&&UK*&& BAS   RE,DISTAX           DISPLAY TAX DATA                             
         BAS   RE,DISNAR           DISPLAY NARRATIVE                            
         GOTO1 SPOOL,DMCB,(R7)                                                  
         B     PR10                                                             
*                                                                               
PR60     DS    0H                                                               
*        ZAP   DUB,NTOTAL                                                       
*        AP    DUB,BTOTAL                                                       
*        AP    DUB,RTOTAL                                                       
*        CP    DUB,=P'0'                                                        
*        BE    ROUTE                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PTOTAL#+1(5),=C'HOURS'                                           
         MVC   PTOTAL$+5(6),=C'AMOUNT'                                          
*&&UK*&& MVC   PTXTOTAL+8(3),=C'TAX'                                            
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PTOTAL#+1(5),=C'-----'                                           
         MVC   PTOTAL$+5(6),=C'------'                                          
*&&UK*&& MVC   PTXTOTAL+8(3),=C'---'                                            
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PTOTAL,=CL12'TOTAL N TIME'                                       
         CURED NTOTAL,(L'PTOTAL#,PTOTAL#),2,MINUS=YES                           
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PTOTAL,=CL12'TOTAL B TIME'                                       
         CURED BTOTAL,(L'PTOTAL#,PTOTAL#),2,MINUS=YES                           
         CURED B$TOTAL,(L'PTOTAL$,PTOTAL$),2,MINUS=YES                          
*&&UK*&& CURED TXTOTAL,(L'PTXTOTAL,PTXTOTAL),2,MINUS=YES                        
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PTOTAL,=CL12'TOTAL R TIME'                                       
         CURED RTOTAL,(L'PTOTAL#,PTOTAL#),2,MINUS=YES                           
         GOTO1 SPOOL,DMCB,(R7)                                                  
         ZAP   DUB,NTOTAL                                                       
         AP    DUB,BTOTAL                                                       
         AP    DUB,RTOTAL                                                       
         MVC   PTOTAL,=CL12'TOTAL'                                              
         CURED DUB,(L'PTOTAL#,PTOTAL#),2,MINUS=YES                              
         CURED B$TOTAL,(L'PTOTAL$,PTOTAL$),2,MINUS=YES                          
*&&UK*&& CURED TXTOTAL,(L'PTXTOTAL,PTXTOTAL),2,MINUS=YES                        
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
*        PRINT NUMBER OF SAVED ITEMS                                            
*                                                                               
         DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R7)                                                  
         MVC   PLINE1,BCSPACES     CLEAR PLINE                                  
         GOTO1 ACNTSAVE            SUBROUTINE TO FIND # OF SAVED ITEMS          
         BE    PRX                 IF EQUAL NO SAVED ITEMS FOUND                
         MVC   PLINE1(25),=CL25'NUMBER OF SAVED ITEMS = '                       
         SR    R3,R3                                                            
         ICM   R3,3,BCHALF         BCHALF CONTAINS # OF SAVED ITEMS             
         CURED (R3),(3,PLINE1+25),0,ALIGN=LEFT                                  
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
PRX      XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(4),=C'$DQU'                                              
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TYPE OF TIME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISTYPE  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM                                                      
         USING TSARRECD,R6                                                      
         L     R6,AIO1                                                          
         LA    R1,PTYPE                                                         
         TM    TRKSTAT,TRKSUPDT    TEST IF MARKED UPDATED                       
         BO    *+12                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
*                                                                               
         MVI   0(R1),C'B'          B TIME                                       
         CLI   TIMTTYP,TIMTCB                                                   
         BE    DSTYPE10                                                         
         MVI   0(R1),C'R'          R TIME                                       
         CLI   TIMTTYP,TIMTCR                                                   
         BE    DSTYPE10                                                         
         MVI   0(R1),C'N'          N TIME                                       
*                                                                               
DSTYPE10 TM    TIMIND,TIMIADJ      ADJUSTED                                     
         BNO   *+8                                                              
         MVI   PTYPE+1,C'A'                                                     
         TM    TIMIND,TIMIWO       WRITEOFF                                     
         BNO   *+8                                                              
         MVI   PTYPE+1,C'W'                                                     
         TM    TIMSTAT,TIMTEMPO    UPLOAD ITEM                                  
         BNO   *+8                                                              
         MVI   PTYPE+1,C'T'                                                     
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HOURS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         CURED TIMHRS,(L'PHOURS,PHOURS),2,MINUS=YES                             
*                                                                               
         USING TSARRECD,R6                                                      
         L     R6,AIO1             DONT ADD SAVED ITEMS INTO TOTAL              
         TM    TRKSTAT,TRKSUPDT                                                 
         BNO   ROUTE                                                            
*                                                                               
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   *+14                                                             
         AP    BTOTAL,TIMHRS                                                    
         B     ROUTE                                                            
         CLI   TIMTTYP,TIMTCR                                                   
         BNE   *+14                                                             
         AP    RTOTAL,TIMHRS                                                    
         B     ROUTE                                                            
         AP    NTOTAL,TIMHRS                                                    
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CLIENT/PRODUCT/JOB/TASK                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCPJT  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVC   SVSJACT,BCSPACES                                                 
         MVC   SVSJCLNM,BCSPACES                                                
         MVC   SVSJPRNM,BCSPACES                                                
*                                                                               
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         CLC   TIMACC(2),=C'1N'                                                 
         BNE   DSCPJ10                                                          
         GOTO1 GTLEVNM,BCDMCB,C'1N',TIMACC+2,0                                  
         MVC   SV1NNAME,WORK                                                    
         MVC   PCLIENT,TIMACC+2                                                 
         B     DSCPJX                                                           
*                                                                               
DSCPJ10  MVC   SVSJCPY,CMPY        *** DISPLAY CLIENT CODE ***                  
         MVC   SVSJUL,TIMACC                                                    
         LA    RF,SVSJCLI                                                       
         SR    R1,R1                                                            
         IC    R1,BCSJLEV1                                                      
         CH    R1,=Y(L'SVSJCLI)   MAX LENGTH OF MOVE - L'(SVSJCLI)              
         BNH   *+8                                                              
         LA    R1,L'SVSJCLI                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),TIMACC+2                                                 
*                                                                               
         LA    RF,PCLIENT                                                       
         SR    R1,R1                                                            
         IC    R1,BCSJLEV1                                                      
         CH    R1,=Y(L'PCLIENT)   MAX LENGTH OF MOVE - L'(PCLIENT)              
         BNH   *+8                                                              
         LA    R1,L'PCLIENT                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),SVSJCLI                                                  
         GOTO1 GTLEVNM,BCDMCB,C'SJ',SVSJCODE,0                                  
         MVC   SVSJCLNM,WORK       GETS CLIENT NAME                             
*                                                                               
DSCPJ20  LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ1                                                      
         AR    RF,R1               BUMP PAST 1ST LEVEL - PROD LEVEL             
         LA    RE,SVSJPRO                                                       
         SR    R1,R1                                                            
         IC    R1,BCSJLEV2                                                      
         CH    R1,=Y(L'SVSJPRO)   MAX LENGTH OF MOVE - L'(SVSJPRO)              
         BNH   *+8                                                              
         LA    R1,L'SVSJPRO                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
         LA    RF,PPROD                                                         
         SR    R1,R1                                                            
         IC    R1,BCSJLEV2                                                      
         CH    R1,=Y(L'PPROD)     MAX LENGTH OF MOVE - L'(PPROD)                
         BNH   *+8                                                              
         LA    R1,L'PPROD                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),SVSJPRO                                                  
         CLC   SVSJPRO,BCSPACES                                                 
         BE    DSCPJ30                                                          
         GOTO1 GTLEVNM,BCDMCB,C'SJ',SVSJCODE,0                                  
         MVC   SVSJPRNM,WORK       GETS PRODUCT NAME                            
*                                                                               
DSCPJ30  LA    RF,TIMACC+2                                                      
         SR    R1,R1                                                            
         IC    R1,BCSJLNQ2                                                      
         AR    RF,R1               BUMP PAST 2ND LEVEL - JOB LEVEL              
         LA    RE,PJOB                                                          
         SR    R1,R1                                                            
         IC    R1,BCSJLEV3                                                      
         CH    R1,=Y(L'PJOB)      MAX LENGTH OF MOVE - L'(PJOB)                 
         BNH   *+8                                                              
         LA    R1,L'PJOB                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)                                                    
         MVC   PTASK,TIMTSK                                                     
*                                                                               
DSCPJX   MVC   AIO,AIO1                                                         
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY MOA                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISMOA   NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         MVC   YYMMDD(2),TIMMOA    DISPLAY MOA                                  
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,BCDMCB,(1,YYMMDD),(9,WORK)                                
         MVC   PMOA,WORK                                                        
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRATE  NTR1                                                                   
         TM    BCFLAG5,BCFLRTAM    IS THERE FULL SECURITY ON FIELD              
         BO    DISRX               YES - SKIP                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BNE   ROUTE                                                            
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'*'                                                      
         TM    TIMRBSTA,TIMRBADJ   DEFAULT ADJUSTMENT RATE                      
         BNO   *+8                                                              
         MVI   BCWORK,C'A'                                                      
         CURED TIMRATE,(L'PRATE,BCWORK+1),2,ALIGN=LEFT                          
         LA    R1,BCWORK                                                        
         TM    TIMRBSTA,TIMRORAT   WAS RATE OVERRIDDEN                          
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   PRATE,0(R1)                                                      
DISRX    B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY AMOUNT                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISAMT   NTR1                                                                   
         TM    BCFLAG5,BCFLRTAM    IS THERE FULL SECURITY ON FIELD              
         BO    DISAX               YES - SKIP                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEM                           
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLBLE INPUT                     
         BNE   ROUTE                                                            
         CURED TIMAMNT,(L'PAMOUNT,PAMOUNT),2,FLOAT=-                            
         USING TSARRECD,R6                                                      
         L     R6,AIO1             DONT ADD SAVED ITEMS INTO TOTAL              
         TM    TRKSTAT,TRKSUPDT                                                 
         BNO   ROUTE                                                            
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   ROUTE                                                            
         AP    B$TOTAL,TIMAMNT                                                  
DISAX    B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY INCOME ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
DISINC   NTR1                                                                   
         TM    BCFLAG5,BCFLINC     FULL SECURITY ON INCOME ACCT?                
         BO    DISIX               YES - SKIP                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEMENT                        
         CLI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE INPUT                    
         BNE   ROUTE                                                            
         CLC   TIMINC,BCSPACES                                                  
         BNH   ROUTE                                                            
         MVC   WORK,BCSPACES                                                    
         LA    R1,WORK                                                          
         TM    TIMRBSTA,TIMROINC   INCOME ACCOUNT OVERRIDDEN                    
         BO    *+12                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'TIMINC,R1),TIMINC                                            
         MVC   PINCOME,WORK                                                     
DISIX    B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY COSTING ACCOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
DISCOST  NTR1                                                                   
         USING TIMELD,R3                                                        
         L     R3,AINPELEM         INPUT DETAILS ELEMENT                        
         CLI   TIMTTYP,TIMTNC      NO 1C ACCOUNT FOR NON CLIENT TIME            
         BE    ROUTE                                                            
         USING TSARRECD,R6                                                      
         L     R6,AIO1                                                          
         MVC   PCOST,TRKCNTRA      1C ACCOUNT                                   
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ACCOUNT NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISNAME  NTR1                                                                   
         MVC   BLOCK(100),BCSPACES                                              
         MVC   BLOCK(L'SVSJCLNM),SVSJCLNM                                       
         CLC   SVSJPRNM,BCSPACES                                                
         BNH   DISNAM10                                                         
         MVI   BLOCK+L'SVSJCLNM+2,C'/'                                          
         MVC   BLOCK+L'SVSJCLNM+5(L'SVSJPRNM),SVSJPRNM                          
         GOTO1 SQUASHER,BCDMCB,BLOCK,100                                        
*                                                                               
DISNAM10 MVC   PNAME,BLOCK                                                      
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY NARRATIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISNAR   NTR1                                                                   
         LA    R2,BCWORK                                                        
         MVC   BCWORK,BCSPACES                                                  
*                                                                               
         USING TIMELD,R3                                                        
         CLI   TWAOFFC,C'*'                                                     
         BNE   DISNAR10                                                         
         L     R3,AINPELEM         INPUT DETAILS ELEMENT                        
         GOTO1 HEXOUT,BCDMCB,TIMLINE#,BCWORK,L'TIMLINE#                         
         LA    R2,5(R2)                                                         
*                                                                               
DISNAR10 ICM   R3,15,ANARELEM      NARRATIVE DETAIL ELEMENT                     
         BZ    DISNAR20                                                         
         SR    R1,R1               R1=LENGTH ELEMENT DATA                       
         IC    R1,TIMLN                                                         
         SH    R1,=Y(TIMHLNQ+1)                                                 
         BM    ROUTE                                                            
         CHI   R1,L'STCTCNAR-1     60 CHAR NARRATIVE                            
         BNH   *+8                                                              
         LHI   R1,L'STCTCNAR-1                                                  
         MVC   0(0,R2),TIMNARR                                                  
         EX    R1,*-6                                                           
*                                                                               
DISNAR20 MVC   PNARR,BCWORK                                                     
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TAX INFORMATION                                             *         
***********************************************************************         
         SPACE 1                                                                
DISTAX   NTR1                                                                   
         USING TIMELD,R3                                                        
         SR    R3,R3                                                            
         ICM   R3,15,ATAXELEM      TAX ELEMENT                                  
         BZ    ROUTE                                                            
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         CURED TIMTBAS,(L'PBASIS,BCWORK),2,FLOAT=-                              
         MVC   PBASIS,BCWORK                                                    
         MVC   PLOCAL,TIMTLOC                                                   
         MVC   PTXWC,TIMTWC                                                     
*                                                                               
         ZAP   BCDUB,=P'0'         GET TOTAL TAX AMOUNT                         
         SR    RF,RF                                                            
         ICM   RF,1,TIMTMINI       # MINI ELEMENTS                              
         BZ    ROUTE                                                            
         LA    R1,TIMTMINS                                                      
         AP    BCDUB,TIMTAMNT                                                   
         LA    R1,TIMTMINQ(R1)                                                  
         BCT   RF,*-10                                                          
         CURED BCDUB,(L'PTXAMNT,PTXAMNT),2,FLOAT=-                              
         USING TSARRECD,R6                                                      
         L     R6,AIO1             DONT ADD SAVED ITEMS INTO TOTAL              
         TM    TRKSTAT,TRKSUPDT                                                 
         BNO   ROUTE                                                            
         AP    TXTOTAL,BCDUB                                                    
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENTED FIELDS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCM    NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVC   P,BCSPACES          CLEAR  PRINT LINE1                           
         MVC   P2,BCSPACES         CLEAR  PRINT LINE2                           
         MVC   P3,BCSPACES         CLEAR  PRINT LINE3                           
         ZAP   PKTXBAS,=P'0'       ZERO BASIS TAX FIELD FOR COMPARING           
         XC    FLAG,FLAG           CLEAR FLAG                                   
         XC    SVMINI,SVMINI       CLEAR SAVED AREA FOR # MINI                  
         XC    FLDREQ,FLDREQ       CLEAR REQUIRED FIELD MARKER                  
         MVC   PNARR,BCSPACES      CLEAR PNARR                                  
*                                                                               
* IN ORDER TO FILTER ON ANY OPTION FILTERS SELECTED, I HAVE TO CHECK            
* AND MAKE SURE THAT THE LINE PASSES THE FILTERS BEFORE PRINTING.               
* SO I LOOP AROUND THE ELEMENT CHECKING THE FILTERS AND IF IT PASSES            
* I CONTINUE - ELSE I EXIT.                                                     
*                                                                               
         OC    BCFLTS,BCFLTS       ANY FILTERS?                                 
         BZ    VALCM60                                                          
*                                                                               
         LA    R0,FLTTNUM          NUMBER OF ENTRIES IN FILTER TABLE            
         LA    R1,BCFLTS           R1=A(FILTERS)                                
         USING FLTTBLD,R2                                                       
         L     R2,AFLTTAB          R2=A(FILTER TABLE)                           
*                                                                               
VALCM10  SR    RE,RE                                                            
         IC    RE,FLTFLDLN                                                      
         BCTR  RE,0                                                             
         STC   RE,BYTE             SAVE LENGTH BECAUSE RE IS USED LATER         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),BCSPACES                                                 
         BH    VALCM30                                                          
VALCM20  SR    RE,RE                                                            
         IC    RE,BYTE                                                          
         LA    R1,1(RE,R1)         BUMP TO NEXT FILTER FIELD                    
         LA    R2,FLTTBLNQ(R2)                                                  
         BCT   R0,VALCM10                                                       
         B     VALCM60                                                          
*                                                                               
         USING TIMELD,R3                                                        
VALCM30  L     R3,ACOMELEM         R3 = A(COMELEM)                              
         SR    RF,RF                                                            
         IC    RF,TIMFMINI         NUMBER OF MINI ELEMENTS                      
VALCM40  CLC   FLTFLD,TIMFNUM      COMPARE FIELD NUM WITH ELEM NUM              
         BE    VALCM50               IF NOT EQUAL BUMP ELEMENT                  
         SR    RE,RE                                                            
         ICM   RE,1,TIMFLEN                                                     
         BZ    VALCM20                                                          
         AR    R3,RE               ADD LENGTH TO BUMP TO NEXT ELEM              
         BCT   RF,VALCM40                                                       
         B     VALCMX              DON'T INCLUDE-DOESN'T MATCH FILT             
*                                                                               
VALCM50  SR    RE,RE                                                            
         IC    RE,TIMFLEN                                                       
         SH    RE,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),TIMFIELD    CHECK IF FIELDS MATCH                        
         BNE   VALCMX                                                           
         B     VALCM20             CONTINUE LOOP                                
         DROP  R2                                                               
*                                                                               
* IN ORDER TO HAVE CERTAIN ROUTINES RUN BEFORE OTHERS I LOOP THROUGH            
* THE ELEMENT INSTEAD OF THE TABLE.  THE TABLE IS SET UP IN THE ORDER           
* THE ROUTINES SHOULD RUN.  THE NUMBER OF MINI ELEMENTS IS IN STORAGE           
* SO THAT THE BCT REGISTER CAN CONTINUALLY BE RESET ONCE ONE OF THE             
* TABLE ENTRIES ARE FOUND OR SKIPPED.                                           
*                                                                               
VALCM60  L     R3,ACOMELEM         R3 = A(COMELEM)                              
*                                                                               
         GOTO1 HEXOUT,BCDMCB,TIMFLINE,PLIN#,L'TIMFLINE                          
         L     R1,AVALTAB          R1 = A(VALIDATION TABLE)                     
         SR    R2,R2                                                            
         IC    R2,TIMFMINI         INSERT NUMBER OF MINI ELEMENTS               
         STC   R2,SVMINI           STORE NUMBER OF MINI ELEMENTS                
VALCM70  CLI   0(R1),X'FF'                                                      
         BE    VALCMX                                                           
VALCM80  CLC   0(2,R1),TIMFNUM       COMPARE FIELD NUM WITH TABLE               
         BE    VALCM90               IF NOT EQUAL BUMP TABLE                    
         SR    RF,RF                                                            
         ICM   RF,1,TIMFLEN                                                     
         BZ    VALCMX                                                           
         AR    R3,RF               ADD LENGTH TO BUMP TO NEXT ELEM              
         BCT   R2,VALCM80                                                       
         LA    R1,VALTABLN(R1)     BUMP TABLE TO NEXT FIELD NUMBER              
         L     R3,ACOMELEM         RESET R3 = A(COMMENTED ELEMENT)              
         SR    R2,R2               INITIAL R2 FOR BCT LOOP                      
         IC    R2,SVMINI           RESET R2 WITH # MINIELEMENT                  
         B     VALCM70             BRANCH TO CHECK NEXT TABLE ENTRY             
VALCM90  SR    RF,RF               INITIALIZE RF FOR BASR                       
         ICM   RF,15,2(R1)         INSERT A(VAL ROUTINE) INTO RF                
         A     RF,RELO             ADD RELO TO FIND CORRECT ADDRESS             
         BASR  RE,RF               BRANCH TO VALIDATION ROUTINE                 
         LA    R1,VALTABLN(R1)     BUMP TABLE TO NEXT FIELD NUMBER              
         L     R3,ACOMELEM         RESET R3 = A(COMMENTED ELEMENT)              
         SR    R2,R2               INITIALIZE R2 FOR BCT LOOP                   
         IC    R2,SVMINI           RESET R2 WITH # MINI-ELEMENTS                
         B     VALCM70             BRANCH TO FIND NEXT ROUTINE                  
*                                                                               
VALCMX   BAS   RE,VLFLD                                                         
         GOTO1 SPOOL,DMCB,(R7)     GOTO PRINT                                   
         MVC   AIO,AIO1            RESET AIO                                    
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TYPE ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLTYP    NTR1                                                                   
         SR    R1,R1               INITIALIZING R1 FOR EX MVC                   
         ICM   R1,1,TIMFLEN        INSERT ELEMENT LENGTH INTO R1                
         BNZ   *+6                                                              
         DC    H'0'                ELEMENT LENGTH CANNOT BE ZERO                
         SH    R1,=Y(TIMFITMQ)     SUBTRACT OVERHEAD                            
         CH    R1,=H'1'            CHECK IF FIELD IS ONE BYTE OR TWO            
         BNH   VLTYP05                                                          
         CLI   TIMFIELD+1,C'B'     CHECK IF TYPE IS B TIME                      
         BNE   *+20                                                             
         OC    FLDREQ,=AL2(FLDHRS+FLDCLI+FLDPRD+FLDJOB)                         
         OC    FLDREQ,=AL2(FLDTSK+FLDMOA+FLDRATE+FLDINC)                        
         B     VLTYP10                                                          
         CLI   TIMFIELD+1,C'R'     CHECK IF TYPE IS R TIME                      
         BNE   *+14                                                             
         OC    FLDREQ,=AL2(FLDHRS+FLDCLI+FLDMOA+FLDRATE)                        
         B     VLTYP10                                                          
         CLI   TIMFIELD+1,C'N'     CHECK IF TYPE IS N TIME                      
         BNE   *+14                                                             
         OC    FLDREQ,=AL2(FLDHRS+FLDCLI+FLDMOA)                                
         B     VLTYP10                                                          
*                                                                               
VLTYP05  MVC   GERROR,=AL2(ACEIVTYP)                                            
         BAS   RE,ERROUT                                                        
*                                                                               
VLTYP10  SH    R1,=H'1'            SUBTRACT OVERHEAD +1                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PTYPE(0),TIMFIELD   MOVE TYPE TO PRINT LINE                      
*                                                                               
VLTYPX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HOURS ROUTINE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLHRS    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDHRS) TURNOFF HOURS BIT                     
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLHRS10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE THAT BUILDS HEADER/DATA           
         LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         GOTO1 AVALHRS,(R2)                                                     
         BE    *+8                                                              
         BAS   RE,ERROUT                                                        
*                                                                               
VLHRS10  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PHOURS(0),TIMFIELD  MOVE HOURS TO PRINT LINE                     
*                                                                               
VLHRSX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT ROUTINE                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLCLI    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDCLI) TURNOFF CLIENT BIT                    
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLCLI10             IF ON JUST SEND OUTPUT TO PRINT LINE         
*                                                                               
*        TEST '1N' ACCOUNT                                                      
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES     CLEAR KEY                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1N'                                                
         BAS   RE,GETCPJ           GET CLIENT/PROD/JOB                          
         MVC   ACTKACT,BCLFLD      MOVE IN CPJ                                  
         GOTO1 AGETACT,0                                                        
         BE    VLCLI10                                                          
*                                                                               
*        IF NOT 1N ACCOUNT                                                      
*                                                                               
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         OI    BCFLAG1,BCFL1CLI    VALIDATE CLIENT                              
         GOTO1 AVALCPJ,(R2)        VALIDATE C/P/J ROUTINE                       
         BE    VLCLI10                                                          
         BAS   RE,ERROUT           GET ERROR MESSAGE AND PUT TO PRINT           
*                                                                               
VLCLI10  NI    BCFLAG1,TURNOFF-BCFL1CLI   TURNOFF CLIENT FLAG                   
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PCLIENT(0),TIMFIELD    MOVE CLIENT TO PRINT LINE                 
*                                                                               
VLCLIX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT ROUTINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLPRD    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDPRD) TURNOFF PRODUCT BIT                   
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLPRD10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         OI    BCFLAG1,BCFL1PRD    VALIDATE PRODUCT                             
         GOTO1 AVALCPJ,(R2)        VALIDATE C/P/J ROUTINE                       
         BE    VLPRD10                                                          
         BAS   RE,ERROUT           GET ERROR MESSAGE AND PUT TO PRINT           
*                                                                               
VLPRD10  NI    BCFLAG1,TURNOFF-BCFL1PRD   TURNOFF PRODUCT FLAG                  
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PPROD(0),TIMFIELD   MOVE PRODUCT TO PRINT LINE                   
*                                                                               
VLPRDX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLJOB    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDJOB) TURNOFF JOB BIT                       
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLJOB10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         OI    BCFLAG1,BCFL1JOB    VALIDATE JOB                                 
         GOTO1 AVALCPJ,(R2)        VALIDATE C/P/J ROUTINE                       
         BE    VLJOB10                                                          
         BAS   RE,ERROUT           GET ERROR MESSAGE AND PUT TO PRINT           
*                                                                               
VLJOB10  NI    BCFLAG1,TURNOFF-BCFL1JOB   TURNOFF JOB FLAG                      
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PJOB(0),TIMFIELD    MOVE JOB TO PRINT LINE                       
*                                                                               
VLJOBX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TASK ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLTSK    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDTSK) TURNOFF TASK BIT                      
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLTSK10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         USING WCORECD,R6                                                       
         LA    R6,BIGKEY           R6 = A(BIGKEY)                               
         XC    BIGKEY,BIGKEY       CLEAR KEY                                    
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ    X'0A' WORK - CODE RECORD TYPE                
         MVC   WCOKCPY,CMPY                                                     
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,TIMFIELD    TASK CODE                                    
         GOTO1 AGETACT,0                                                        
         BE    VLTSK10                                                          
         MVC   GERROR,=AL2(ACETASK)   INVALID TASK                              
         BAS   RE,ERROUT                                                        
*                                                                               
VLTSK10  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PTASK(0),TIMFIELD   MOVE TASK TO PRINT LINE                      
*                                                                               
VLTSKX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOA ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLMOA    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDMOA) TURNOFF MOA BIT                       
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLMOA10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLD                                                        
         ICM   R2,8,BCLFLDH+5                                                   
         GOTO1 PERVAL,DMCB,(2),(X'40',BCWORK)                                   
         TM    DMCB+4,X'01'        DID PERVAL RETURNED AN INVAL DATE            
         BNO   VLMOA10               IF NOT SKIP ERROR ROUTINE                  
         MVC   GERROR,=Y(ACEIVDT)    INVALID DATE                               
         BAS   RE,ERROUT                                                        
*                                                                               
VLMOA10  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PMOA(0),TIMFIELD    MOVE MOA TO PRINT LINE                       
*                                                                               
VLMOAX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATE ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLRTE    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDRATE)     TURNOFF RATE BIT                 
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLRTE10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH                                                       
         GOTO1 =V(NUMVAL),DMCB,(2,8(R2)),(1,0),RR=RELO                          
         CLI   0(R1),X'FF'         DID NUMVAL RETURN AN INVALID MESS            
         BNE   VLRTE10               IF NOT SKIP ERROR ROUTINE                  
         MVC   GERROR,=Y(ACEMSRAT)   INVALID RATE                               
         BAS   RE,ERROUT                                                        
*                                                                               
VLRTE10  TM    BCFLAG5,BCFLRTAM    IS THERE FULL SECURITY ON FIELD              
         BO    VLRTEX              YES - SKIP                                   
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PRATE(0),TIMFIELD    MOVE RATE TO PRINT LINE                     
*                                                                               
VLRTEX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INCOME ACCOUNT ROUTINE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLINC    NTR1                                                                   
         NC    FLDREQ,=AL2(FLDEFS-FLDINC)      TURNOFF RATE BIT                 
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLINC40             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH          R2 = A(DATA FIELD)                           
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          INSERT DATA LEN INTO R1 FOR EX MVC           
         BNZ   *+18                    IF ZERO GIVE ERROR MESSAGE               
         MVC   GERROR,=Y(ACEMSINC)     MISSING INCOME ACCOUNT                   
         BAS   RE,ERROUT               GOTO ERROR ROUTINE                       
         B     VLINC40                 AND PRINT OUT DATA                       
         BCTR  R1,0                SUBTRACT 1 FOR ACTUAL LENGTH                 
         LA    R2,8(R2)            R2 = A(DATA FIELD)                           
         CLI   0(R2),C'*'          CHECK INC ACCT FOR A LEADING STAR            
         BNE   *+10                IF NOT START BUILDING KEY                    
         LA    R2,1(R2)            R2 = A(START OF DATA FIELD W/O *)            
         BCTR  R1,0                SUBTRACT 1 FOR STAR (*)                      
*                                                                               
*        BUILD KEY AND GET ACCOUNT                                              
*                                                                               
         MVC   SAVEKEY,BCSPACES    CLEAR TEMP KEY                               
         MVC   SVKULA,0(R2)        MOVE IN KEY TO TEMP KEY                      
         OC    SAVEKEY,BCSPACES    CAPITALIZE KEY                               
         XC    BIGKEY,BIGKEY       CLEAR BIGKEY                                 
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES     CLEAR KEY!!                                  
         MVC   ACTKCPY,CMPY        MOVE COMPANY INTO KEY                        
         EX    R1,*+4              R1 CONTAINS PREVIOUSLY FOUND LEN             
         MVC   ACTKULA(0),SVKULA   MOVE IN U/L/ACC INTO KEY                     
         OC    BIGKEY,BCSPACES     CAPITALIZE KEY                               
         GOTO1 AGETACT,0                                                        
         BNE   VLINC10             IF FOUND SKIP ERROR ROUTINE                  
*                                                                               
         TM    BCASTAT1,RSTSACIC+RSTSACIL                                       
         BO    VLINC10                                                          
         TM    BCINDS,BCFLABAL                                                  
         BO    VLINC20                                                          
*                                                                               
VLINC10  MVC   GERROR,=Y(ACEIINCM)   INVALID INCOME ACCOUNT                     
         BAS   RE,ERROUT                                                        
         B     VLINC40                                                          
*                                                                               
VLINC20  CLC   SVKUL,=C'SK'        CHECK IF U/L IS 'SK'                         
         BE    VLINC40                                                          
         XC    BIGKEY,BIGKEY                                                    
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'12'                                                
         MVC   ACTKACT(1),BCACOST                                               
         CLC   BCSPANAL,BCSPACES                                                
         BNH   *+10                                                             
         MVC   ACTKCULA,BCSPANAL                                                
         GOTO1 AGETACT,0                                                        
         BNE   VLINC30             INVALID ANALSIS ACCOUNT                      
*                                                                               
         TM    BCASTAT1,RSTSACIC+RSTSACIL                                       
         BO    VLINC30                                                          
         TM    BCINDS,BCFLABAL     VALID FOR POSTING                            
         BO    VLINC40                                                          
*                                                                               
VLINC30  MVC   GERROR,=Y(ACEIANAL)   INVALID ANALYSIS ACCOUNT                   
         BAS   RE,ERROUT                                                        
*                                                                               
VLINC40  TM    BCFLAG5,BCFLINC       FULL SECURITY ON INCOME ACCT?              
         BO    VLINCX                YES - SKIP                                 
         SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PINCOME(0),TIMFIELD    MOVE INC ACCT TO PRINT LINE               
*                                                                               
VLINCX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE NARRATIVE ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLNAR    NTR1                                                                   
         SR    R1,R1               INITIALIZING R1 FOR EX MVC                   
         ICM   R1,1,TIMFLEN        INSERT ELEMENT LENGTH INTO R1                
         BZ    VLNARX                                                           
         SH    R1,=Y(TIMFITMQ+1)      SUBTRACT OVERHEAD +1                      
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PNARR2(0),TIMFIELD    MOVE NARRATIVE TO PRINT LINE               
*                                      PAST LINE NUMBER 1ST 5 CHARS             
VLNARX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX BASIS ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLBAS    NTR1                                                                   
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLBAS20             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH                                                       
         GOTO1 =V(NUMVAL),DMCB,(2,8(R2)),(1,0),RR=RELO                          
         CLI   0(R1),X'FF'         DID NUMVAL RETURN AN INVALID MESS            
         BNE   VLBAS10               IF NOT SKIP ERROR ROUTINE                  
         MVC   GERROR,=Y(ACEINVBA)   INVALID TAX                                
         BAS   RE,ERROUT                                                        
         B     VLBAS20                                                          
*                                                                               
VLBAS10  ZAP   PACK16,=P'0'        INITIALIZE PACK16 FOR CALCULATIONS           
         L     R2,4(R1)            LOAD VALIDATED NUMBER INTO R2                
         CVD   R2,DUB                                                           
         ZAP   PACK16,DUB                                                       
         DP    PACK16,=PL8'100'    NUMBER IN CENTS-CONVERT TO DOLLARS           
         ZAP   PKTXBAS,PACKQ                                                    
*                                                                               
VLBAS20  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PBASIS(0),TIMFIELD  MOVE TAX TO PRINT LINE                       
*                                                                               
VLBASX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LOCALITY ROUTINE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLLOC    NTR1                                                                   
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLLOC10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         CP    PKTXBAS,=P'0'       COMPARE IF BASIS TAX WAS ENTERED             
         BNE   *+18                                                             
         MVC   GERROR,=Y(ACEITAX)                                               
         BAS   RE,ERROUT                                                        
         BE    VLLOC10                                                          
         BAS   RE,BLDHD                                                         
         GOTO1 AVALLOC,DMCB,BCLFLD,PRDENDTE,PKTXBAS                             
         BE    VLLOC10                                                          
         MVC   GERROR,=Y(ACEITAX)                                               
         BAS   RE,ERROUT                                                        
*                                                                               
VLLOC10  SR    R1,R1               INITIALIZING R1 FOR EX MVC                   
         ICM   R1,1,TIMFLEN        INSERT ELEMENT LENGTH INTO R1                
         BZ    VLNARX                                                           
         SH    R1,=Y(TIMFITMQ+1)   SUBTRACT OVERHEAD +1                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PLOCAL(0),TIMFIELD  MOVE LOCALITY TO PRINT LINE                  
*                                                                               
VLLOCX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TAX WORK ROUTINE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLTXW    NTR1                                                                   
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLTXW10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLD                                                        
         GOTO1 AVALTXWC,(R2)                                                    
         BE    VLTXW10               IF EQUAL SKIP ERROR ROUTINE                
         MVC   GERROR,=Y(ACEITXWC)                                              
         BAS   RE,ERROUT                                                        
*                                                                               
VLTXW10  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PTXWC(0),TIMFIELD   MOVE TAX TO PRINT LINE                       
*                                                                               
VLTXWX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMOUNT ROUTINE                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
VLAMT    NTR1                                                                   
         TM    FLAG,FLAGERR        TEST IF ERROR FLAG ALREADY SET               
         BO    VLAMT10             IF ON JUST SEND OUTPUT TO PRINT LINE         
         BAS   RE,BLDHD            SUBROUTINE TO BUILD HEADER/DATA              
         LA    R2,BCLFLDH                                                       
         GOTO1 =V(NUMVAL),DMCB,(2,8(R2)),(1,0),RR=RELO                          
         CLI   0(R1),X'FF'         DID NUMVAL RETURN AN INVALID MESS            
         BNE   VLAMT10               IF NOT SKIP ERROR ROUTINE                  
         MVC   GERROR,=Y(ACEAMNT)    INVALID AMOUNT                             
         BAS   RE,ERROUT                                                        
*                                                                               
VLAMT10  SR    R1,R1                                                            
         IC    R1,TIMFLEN                                                       
         SH    R1,=Y(TIMFITMQ+1)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PAMOUNT(0),TIMFIELD  MOVE AMOUNT TO PRINT LINE                   
*                                                                               
VLAMTX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FIELD REQUIREMENT ROUTINE                                  *         
***********************************************************************         
         SPACE 1                                                                
VLFLD    NTR1                                                                   
         OC    FLDREQ,FLDREQ       CHECK IF EMPTY OR NOT                        
         BZ    VLFLDX                IF IT IS - EXIT                            
*                                                                               
         MVC   BCWORK,BCSPACES     CLEAR BCWORK                                 
         LA    R0,ERRTABQ          # OF ENTRIES                                 
         LA    R1,ERRTAB           START OF ERROR TABLE                         
VLFLD10  MVC   BCHALF,0(R1)                                                     
         NC    BCHALF,FLDREQ       CHECK IF ON OR NOT                           
         BNZ   VLFLD20               IF ON EXIT LOOP                            
         LA    R1,L'ERRTAB(R1)                                                  
         BCT   R0,VLFLD10                                                       
         B     ROUTE                                                            
*                                                                               
VLFLD20  MVC   GERROR,2(R1)                                                     
         BAS   RE,ERROUT                                                        
*                                                                               
VLFLDX   B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINE                                                       *         
***********************************************************************         
         SPACE 1                                                                
ERROUT   NTR1                                                                   
         OI    FLAG,FLAGERR        SET BIT IN FLAG AS ERROR FOUND               
         SR    R6,R6                                                            
         ICM   R6,3,GERROR                                                      
         BZ    ERROUTX             IF NO NUMBER PUT JOB TO PRINT LINE           
         GOTO1 GETTXT,BCDMCB,(R6),(60,BCWORK),(C'E',DMCB),0,(X'08',0),0         
         MVC   PERROR,BCWORK    ERROR RETURNED IN BCWORK                        
*                                                                               
ERROUTX  B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET CLIENT/PRODUCT/JOB ROUTINE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
GETCPJ   NTR1                                                                   
         L     R3,ACOMELEM         R3 = A(COMMENTED ELEMENT)                    
         XC    BCLFLD,BCLFLD       CLEAR FIELD                                  
         LA    R2,BCLFLD                                                        
         L     R1,AVALTAB          R1 = A(VALIDATION ROUTINE TABLE)             
         LA    R1,12(R1)           BUMP R1 TO CLIENT CODE                       
GETCPJ10 CLC   TIMFNUM,=Y(FLDJOB)  JOB IS THE LAST FIELD                        
         BH    GETCPJX             IF GREATER THAN JOB EXIT                     
         CLC   TIMFNUM,0(R1)       COMPARE FIELD NUM WITH TABLE                 
         BNE   GETCPJ20              IF NOT EQUAL BUMP TABLE                    
         SR    RF,RF                                                            
         ICM   RF,1,TIMFLEN                                                     
         BZ    GETCPJX                                                          
         SH    RF,=Y(TIMFITMQ+1)   SUBTRACT OVERHEAD                            
         EX    RF,*+4                                                           
         MVC   0(0,R2),TIMFIELD                                                 
         LA    R2,1(RF,R2)         BUMP TO NEXT POSITION INFIELD                
         LA    R1,VALTABLN(R1)     BUMP TABLE                                   
         L     R3,ACOMELEM         RESETTING R3                                 
GETCPJ20 SR    RF,RF                                                            
         ICM   RF,1,TIMFLEN                                                     
         BZ    GETCPJX                                                          
         AR    R3,RF               ADD LENGTH TO BUMP TO NEXT ELEM              
         B     GETCPJ10            REPEAT LOOP                                  
*                                                                               
GETCPJX  B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER AND DATA ROUTINE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R3                                                        
BLDHD    NTR1                                                                   
         XC    BCLFLDH,BCLFLDH     CLEAR HEADER FIELD                           
         MVC   BCLFLD,BCSPACES     CLEAR DATA FIELD                             
         CLC   TIMFNUM,=Y(FLDRATE) CHECK IF BUILDING RATE HEADER                
         BNE   BLDHD10                                                          
         LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         SR    R1,R1               INITIALIZE R1 TO ACCUMULATE LENGTH           
         ICM   R1,1,TIMFLEN        INSERT ELEMENT LENGTH INTO R1                
         BNZ   *+6                                                              
         DC    H'0'                ELEMENT LENGTH CANNOT BE ZERO                
         SH    R1,=Y(TIMFITMQ+1)   SUBTRACT OVHD + * TO GET DATA LEN            
         STC   R1,5(R2)            INSERT LENGTH OF DATA INTO HEADER            
         BCTR  R1,0                SUBTRACT 1 FOR EX MVC                        
         EX    R1,*+4                                                           
         MVC   8(0,R2),TIMFIELD+1  MOVE DATA W/O * INTO DATA FIELD              
         LA    R1,L'BCLFLDH+1(R1)  ADD 8 FOR HEADER/1 FOR PREV BCTR             
         STC   R1,0(R2)            INSERT TOTAL LENGTH INTO HEADER              
         B     BLDHDX                                                           
*                                                                               
BLDHD10  LA    R2,BCLFLDH          R2 = A(BCLFLDH) FIELD HEADER                 
         SR    R1,R1               INITIALIZE R1 TO ACCUMULATE LENGTH           
         ICM   R1,1,TIMFLEN        INSERT ELEMENT LENGTH INTO R1                
         BNZ   *+6                                                              
         DC    H'0'                ELEMENT LENGTH CANNOT BE ZERO                
         SH    R1,=Y(TIMFITMQ)     SUBTRACT OVERHEAD TO GET DATA LEN            
         STC   R1,5(R2)            INSERT LENGTH OF DATA INTO HEADER            
         BCTR  R1,0                SUBTRACT 1 FOR EX MVC                        
         EX    R1,*+4                                                           
         MVC   8(0,R2),TIMFIELD    MOVE DATA INTO DATA FIELD                    
         AH    R1,=H'9'            ADD 8 FOR HEADER/1 FOR PREV BCTR             
         STC   R1,0(R2)            INSERT TOTAL LENGTH INTO HEADER              
*                                                                               
BLDHDX   B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EQUATE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
ERRTAB   DS    0AL4                                                             
         DC    AL2(FLDHRS),AL2(ACEMSHRS)       MISSING HOURS                    
         DC    AL2(FLDCLI),AL2(ACEMSCLI)       MISSING CLIEN                    
         DC    AL2(FLDPRD),AL2(ACEMSPRD)       MISSING PRODUCTS                 
         DC    AL2(FLDJOB),AL2(ACEMSJOB)       MISSING JOB                      
         DC    AL2(FLDTSK),AL2(ACEMSTSK)       MISSING TASK                     
         DC    AL2(FLDMOA),AL2(ACEMSMOA)       MISSING MOA                      
         DC    AL2(FLDRATE),AL2(ACEMSRAT)      MISSING RATE                     
         DC    AL2(FLDINC),AL2(ACEMSINC)       MISSING INCOME ACCNT             
ERRTABQ  EQU   (*-ERRTAB)/L'ERRTAB                                              
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK                                                           *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H1(L'HD1LINE),HD1LINE                                            
         MVC   H2(L'HD2LINE),HD2LINE                                            
         MVC   H3(L'HD3LINE),HD3LINE                                            
         MVC   H4(L'HD4LINE),HD4LINE                                            
         MVC   H5(L'HD5LINE),HD5LINE                                            
         MVC   H6(L'HD6LINE),HD6LINE                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK IF SECURITY IS SET                                            *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
CHKSEC   NTR1                                                                   
         MVI   BCFLAG5,0                                                        
         MVI   BCBYTE1,RTAMFLDQ                                                 
         BAS   RE,FLDSEC           SECURITY TO VIEW RATE/AMOUNT                 
         BE    *+16                  NO SECURITY AT ALL                         
         MVI   BCFLAG5,BCFLRTAM    ASSUME RATE/AMT HAS FULL SECURITY            
         BL    *+8                   IF LOW - FULL SECURITY                     
         MVI   BCFLAG5,BCFLRARD    RATE/AMT HAS READ ONLY SECURITY              
*                                                                               
         MVI   BCBYTE1,INCFLDQ                                                  
         BAS   RE,FLDSEC           SECURITY TO VIEW INCOME                      
         BE    *+20                  NO SECURITY AT ALL                         
         OI    BCFLAG5,BCFLINC     ASSUME INCOME HAS FULL SECURITY              
         BL    *+12                  IF LOW - FULL SECURITY                     
         NI    BCFLAG5,X'FF'-BCFLINC ELSE TURN OFF FULL SECURITY                
         OI    BCFLAG5,BCFLINRD      INCOME HAS READ ONLY SECURITY              
*                                                                               
CHKSX    B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
**********************************************************************          
* CHECK FIELD SECURITY TO DISPLAYING RATE                            *          
**********************************************************************          
         SPACE 1                                                                
FLDSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R2,BCBYTE1                                                       
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    ROUTE                                                            
         CLI   DMCB,SECPREAD                                                    
         BE    ROUTH                                                            
         B     ROUTL                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
EXIT     XIT1                                                                   
*                                                                               
ERRRECNF MVC   GERROR,=AL2(ACERECNF)       RECORD NOT FOUND                     
         B     ACCERRX                                                          
ERRMISS  MVC   GERROR,=AL2(ACEMISS)        MISSING INPUT FIELD                  
         B     ACCERRX                                                          
ERRINV   MVC   GERROR,=AL2(ACEINV)         INVALID INPUT FIELD                  
         B     ACCERRX                                                          
EINVCLI  MVC   GERROR,=AL2(ACECLI)                                              
         B     ACCERRX                                                          
EINVPROD MVC   GERROR,=AL2(ACEPROD)                                             
         B     ACCERRX                                                          
EINVJOB  MVC   GERROR,=AL2(ACEJOB)                                              
         B     ACCERRX                                                          
EINVTSK  MVC   GERROR,=AL2(ACETASK)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)                                             
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
EINVLOC  MVC   GERROR,=AL2(ACEIVLOC)                                            
         B     ACCERRX                                                          
EDUPEN   MVC   GERROR,=AL2(ACEDUPEN)                                            
         B     ACCERRX                                                          
EPERIOD  MVC   GERROR,=AL2(ACEPERDF)                                            
         B     ACCERRX                                                          
EINVAMT  MVC   GERROR,=AL2(ACEAMNT)                                             
         B     ACCERRX                                                          
ENOCAL   MVC   GERROR,=AL2(ACENOCAL)                                            
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EHRNOMAT MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ACCERRX                                                          
EENTDATE MVC   GERROR,=AL2(ACEENTDT)                                            
         B     ACCERRX                                                          
ENATMS   MVC   GERROR,=AL2(ACENATMS)                                            
         B     ACCERRX                                                          
ESIANL   MVC   GERROR,=AL2(ACESIANL)                                            
         B     ACCERRX                                                          
EFUTMT   MVC   GERROR,=AL2(ACEFUTMT)                                            
         B     ACCERRX                                                          
EMAX#    MVC   GERROR,=AL2(ACEMAX#)                                             
         B     ACCERRX                                                          
ERRINIT  MVC   GERROR,=AL2(ACEINIRP)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
ENEGCK   MVC   GERROR,=AL2(ACENEGCK)       NEGATIVE HRS ON JOB                  
         MVI   GLTXT,15                                                         
         LA    R1,BCWORK                                                        
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         B     ACCERRX                                                          
EINVACC  MVC   GERROR,=AL2(ACEACCT)        INVALID ACCOUNT                      
ACCERRM  MVI   GLTXT,L'BCACCODE-1                                               
         LA    R1,BCACKUL                                                       
         STCM  R1,7,GATXT          A(INSERTION TEST)                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             DEFAULT IS ACCOUNTING MSG SYSTEM             
         GOTO1 MYERR                                                            
*                                                                               
GENERRX  MVI   GMSYS,X'FF'         GENERAL MSG SYSTEM                           
         MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
PFTABLE  DS    0C                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,(PPF01X-PPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#TIME,8                                                        
         DCDD  AC#LIST,8                                                        
PPF01    DC    AL1(KEYTYTWA,L'REPCODE-1),AL2(REPCODE-T61DFFD)                   
PPF01X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* HEADLINE SPECS                                                      *         
***********************************************************************         
         SPACE 1                                                                
HEDSPEC  DS    0H                                                               
         SSPEC H1,78,RUN                                                        
         SSPEC H2,78,REQUESTOR                                                  
         SSPEC H3,78,REPORT                                                     
         SSPEC H4,78,PAGE                                                       
         SPACE 1                                                                
         SSPEC H7,1,C'TYP'                                                      
         SSPEC H7,5,C'HOURS'                                                    
         SSPEC H7,12,C'CLIENT'                                                  
         SSPEC H7,20,C'PROD'                                                    
         SSPEC H7,25,C'JOB'                                                     
         SSPEC H7,32,C'TK'                                                      
         SSPEC H7,36,C'MOA'                                                     
         SSPEC H7,44,C'RATE'                                                    
         SSPEC H7,53,C'AMOUNT'                                                  
         SSPEC H7,62,C'INCOME ACCOUNT'                                          
         SSPEC H7,78,C'COST ACCOUNT'                                            
*        SSPEC H7,95,C'BASIS'                                                   
*        SSPEC H7,103,C'LOCALITY'                                               
*        SSPEC H7,112,C'W/C'                                                    
*        SSPEC H7,120,C'TAX'                                                    
         SPACE 1                                                                
         SSPEC H8,1,C'---'                                                      
         SSPEC H8,5,C'-----'                                                    
         SSPEC H8,12,C'------'                                                  
         SSPEC H8,20,C'----'                                                    
         SSPEC H8,25,C'---'                                                     
         SSPEC H8,32,C'--'                                                      
         SSPEC H8,36,C'---'                                                     
         SSPEC H8,44,C'----'                                                    
         SSPEC H8,53,C'------'                                                  
         SSPEC H8,62,C'--------------'                                          
         SSPEC H8,78,C'------------'                                            
*        SSPEC H8,95,C'-----'                                                   
*        SSPEC H8,103,C'--------'                                               
*        SSPEC H8,112,C'---'                                                    
*        SSPEC H8,120,C'---'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
VALTAB   DC    AL2(FLDTYPE),AL4(VLTYP)     VALIDATION TABLE                     
         DC    AL2(FLDHRS),AL4(VLHRS)                                           
         DC    AL2(FLDCLI),AL4(VLCLI)                                           
         DC    AL2(FLDPRD),AL4(VLPRD)                                           
         DC    AL2(FLDJOB),AL4(VLJOB)                                           
         DC    AL2(FLDTSK),AL4(VLTSK)                                           
         DC    AL2(FLDMOA),AL4(VLMOA)                                           
         DC    AL2(FLDRATE),AL4(VLRTE)                                          
         DC    AL2(FLDINC),AL4(VLINC)                                           
         DC    AL2(FLDNARR),AL4(VLNAR)                                          
         DC    AL2(FLDBASIS),AL4(VLBAS)                                         
         DC    AL2(FLDLOCAL),AL4(VLLOC)                                         
         DC    AL2(FLDTAXWC),AL4(VLTXW)                                         
         DC    AL2(FLDAMNT),AL4(VLAMT)                                          
VALTABLN EQU   (*-VALTAB)/14                 LENGTH OF ENTRY                    
         DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
FLTTAB   DC    AL2(FLDCLI),AL1(L'BCFLTCLI)  FILTER TABLE                        
         DC    AL2(FLDPRD),AL1(L'BCFLTPRO)                                      
         DC    AL2(FLDJOB),AL1(L'BCFLTJOB)                                      
         DC    AL2(FLDTSK),AL1(L'BCFLTTSK)                                      
         DC    AL2(FLDMOA),AL1(L'BCFSTMOA)                                      
         DC    AL2(FLDMOA),AL1(L'BCFENMOA)                                      
         DC    AL2(FLDTYPE),AL1(L'BCFSTMOA)                                     
FLTTNUM  EQU   (*-FLTTAB)/FLTTBLNQ           LENGTH OF ENTRY                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TABLE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
FLTTBLD  DSECT                                                                  
FLTFLD   DS    XL2                 FILTER FIELD EQUATE                          
FLTFLDLN DS    XL1                 FILTER FIELD LENGTH                          
FLTTBLNQ EQU   *-FLTFLD                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PLINE1   DS    0CL132                                                           
PTYPE    DS    CL2                                                              
         DS    CL1                                                              
PHOURS   DS    CL7                                                              
         DS    CL1                                                              
PCLIENT  DS    CL7                                                              
         DS    CL1                                                              
PPROD    DS    CL4                                                              
         DS    CL1                                                              
PJOB     DS    CL6                                                              
         DS    CL1                                                              
PTASK    DS    CL2                                                              
         DS    CL1                                                              
PMOA     DS    CL6                                                              
         DS    CL1                                                              
PRATE    DS    CL8                                                              
         DS    CL1                                                              
PAMOUNT  DS    CL10                                                             
         DS    CL1                                                              
PINCOME  DS    CL15                                                             
         DS    CL1                                                              
PCOST    DS    CL14                                                             
         DS    CL1                                                              
PNAME    DS    0CL36                                                            
PBASIS   DS    CL9                                                              
         DS    CL1                                                              
PLOCAL   DS    CL8                                                              
         DS    CL1                                                              
PTXWC    DS    CL2                                                              
         DS    CL1                                                              
PTXAMNT  DS    CL10                                                             
         ORG   PLINE1+L'PLINE1                                                  
PLINE2   DS    0CL132                                                           
         DS    CL11                                                             
PNARR    DS    0CL60               NARRATINE FOR NON COMMENTED ELEMENTS         
PLIN#    DS    CL5                 LINE # FOR COMMENTED ELEMENTS                
PNARR2   DS    CL55                NARRATIVE FOR COMMENTED ELEMENTS             
         DS    CL61                                                             
         ORG   PLINE2+L'PLINE2                                                  
PLINE3   DS    0CL132                                                           
         DS    CL11                                                             
PERROR   DS    CL60                                                             
         DS    CL61                                                             
         ORG   PLINE1                                                           
PTOTAL   DS    CL12                                                             
         DS    CL1                                                              
PTOTAL#  DS    CL7                                                              
         DS    CL1                                                              
PTOTAL$  DS    CL12                                                             
         DS    CL1                                                              
PTXTOTAL DS    CL12                                                             
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SCREENS                                                             *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACCAPFFD          BASE SCREEN                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPD3D                                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION AND GLOBAL STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
STARTWRK DS    0A                  IN TWA                                       
RELO     DS    A                                                                
AINPELEM DS    A                   A(X'8B' INPUT DETAIL ELEM)                   
ATAXELEM DS    A                   A(X'8B' TAX DETAIL ELEM)                     
ANARELEM DS    A                   A(X'8B' NARRATIVE ELEM)                      
ACOMELEM DS    A                   A(X'8B' COMMENT ELEM)                        
AVALTAB  DS    A                   A(VALIDATION TABLE)                          
AFLTTAB  DS    A                   A(FILTER TABLE)                              
         DS    2A                  N/D                                          
WORK2    DS    10F                                                              
SVOPT1   DS    XL1                                                              
         DS    XL5                 N/D                                          
SVINCLN  DS    X                   SAVED INCOME ACCOUNT LENGTH                  
SVMINI   DS    XL1                 SAVED NUMBER OF MINI ELEMENTS                
SVLINE   DS    XL2                 SAVED TIMESHEET LINE #                       
FLDREQ   DS    XL2                 REQUIRED FIELD MARKER                        
TURNOFF  EQU   X'FF'               TURNOFF FLAGS                                
FLAG     DS    X                   FLAG FOR ERROR MESSAGES                      
FLAGERR  EQU   X'80'               EQUATED ERROR FLAG                           
SAVEKEY  DS    0XL15               TEMPORARY KEY STORAGE                        
SVKCMPY  DS    XL1                 TEMPORARY COMPANY                            
SVKULA   DS    0XL14               TEMPORARY UNIT/LEGDER/ACCOUNT                
SVKUL    DS    XL2                 TEMPORARY UNIT/LEGDER                        
SVKACC   DS    XL12                TEMPORARY ACCOUNT                            
BCLFLDH  DS    XL8                 TEMPORARY FIELD HEADER                       
BCLFLD   DS    XL80                TEMPORARY FIELD                              
PACK16   DS    0PL16               STORAGE FOR MULTIPLICATION                   
PACKQ    DS    PL8                 PACKED QUOTIENT FIELD                        
PACKR    DS    PL8                 PACKED REMAINDER FIELD                       
PKTXBAS  DS    PL6                 TAX BASIS                                    
PRDSTDTE DS    PL3                 PERIOD START DATE                            
PRDENDTE DS    PL3                 PERIOD ENDING DATE                           
PRDNUM   DS    XL1                 PERIOD NUMBER                                
PRDMON   DS    PL2                 PERIOD MONTH                                 
YYMMDD   DS    PL3                                                              
SVYYMMDD DS    PL3                                                              
REPSTAT  DS    XL1                 REPORT STATUS BYTE                           
RECFOUND EQU   X'80'               RECORD FOUND BYTE                            
*                                                                               
TOTALS   DS    0PL8                                                             
NTOTAL   DS    PL8                                                              
BTOTAL   DS    PL8                                                              
RTOTAL   DS    PL8                                                              
TXTOTAL  DS    PL8                                                              
B$TOTAL  DS    PL8                                                              
TOTALQ   EQU   (*-TOTALS)/L'TOTALS                                              
*                                                                               
HD1LINE  DS    0CL60                                                            
HD1TITLE DS    CL6                                                              
         DS    CL2                                                              
HD1FIELD DS    CL8                                                              
         DS    CL2                                                              
HD1DESC  DS    CL42                                                             
*                                                                               
HD2LINE  DS    0CL60                                                            
HD2TITLE DS    CL6                                                              
         DS    CL2                                                              
HD2FIELD DS    CL8                                                              
         DS    CL2                                                              
HD2DESC  DS    CL42                                                             
*                                                                               
HD3LINE  DS    0CL60                                                            
HD3TITLE DS    CL6                                                              
         DS    CL2                                                              
HD3FIELD DS    CL8                                                              
         DS    CL2                                                              
HD3DESC  DS    CL42                                                             
*                                                                               
HD4LINE  DS    0CL60                                                            
HD4TITLE DS    CL6                                                              
         DS    CL2                                                              
HD4FIELD DS    CL8                                                              
         DS    CL2                                                              
HD4DESC  DS    CL42                                                             
*                                                                               
HD5LINE  DS    0CL60                                                            
HD5TITLE DS    CL6                                                              
         DS    CL2                                                              
HD5FIELD DS    CL8                                                              
         DS    CL2                                                              
HD5DESC  DS    CL42                                                             
*                                                                               
HD6LINE  DS    0CL60               OPTIONS                                      
HD6TITLE DS    CL7                                                              
         DS    CL1                                                              
HD6FIELD DS    CL52                                                             
*                                                                               
SVVKBLCK DS    0C                  *** SAVE TOP OF SCREEN ***                   
SV1RPRSN DS    CL8                 PERSON CODE                                  
SV1ROFFC DS    CL2                 OFFICE CODE                                  
SV1RDPT  DS    CL3                 DEPARTMENT CODE                              
SV1RSDPT DS    CL3                 SUB DEPARTMENT CODE                          
SV1RACT  DS    0XL15               1R ACCOUNT                                   
SV1RCPY  DS    XL1                 1R COMPANY CODE                              
SV1RULA  DS    0CL14               1R UNIT/LEDGER/ACCOUNT                       
SV1RUL   DS    CL2                 1R UNIT/LEDGER                               
SV1RCODE DS    CL12                1R ACCOUNT CODE                              
*                                                                               
SVSJACT  DS    0XL15               SJ ACCOUNT                                   
SVSJCPY  DS    XL1                 SJ COMPANY CODE                              
SVSJULA  DS    0CL14               SJ UNIT/LEDGER/ACCOUNT                       
SVSJUL   DS    CL2                 SJ UNIT/LEDGER                               
SVSJCODE DS    0CL12               SJ ACCOUNT CODE                              
SVSJCLI  DS    CL3                 SJ CLIENT CODE                               
SVSJPRO  DS    CL3                 SJ PRODUCT CODE                              
SVSJJOB  DS    CL6                 SJ JOB CODE                                  
SV1NNAME DS    0CL36                                                            
SVSJCLNM DS    CL36                                                             
SVSJPRNM DS    CL36                                                             
SVVKBLKQ EQU   *-SVVKBLCK                                                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION AND GLOBAL STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE ACCAP30GW                                                      
       ++INCLUDE ACCAP30DST                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASECRETD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACCAP43   07/26/17'                                      
         END                                                                    
