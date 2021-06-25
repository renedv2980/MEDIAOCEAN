*          DATA SET ACLNK10    AT LEVEL 003 AS OF 05/28/09                      
*PHASE T61F10A                                                                  
ACLNK10  TITLE '- ACCOUNTING - P&&L INITIAL DOWNLOAD'                           
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
*                                                                               
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(CODE-SVRDEF)    SERVER ENTRY POINT                           
         DC    AL2(0)              NO FILE LIST                                 
         DC    AL2(0)              NO FACILITIES LIST                           
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
*                                                                               
         ORG   SVRDEF+(RSVRIND1-RSVRDEFD)                                       
         DC    AL1(RSVRILNK)       RUNS UNDER DDLINK CONTROL                    
         ORG                                                                    
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**AL10**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    R5,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
         SPACE 1                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   RUNREQ                                                           
*                                                                               
B#OFFREC EQU   3                   I/O AREA FOR OFFICE RECORDS                  
*                                                                               
         MVC   LP_BLKS+((B#OFFREC-1)*L'LP_BLKS),AIO2                            
*                                                                               
         XC    SVCPYST1(SVCPYLNQ),SVCPYST1                                      
*                                                                               
         MVC   WVALUES(WVALUEL),LVALUES                                         
*                                                                               
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONN                                                        
         L     RF,SRVRRELO                                                      
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         AR    R2,RF                                                            
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
*                                                                               
RUNSTRX  J     EXITY                                                            
*                                                                               
LVALUES  DS    0F                  ** LITERALS MOVED TO W/S **                  
         DC    A(OFFKY1)                                                        
         DC    A(OFFKY1L)                                                       
         DC    A(OFFKY2)                                                        
         DC    A(OFFKY2L)                                                       
         DC    C'2'                                                             
         DC    C'D'                                                             
         DC    X'41',(L'OFFKOFF-1)X'40',(L'OFFKOFF)X'FF'                        
         DC    X'41',(L'ACTKACT-1)X'40',(L'ACTKACT)X'FF'                        
         EJECT                                                                  
***********************************************************************         
* RUN A WORK REQUEST                                                  *         
***********************************************************************         
         SPACE 1                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
*                                                                               
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
         MVC   CPYALPH,LP_AGY      SET COMPANY ID                               
         GOTOR VDATCON,DMCB,(5,0),(2,CPYRDTE)    RUN DATE                       
*                                                                               
         L     R2,ACPYREC                                                       
         USING CPYRECD,R2          R2=A(COMPANY RECORD)                         
         SR    R0,R0                                                            
         LA    R3,CPYRFST                                                       
         USING CPYELD,R3                                                        
RUNREQ10 CLI   CPYEL,0             TEST END OF RECORD                           
         BE    RUNREQ60                                                         
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    RUNREQ30                                                         
*&&US                                                                           
         CLI   CPYEL,NAMELQ        TEST NAME ELEMENT                            
         BE    RUNREQ40                                                         
         CLI   CPYEL,ADRELQ        TEST ADDRESS ELEMENT                         
         BE    RUNREQ50                                                         
*&&                                                                             
RUNREQ20 IC    R0,CPYLN                                                         
         AR    R3,R0                                                            
         B     RUNREQ10                                                         
*                                                                               
RUNREQ30 DS    0H                                                               
         MVC   SVCPYST1,CPYSTAT1   COMPANY STATUS BYTES FOR OFFAL               
         MVC   SVCPYST2,CPYSTAT2                                                
         MVC   SVCPYST3,CPYSTAT3                                                
         MVC   SVCPYST4,CPYSTAT4                                                
         MVC   SVCPYST5,CPYSTAT5                                                
         MVC   SVCPYST6,CPYSTAT6                                                
         MVC   SVCPYST7,CPYSTAT7                                                
         MVC   SVCPYST8,CPYSTAT8                                                
*&&US*&& MVC   CPYSLOG,CPYLOGO                                                  
*                                                                               
         MVI   CPYOFFL,0                                                        
         MVI   CPYINDS,0                                                        
         TM    CPYSTAT1,CPYSOROE                                                
         BZ    *+12                                                             
         MVI   CPYOFFL,1                                                        
         OI    CPYINDS,CPYIOFF1                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+12                                                             
         MVI   CPYOFFL,2                                                        
         OI    CPYINDS,CPYIOFF2                                                 
*                                                                               
         MVC   WORK(L'CPYSFST),CPYSFST                                          
         NI    WORK,X'0F'                                                       
         ICM   R0,1,WORK                                                        
         BNZ   *+12                                                             
         MVI   CPYSFMO,1           DEFAULT IS JANUARY                           
         B     RUNREQ20                                                         
         TM    CPYSFST,X'F0'                                                    
         BO    *+8                                                              
         AHI   R0,9                ADJUST FOR NOVEMBER THROUGH DECEMBER         
         STC   R0,CPYSFMO                                                       
         B     RUNREQ20                                                         
*                                                                               
         USING NAMELD,R3                                                        
RUNREQ40 SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     RUNREQ20                                                         
         MVC   CPYNAME(0),NAMEREC                                               
*                                                                               
         USING ADRELD,R3                                                        
RUNREQ50 LA    RE,CPYADD1          POINT TO 1ST ADDRESS LINE IN STORAGE         
         LA    RF,ADRADD1          POINT TO 1ST ADDRESS LINE IN ELEM            
         SR    R0,R0                                                            
         IC    R0,ADRNUM           NUMBER OF ADDRESS LINES                      
         CHI   R0,4                                                             
         BNH   *+8                                                              
         LA    R0,4                MAX IS 4 LINES                               
         MVC   0(L'ADRADD1,RE),0(RF)                                            
         LA    RE,L'CPYADD1(RE)                                                 
         LA    RF,L'ADRADD1(RF)                                                 
         BCT   R0,*-14                                                          
         B     RUNREQ20                                                         
*                                                                               
         USING CTIREC,R2           R2=A(VALUE RECORD)                           
RUNREQ60 LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LP_USRID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         L     R2,AIO1                                                          
         LA    R1,CTIDATA          LOCATE ORIGIN DETAILS ELEMENT                
*&&UK                                                                           
         USING CTDSTD,R1                                                        
RUNREQ62 CLI   CTDSTEL,0           TEST END OF RECORD                           
         BE    RUNREQ70                                                         
         CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION DETAILS ELEMENT             
         BE    RUNREQ66                                                         
*                                                                               
RUNREQ64 IC    R0,CTDSTLEN                                                      
         AR    R1,R0                                                            
         B     RUNREQ62                                                         
*                                                                               
RUNREQ66 MVC   CPYNAME,CTDSTNAM                                                 
         MVC   CPYADD1,CTDSTADD                                                 
         CLI   CTDSTLEN,166        TEST LONG ELEMENT                            
         BL    RUNREQ70                                                         
         MVC   CPYADD2,CTDSTAD2    YES - GET REST OF ADDRESS/ATTN DETS          
         MVC   CPYADD3,CTDSTAD3                                                 
*&&                                                                             
*&&US                                                                           
         USING CTORGD,R1                                                        
RUNREQ62 CLI   CTORGEL,0           TEST END OF RECORD                           
         BE    RUNREQ70                                                         
         CLI   CTORGEL,CTORGELQ    TEST DESTINATION DETAILS ELEMENT             
         BE    RUNREQ66                                                         
*                                                                               
RUNREQ64 IC    R0,CTORGLEN                                                      
         AR    R1,R0                                                            
         B     RUNREQ62                                                         
*                                                                               
RUNREQ66 MVC   CPYNAME,CTORGNAM                                                 
         MVC   CPYADD1,CTORGADD                                                 
         MVC   CPYADD2(L'CPYADD2+L'CPYADD3),SPACES                              
*&&                                                                             
*                                                                               
RUNREQ70 XC    LIMACC,LIMACC       INIT LIMITED ACCESS FIELD                    
         OC    TWAACCS,TWAACCS     ANY LIMITED ACCESS?                          
         JZ    RUNREQ80                                                         
         MVC   LIMACC,TWAACCS      SAVE OFF LIMITED ACCESS FIELD                
*                                                                               
         LA    R0,OFFTAB           CLEAR OFFICE TABLE                           
         LHI   R1,L'OFFTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING OFFALD,R1                                                        
         LA    R1,OFFBLK           INITIALIZE OFFAL FOR OFFICE ACCESS           
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,CPYALPH                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFACST1(SVCPYLQ1),SVCPYST1   COMPANY STATUS BYTES 1-4           
         MVC   OFFACST5(SVCPYLQ2),SVCPYST5   COMPANY STATUS BYTES 5-8           
         MVI   OFFAACT,OFFAINI                                                  
         OI    OFFACTRL,OFFACCNV   NEW STYLE RECORDS                            
         GOTOR VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T INITIALISE OFFAL                
         DROP  R1                                                               
*                                                                               
         GOTOR INIOFF              INITILAIZE OFFICE BLOCK                      
*                                                                               
RUNREQ80 GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
*                                                                               
RUNREQX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2,R3,RB                                                         
***********************************************************************         
* READ OFFICE RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
NXTOFF   J     *+12                                                             
         DC    C'*NXTOFF*'                                                      
         LR    RB,RF                                                            
         USING NXTOFF,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTOFF20                                                         
*                                                                               
         MVI   LASTOFF,0           INIT LAST OFFICE FIELD                       
         L     R0,AOFFKY2          USE OFFICE RECORD DRIVER                     
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS                           
         BZ    *+8                                                              
         L     R0,AOFFKY2L         USE OFFICE RECORD DRIVER W/LIST              
         LA    RF,FLTOFK                                                        
         TM    CPYINDS,CPYIOFF2                                                 
         BNZ   NXTOFF10                                                         
         SR    RF,RF                                                            
         L     R0,AOFFKY1          USE 2D LEDGER DRIVER                         
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS                           
         BZ    *+8                                                              
         L     R0,AOFFKY1L         USE 2D LEDGER DRIVER W/LIST                  
         TM    CPYINDS,CPYIOFF1                                                 
         BZ    NOMORE                                                           
*                                                                               
NXTOFF10 ST    R0,AOFFKEY          SET A(OFFICE DRIVER TABLE)                   
         ST    RF,AOFKFLT                                                       
*                                                                               
NXTOFF20 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',AOFFKEY),('B#OFFREC',0), *        
               SAVED,AOFKFLT,0                                                  
         JNE   EXITY                                                            
*                                                                               
         LA    R0,OFFVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         XC    OFFVALS(OFFVALL),OFFVALS                                         
         L     R2,IOADDR                                                        
         USING OFFRECD,R2                                                       
         CLI   OFFKTYP,OFFKTYPQ                                                 
         JNE   *+14                                                             
         MVC   OFFCODE,OFFKOFF                                                  
         J     NXTOFF30                                                         
         USING ACTRECD,R2                                                       
         CLC   LASTOFF,ACTKACT                                                  
         JE    NXTOFF20                                                         
         MVC   OFFCODE(1),ACTKACT                                               
         MVC   LASTOFF,ACTKACT                                                  
*                                                                               
NXTOFF30 LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         SR    R0,R0                                                            
NXTOFF40 CLI   NAMEL,0             EXTRACT OFFICE NAME                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     NXTOFF40                                                         
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         J     EXITY                                                            
         MVC   OFFNAME(0),NAMEREC                                               
*                                                                               
FLTOFK   TM    IOKEY+(OFFKSTAT-OFFKEY),OFFSLIST                                 
         BR    RE                                                               
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE TABLE FOR LIMITED ACCESS                          *         
***********************************************************************         
         SPACE 1                                                                
INIOFF   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*INIOFF*'                                                      
*                                                                               
         XC    OFFNUM,OFFNUM                                                    
         USING OFFALD,R1                                                        
         LA    R1,OFFBLK                                                        
         LA    R3,OFFAWORK         OFFICE LIST                                  
         DROP  R1                                                               
*                                                                               
         LA    R0,32               MAX # OF OLD OFFICES (16/PAGE*2)             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   INIOFF10            # OF OFFICES IS NOT INCL IN LIST             
         LH    R1,0(R3)            GET NUMBER OF OFFICES IN LIST                
         STCM  R1,3,OFFNUM                                                      
         SLL   R1,1                                                             
         LA    R3,2(R3)            BUMP TO BEGINNING OF OFFICES                 
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         J     INIOFFX                                                          
         MVC   OFFTAB(0),0(R3)                                                  
*                                                                               
INIOFF10 LA    R2,OFFTAB                                                        
         SR    RE,RE                                                            
INIOFF20 CLI   0(R3),0             END OF FILE FOR 1 BYTE OFFICES               
         JE    INIOFFX                                                          
         CLI   0(R3),C'0'          END OF FILE FOR 1 BYTE OFFICES               
         JE    INIOFFX                                                          
*                                                                               
INIOFF30 MVC   0(1,R2),0(R3)                                                    
         LA    R3,1(R3)            BUMP TO NEXT OFFICE                          
         ICM   RE,3,OFFNUM                                                      
         AHI   RE,1                                                             
         STCM  RE,3,OFFNUM                                                      
         LA    R2,1(R2)                                                         
         BCT   R0,INIOFF20                                                      
*                                                                               
INIOFFX  DS    0H                                                               
         LA    R2,OFFTAB                                                        
         SR    R0,R0                                                            
         ICM   R0,3,OFFNUM                                                      
         LA    R3,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LA    R3,2                                                             
         GOTO1 VXSORT1,DMCB,(0,(R2)),(R0),(R3),(R3),0                           
         GOTO1 VXSORT1,DMCB,(0,(R2)),(R0),(R3),(R3),0                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ONE CHARACTER OFFICE KEY DRIVER TABLE                               *         
***********************************************************************         
         SPACE 1                                                                
OFFKY1   DS    0X                  ** ONE CHARACTER OFFICE DRIVER **            
*                                         ** WITHOUT A LIST **                  
         DC    AL2(L'ACTKEY)                                                    
*                                                                               
         DC    AL1(ACTKCPY-ACTKEY,L'ACTKCPY-1)                                  
         DC    AL2(COMPANY-SAVED)                                               
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(ACTKUNT-ACTKEY,L'ACTKUNT+L'ACTKLDG-1)                        
         DC    AL2(OFFUNT-SAVED)                                                
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(ACTKACT-ACTKEY,L'ACTKACT-1)                                  
         DC    AL2(ACTRANGE-SAVED)                                              
         DC    AL1(LQ_TRNGQ)                                                    
*                                                                               
         DC    AL1(ACTKACT+L'ACTKACT-ACTKEY)                                    
         DC    AL1(L'ACTKEY-(ACTKACT+L'ACTKACT-ACTKEY)-1)                       
         DC    AL1(C' ',0)                                                      
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
OFFKY1X  DC    AL1(LK_EOTQ)                                                     
*                                                                               
OFFKY1L  DS    0X                  ** ONE CHARACTER OFFICE DRIVER **            
*                                           ** WITH A LIST **                   
         DC    AL2(L'ACTKEY)                                                    
*                                                                               
         DC    AL1(ACTKCPY-ACTKEY,L'ACTKCPY-1)                                  
         DC    AL2(COMPANY-SAVED)                                               
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(ACTKUNT-ACTKEY,L'ACTKUNT+L'ACTKLDG-1)                        
         DC    AL2(OFFUNT-SAVED)                                                
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(ACTKACT-ACTKEY,(L'ACTKACT-11)-1)                             
         DC    AL2(OFFLST-SAVED)                                                
         DC    AL1(LQ_TLSTQ)                                                    
*                                                                               
         DC    AL1((ACTKACT+1)-ACTKEY,(L'ACTKACT-1)-1)                          
         DC    AL1(C' ',0)                                                      
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
         DC    AL1(ACTKACT+L'ACTKACT-ACTKEY)                                    
         DC    AL1(L'ACTKEY-(ACTKACT+L'ACTKACT-ACTKEY)-1)                       
         DC    AL1(C' ',0)                                                      
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
OFFKY1LX DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* TWO CHARACTER OFFICE KEY DRIVER TABLE                               *         
***********************************************************************         
         SPACE 1                                                                
OFFKY2   DS    0X                  ** TWO CHARACTER OFFICE DRIVER **            
*                                         ** WITHOUT A LIST **                  
         DC    AL2(L'OFFKEY)                                                    
*                                                                               
         DC    AL1(OFFKTYP-OFFKEY,L'OFFKTYP-1)                                  
         DC    AL1(OFFKTYPQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
         DC    AL1(OFFKCPY-OFFKEY,L'OFFKCPY-1)                                  
         DC    AL2(COMPANY-SAVED)                                               
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(OFFKCPY+L'OFFKCPY-OFFKEY)                                    
         DC    AL1(OFFKOFF-(OFFKCPY+L'OFFKCPY)-1)                               
         DC    AL1(C' ',0)                                                      
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
         DC    AL1(OFFKOFF-OFFKEY,L'OFFKOFF-1)                                  
         DC    AL2(OFFRANGE-SAVED)                                              
         DC    AL1(LQ_TRNGQ)                                                    
*                                                                               
OFFKY2X  DC    AL1(LK_EOTQ)                                                     
*                                                                               
OFFKY2L  DS    0X                  ** TWO CHARACTER OFFICE DRIVER **            
*                                           ** WITH A LIST **                   
         DC    AL2(L'OFFKEY)                                                    
*                                                                               
         DC    AL1(OFFKTYP-OFFKEY,L'OFFKTYP-1)                                  
         DC    AL1(OFFKTYPQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
         DC    AL1(OFFKCPY-OFFKEY,L'OFFKCPY-1)                                  
         DC    AL2(COMPANY-SAVED)                                               
         DC    AL1(LQ_TSINQ)                                                    
*                                                                               
         DC    AL1(OFFKCPY+L'OFFKCPY-OFFKEY)                                    
         DC    AL1(OFFKOFF-(OFFKCPY+L'OFFKCPY)-1)                               
         DC    AL1(C' ',0)                                                      
         DC    AL1(LK_ILITQ)                                                    
*                                                                               
         DC    AL1(OFFKOFF-OFFKEY,L'OFFKOFF-1)                                  
         DC    AL2(OFFLST-SAVED)                                                
         DC    AL1(LQ_TLSTQ)                                                    
*                                                                               
OFFKY2LX DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQUEST  DS    0X                                                               
*                                                                               
REQINI   DS    0XL(LH_LNQ)         ** INITIAL P&L DOWNLOAD **                   
         DC    AL2(REQINIX+1-*)                                                 
         DC    AL2(I#INIDLD)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTINI-SVRDEF)                                               
         DC    XL4'00'                                                          
*                                                                               
REQINIX  DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
         SPACE 1                                                                
OUTINI   DS    0X                  ** INITIAL P&L DOWNLOAD **                   
*                                                                               
         DC    AL2(OUTINIX-*)                                                   
         DC    AL2(1)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
*                                                                               
OUTCPY   DC    AL2(OUTCPYX-*)      ** COMPANY VALUES **                         
         DC    AL2(O#CPYVAL),C'CpyVl'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#RECALP)       COMPANY ALPHA ID                             
         DC    CL5'CpyId'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYALPH-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYALPH)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#RECNAM)       COMPANY NAME                                 
         DC    CL5'CpyNm'                                                       
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(CPYNAME-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYNAME)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#CPYSFM)       COMPANY START FINANCIAL MONTH                
         DC    CL5'CpyFM'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYSFMO-SAVED)                                               
         DC    AL1(LD_LBINQ,L'CPYSFMO)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ADD1)         COMPANY ADDRESS LINE 1                       
         DC    CL5'CpyA1'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYADD1-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYADD1)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ADD2)         COMPANY ADDRESS LINE 2                       
         DC    CL5'CpyA2'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYADD2-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYADD2)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ADD3)         COMPANY ADDRESS LINE 3                       
         DC    CL5'CpyA3'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYADD3-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYADD3)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ADD4)         COMPANY ADDRESS LINE 4                       
         DC    CL5'CpyA4'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYADD4-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYADD4)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#RUNDTE)       RUN DATE                                     
         DC    CL5'RunDt'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYRDTE-SAVED)                                               
         DC    AL1(LD_CDATQ,L'CPYRDTE)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#OFFLEN)       OFFICE LENGTH                                
         DC    CL5'OffLn'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYOFFL-SAVED)                                               
         DC    AL1(LD_LBINQ,L'CPYOFFL)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(10)             Sign in ID                                   
         DC    CL5'Cpylg'                                                       
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPYSLOG-SAVED)                                               
         DC    AL1(LD_CHARQ,L'CPYSLOG)                                          
         DC    XL4'00'                                                          
*                                                                               
OUTCPYX  DS    0X                                                               
*                                                                               
OUTOFF   DC    AL2(OUTOFFX-*)      ** OFFICE VALUES **                          
         DC    AL2(O#OFFVAL),C'OffVl'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(O#OFFVAL)                                                    
         DC    C'Array'                                                         
         DC    AL1(LO_IARRQ,0,LO_IXNUQ)                                         
         DC    AL2(ARYOFF-SVRDEF)                                               
         DC    AL1(0,0)                                                         
         DC    XL4'00'                                                          
OUTOFFX  DS    0X                                                               
*                                                                               
OUTINIX  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR OFFICE VALUES                                  *         
***********************************************************************         
         SPACE 1                                                                
ARYOFF   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTOFF-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(OFFCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
OFFCOL   DS    0X                                                               
*                                                                               
         DC    AL2(D#RECCOD),C'OffCd'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OFFCODE-OFFVALS),AL1(L'OFFCODE)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#RECNAM),C'OffNm'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OFFNAME-OFFVALS),AL1(L'OFFNAME)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
OFFCOLN  EQU   (*-OFFCOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
*                                                                               
WVALUES  DS    0X                  ** LITERAL VALUES **                         
*                                                                               
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AOFFKY1  DS    A                   A(OFFICE DRIVER TABLE 1)                     
AOFFKY1L DS    A                   A(OFFICE DRIVER TABLE 1 W/LIST)              
AOFFKY2  DS    A                   A(OFFICE DRIVER TABLE 2)                     
AOFFKY2L DS    A                   A(OFFICE DRIVER TABLE 2 W/LIST)              
ADCONN   EQU   (*-ADCONS)/L'ADCONS                                              
*                                                                               
OFFUNT   DS    C                   OFFICE UNIT CODE                             
OFFLDG   DS    C                   OFFICE LEDGER CODE                           
OFFRANGE DS    XL(L'OFFKOFF*2)     OFFICE RANGE                                 
ACTRANGE DS    XL(L'ACTKACT*2)     ACCOUNT RANGE                                
*                                                                               
WVALUEL  EQU   *-WVALUES                                                        
*                                                                               
* OUTPUT VALUES                                                                 
*                                                                               
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
CPYVALS  DS    0X                  ** COMPANY VALUES **                         
CPYALPH  DS    CL(L'LP_AGY)        COMPANY ALPHA ID                             
CPYNAME  DS    CL(L'CTDSTNAM)      COMPANY NAME                                 
CPYSFMO  DS    X                   COMPANY START FINANCIAL MONTH                
CPYADD1  DS    CL(L'CTDSTADD)      COMPANY ADDRESS LINE 1                       
CPYADD2  DS    CL(L'CTDSTADD)      COMPANY ADDRESS LINE 2                       
CPYADD3  DS    CL(L'CTDSTADD)      COMPANY ADDRESS LINE 3                       
CPYADD4  DS    CL(L'CTDSTADD)      COMPANY ADDRESS LINE 4                       
CPYRDTE  DS    XL2                 COMPRESSED RUN DATE                          
CPYOFFL  DS    XL1                 OFFICE LENGTH                                
CPYSLOG  DS    CL7                 COMPANY SIGNIN ID (LOGO)                     
CPYVALL  EQU   *-CPYVALS                                                        
*                                                                               
OFFVALS  DS    0X                  ** OFFICE VALUES **                          
OFFCODE  DS    CL2                 OFFICE CODE                                  
OFFNAME  DS    CL(L'NAMEREC)       OFFICE NAME                                  
OFFVALL  EQU   *-OFFVALS                                                        
OUTVALL  EQU   *-OUTVALS                                                        
*                                                                               
* GENERAL STORAGE                                                               
*                                                                               
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
*                                                                               
CPYINDS  DS    X                   ** COMPANY INDICATORS **                     
CPYIOFF1 EQU   X'80'               ONE CHARACTER OFFICES                        
CPYIOFF2 EQU   X'40'               TWO CHARACTER OFFICES                        
*                                                                               
LIMACC   DS    CL2                 LIMITED ACCESS                               
*                                                                               
AOFFKEY  DS    A                   A(OFFICE DRIVER TABLE)                       
AOFKFLT  DS    A                   A(OFFICE KEY FILTER ROUTINE)                 
*                                                                               
SVCPYST1 DS    XL1                 SAVED ARE FOR COMPANY STATUS BYTE 1          
SVCPYST2 DS    XL1                               COMPANY STATUS BYTE 2          
SVCPYST3 DS    XL1                               COMPANY STATUS BYTE 3          
SVCPYST4 DS    XL1                               COMPANY STATUS BYTE 4          
SVCPYLQ1 EQU   *-SVCPYST1                                                       
SVCPYST5 DS    XL1                 SAVED ARE FOR COMPANY STATUS BYTE 5          
SVCPYST6 DS    XL1                               COMPANY STATUS BYTE 6          
SVCPYST7 DS    XL1                               COMPANY STATUS BYTE 7          
SVCPYST8 DS    XL1                               COMPANY STATUS BYTE 8          
SVCPYLQ2 EQU   *-SVCPYST5                                                       
SVCPYLNQ EQU   *-SVCPYST1                                                       
*                                                                               
LASTOFF  DS    CL1                 PREVIOUS OFFICE FOR 1 BYTE OFFICES           
*                                                                               
OFFLST   DS    0C                                                               
OFFNUM   DS    XL2                                                              
OFFTAB   DS    CL240               LIST OF VALID OFFICE                         
         DC    X'FF'               EOT                                          
*                                                                               
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACLNK10   05/28/09'                                      
         END                                                                    
