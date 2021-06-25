*          DATA SET ACCLB42B   AT LEVEL 159 AS OF 12/23/99                      
*PHASE T62142B                                                                  
CLB42    TITLE '- BILL PROGRAM OPTION ROUTINES'                                 
CLB42    CSECT                                                                  
         PRINT NOGEN                                                            
ROUT     NMOD1 0,**CB42**,RR=R8                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
*                                                                               
         SRL   RF,24               RF = A(ROUTINE, SIZE OF LOCAL W/S)           
         SLL   RF,3                                                             
         LA    RF,AROUT(RF)                                                     
         A     R8,0(RF)            R8 = A(ROUTINE)                              
*                                                                               
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZ    ROUT02                                                           
         BCTR  R3,0                                                             
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         LA    R3,1(R3)                                                         
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
ROUT02   DS    0H                  BRANCH TO ROUTINE                            
         BR    R8                                                               
*                                                                               
AROUT    DS    0A                  ROUTINES / LOCAL W/S SIZE                    
         DC    A(SETTRN,STWORKL)                                                
         DC    A(VALOPT,VOWORKL)                                                
         DC    A(STROPT,SOWORKL)                                                
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB42+X'0180'                                                    
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER TRANSACTION DIRECTORY RECORD                      *         
* NTRY: P1 BYTE 0 = C'D'                                              *         
*             1-3 = A(ACCDIR RECORD)                                  *         
* EXIT: TSAR RECORD SET UP FROM DIRECTORY RECORD                      *         
*                                                                     *         
* ROUTINE TO FILTER TRANSACTION MASTER RECORD                         *         
* NTRY: P1 BYTE 0 = C'M'                                              *         
*             1-3 = A(ACCMST RECORD)                                  *         
* EXIT: TSAR RECORD SET UP FROM MASTER RECORD                         *         
*                                                                     *         
* ROUTINE TO SET UP TSAR RECORD FOR TRANSACTION                       *         
* NTRY: P1 BYTE 0 = C'S'                                              *         
*             1-3 = A(ACCDIR RECORD)                                  *         
*       P2        = A(ACCMST RECORD)                                  *         
*       P3        = A(PRO-BLOCK)                                      *         
*                                                                     *         
* ROUTINE TO REFRESH TSAR RECORD                                      *         
* NTRY: P1 BYTE 0 = C'R'                                              *         
*             1-3 = A(ACCMST RECORD)                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING STWORKD,RC                                                       
SETTRN   DS    0H                                                               
         USING *,R8                                                             
         MVC   STPARMS,0(R1)                                                    
         XR    R2,R2                                                            
         ICM   R2,7,STPATRN                                                     
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         LA    R4,LSPRATA                                                       
         USING PRORATAD,R4                                                      
         CLI   STPACT,STPADIR      TEST DIRECTORY FILTER                        
         BE    FLTDIR                                                           
         CLI   STPACT,STPAMST      TEST MASTER FILTER                           
         BE    FLTMST                                                           
         CLI   STPACT,STPAREF      TEST REFRESH FILTER                          
         BNE   *+12                                                             
         BAS   RE,REFMST                                                        
         B     EXITY                                                            
         CLI   STPACT,STPASET      TEST SET UP TLST                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,STPAPROR         GET A(USERS PRO-RATA BLOCK)                  
         XC    TLNUM,TLNUM                                                      
         BAS   RE,SETDIR                                                        
         L     R2,STPATRN2                                                      
         BAS   RE,SETMST                                                        
         BAS   RE,REFMST                                                        
         B     EXITY                                                            
*                                                                               
FLTDIR   DS    0H                  ** DIRECTORY RECORD **                       
         CLC   TRNKWORK,TRNBILL    IGNORE BILLS                                 
         BE    EXITL                                                            
         TM    CUSTAT,CUSDDS       DDS REVERSALS DEALT WITH BELOW               
         BO    *+12                                                             
         TM    TRNKSTAT,TRNSREVS IGNORE REVERSALS                               
         BO    EXITL                                                            
         TM    TRNKSTA2,TRNSEXCL   TEST EXCLUDED FROM REPORTS                   
         BO    EXITL                                                            
         TM    MIXLIND1,MIXLIDRA   TEST INCLUDING DRAFTS                        
         BO    FLTD001                                                          
         CLI   TRNKSTYP,99         IGNORE NON-99 DRAFTS                         
         BE    FLTD001                                                          
         TM    TRNKSTAT,TRNSDRFT                                                
         BO    EXITL                                                            
FLTD001  TM    LSSTAT1,LSSORDER    TEST INCLUDING ORDERS                        
         BO    *+14                                                             
         CLC   TRNKWORK,TRNORDER   NO - IGNORE THEM                             
         BE    EXITL                                                            
*                                                                               
         BAS   RE,SETDIR                                                        
*                                  WORKCODE                                     
FLTD002  OC    LSWC,LSWC                                                        
         BZ    FLTD010                                                          
         LA    RF,X'80'+LSWCODEN                                                
         GOTO1 STRCMP,STPARM,(L'TRNKWORK,TRNKWORK),((RF),LSWC)                  
         BNE   EXITL                                                            
*                                  INPUT TYPE                                   
FLTD010  OC    LSTYP,LSTYP                                                      
         BZ    FLTD020                                                          
         LA    RF,X'80'+L'LSTYPE                                                
         GOTO1 STRCMP,STPARM,(L'TRNKSTYP,TRNKSTYP),((RF),LSTYP)                 
         BNE   EXITL                                                            
*                                  TRANSACTION REFERENCE                        
FLTD020  OC    LSREF,LSREF                                                      
         BZ    FLTD030                                                          
         SR    RF,RF                                                            
         ICM   RF,1,LSREFSTL       RF=L'START CODE                              
         BZ    FLTD022                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),LSREFST                                               
         BL    FLTD026             OUTSIDE SPECIFIED RANGE                      
*                                                                               
FLTD022  ICM   RF,1,LSREFENL       RF=L'END CODE                                
         BZ    FLTD024                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),LSREFEN                                               
         BH    FLTD026             OUTSIDE SPECIFIED RANGE                      
*                                                                               
FLTD024  TM    LSREFFI,OPTNEQ      INSIDE RANGE                                 
         BO    EXITL                                                            
         B     FLTD030                                                          
*                                                                               
FLTD026  TM    LSREFFI,OPTNEQ      OUTSIDE RANGE                                
         BO    FLTD030                                                          
         B     EXITL                                                            
*                                  TRANSATION DATE                              
FLTD030  OC    LSDATE,LSDATE                                                    
         BZ    FLTD040                                                          
         CLC   TRNKDATE,LSDATST                                                 
         BL    FLTD032             OUTSIDE OF RANGE                             
         CLC   TRNKDATE,LSDATEN                                                 
         BH    FLTD032             OUTSIDE OF RANGE                             
         TM    LSDATFI,OPTNEQ                                                   
         BO    EXITL                                                            
         B     FLTD040                                                          
FLTD032  TM    LSDATFI,OPTNEQ                                                   
         BNO   EXITL                                                            
*                                  MONTH OF ACTIVITY                            
FLTD040  OC    LSMOA,LSMOA                                                      
         BZ    FLTD050                                                          
         CLC   TRNKSMOS,LSMOAST                                                 
         BL    FLTD042             OUTSIDE OF RANGE                             
         CLC   TRNKSMOS,LSMOAEN                                                 
         BH    FLTD042             OUTSIDE OF RANGE                             
         TM    LSMOAFI,OPTNEQ                                                   
         BO    EXITL                                                            
         B     FLTD050                                                          
FLTD042  TM    LSMOAFI,OPTNEQ                                                   
         BNO   EXITL                                                            
*                                                                               
FLTD050  DS    0H                                                               
         TM    CUSTAT,CUSDDS                                                    
         BZ    FLTD060                                                          
         OC    LSREVERS,LSREVERS                                                
         BZ    FLTD060                                                          
         CLI   LSREVERS,C'Y'       TAKE ALL                                     
         BE    FLTD060                                                          
         LA    RE,BOQ              BO - REVERSAL=NO                             
         CLI   LSREVERS,C'N'                                                    
         BE    *+8                                                              
         LA    RE,BNOQ             BNO - REVERSAL=ONLY                          
         TM    TRNKSTAT,TRNSREVS                                                
         EX    RE,*+4                                                           
         NOP   EXITL                                                            
*                                                                               
FLTD060  OC    LSULC,LSULC                                                      
         BZ    FLTD070                                                          
         LA    RE,BNEQ             B NOT EQUAL                                  
         TM    LSULCFI,OPTNEQ                                                   
         BNO   *+8                                                              
         LA    RE,BEQ              B EQUAL                                      
         IC    RF,LSULCLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),LSULCACC                                              
         EX    RE,*+4                                                           
         NOP   EXITL                                                            
*                                                                               
FLTD070  DS    0H                                                               
*                                                                               
FLTD500  BAS   RE,SETDIR           * SET VALUES IN TSAR LIST RECORD *           
*                                                                               
FLTDIRY  B     EXITY                                                            
         SPACE 1                                                                
FLTMST   DS    0H                  ** ACCMST RECORD **                          
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TRNKACT,LSGOJOB     MATCH ON LAST GETOPT JOB                     
         BNE   FLTM006                                                          
         CLC   TRNKWORK,LSGOWC     MATCH ON LAST GETOPT W/C                     
         BE    FLTM008                                                          
         CLC   TRNKWORK,TRNORDER   TEST AN ORDER                                
         BNE   FLTM006                                                          
         LA    R1,TRNELD           YES - MATCH ON ORDER WORK-CODE               
         USING OAMELD,R1                                                        
         XR    RF,RF                                                            
FLTM002  CLI   OAMEL,OAMELQ                                                     
         BE    FLTM004                                                          
         CLI   OAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,OAMLN                                                         
         BXH   R1,RF,FLTM002                                                    
FLTM004  CLC   LSGOWC,OAMWORK                                                   
         BE    FLTM008                                                          
         DROP  R1                                                               
FLTM006  GOTO1 AGETOPT,STPARM,(X'80',TRNRECD)                                   
         MVC   LSGOJOB,TRNKACT                                                  
         L     RF,AGOPBLK                                                       
         MVC   LSGOWC,GOSELWC-GOBLOCK(RF)                                       
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FLTM008  XR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    FLTM009             BILLING IN AGENCY CURRENCY                   
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    FLTM009             BILLING IN SECOND CURRENCY                   
         LA    R0,CSEXCVAL                                                      
FLTM009  GOTO1 APRORATA,STPARM,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         TM    MIXLIND1,MIXLICUR   TEST MATCH ON BILLING CURRENCY               
         BZ    FLTM010                                                          
         CLC   PG$BLCUR,BCSPACES   TEST ANY PRIOR ACTIVITY                      
         BNH   FLTM010                                                          
         CLC   CSBILCUR,PG$BLCUR   TEST CURRENCY OF PRIOR ACTIVITY              
         BE    FLTM010             MATCHES - OK                                 
         CP    PA$NETBL,BCPZERO    IF PRIOR BILLING COMES TO ZERO OK            
         BNE   EXITL                                                            
         CP    PA$COMBL,BCPZERO                                                 
         BNE   EXITL                                                            
*                                                                               
FLTM010  LA    R1,TRNELD                                                        
         USING TRXELD,R1                                                        
         XR    RF,RF                                                            
FLTM012  CLI   TRXEL,0                                                          
         BE    FLTM020                                                          
         CLI   TRXEL,TRXELQ                                                     
         BE    FLTM016                                                          
         CLI   TRXEL,TRSELQ                                                     
         BE    FLTM018                                                          
*                                                                               
FLTM014  IC    RF,TRXLN                                                         
         BXH   R1,RF,FLTM012                                                    
*                                                                               
FLTM016  TM    TRXSTA1,TRXSXALC    TEST EXCLUDE FROM BILL ALLOCATION            
         BO    EXITL                                                            
         B     FLTM014                                                          
*                                                                               
         USING TRSELD,R1                                                        
FLTM018  OC    LSACTDAT,LSACTDAT   TEST ACTIVITY DATE FILTER SET                
         BZ    FLTM014                                                          
         CLC   TRSDATE,LSACTDST    TEST BEFORE START DATE                       
         BL    FLTM019                                                          
         CLC   TRSDATE,LSACTDND    TEST AFTER END DATE                          
         BH    FLTM019                                                          
         TM    LSACTFI,OPTNEQ      IN RANGE - NEGATIVE FILTER OFF?              
         BZ    FLTM014                                                          
         B     EXITL                                                            
FLTM019  TM    LSACTFI,OPTNEQ      OUT OF RANGE - NEGATIVE FILTER ON?           
         BO    FLTM014                                                          
         B     EXITL                                                            
         DROP  R1                                                               
*                                  BILLED=Y/N/O                                 
FLTM020  CLI   CSACT,ACTREV        NOT VALID FOR ACTION REVERSE                 
         BE    FLTM030                                                          
         CLI   LSBILD,0            TEST NOT SET                                 
         BE    FLTM030                                                          
         CLI   LSBILD,C'Y'         TEST TAKE ALL BILLED/UNBILLED                
         BE    FLTM030                                                          
         LA    RE,BEQ              BE - EXCLUDE FULLY BILLED                    
         CLI   LSBILD,C'N'                                                      
         BE    *+8                                                              
         LA    RE,BNEQ             BNE - FULLY BILLED ONLY                      
         ZAP   STDUB1,PA$NETUB     TEST UNBILLED AMOUNT                         
         CLI   TRNTYPE,99          UNLESS ADVANCE BILLING ITEM                  
         BNE   FLTM022                                                          
         ZAP   STDUB1,PP$AALLO     IF PENDING AMOUNT THEN IS UNBILLED           
         AP    STDUB1,PP$ACOMM                                                  
         BNZ   FLTM022                                                          
         CP    PA$NETBL,BCPZERO    IF BILLED=0 THEN IS UNBILLED                 
         BNE   FLTM022                                                          
         CP    PA$COMBL,BCPZERO                                                 
         BNE   FLTM022                                                          
         ZAP   STDUB1,=P'1'        SET DUMMY AMOUNT FOR BILLED                  
*                                                                               
FLTM022  CP    STDUB1,BCPZERO                                                   
         EX    RE,*+4                                                           
         NOP   EXITL                                                            
*                                                                               
FLTM030  DS    0H                                                               
*&&UK                                                                           
*                                  AUTH STATUS                                  
         CLI   LSAUTH,0                                                         
         BE    FLTM040                                                          
         CLI   LSAUTH,C'Y'         TAKE AUTH AND UNAUTH                         
         BE    FLTM040                                                          
         CLI   LSAUTH,C'O'         TAKE AUTHD TRANSACTIONS ONLY                 
         BNE   *+16                                                             
         TM    TRNSTAT,TRNSAUTH                                                 
         BO    FLTM040                                                          
         B     EXITL                                                            
         TM    TRNSTAT,TRNSAUTH                                                 
         BNO   FLTM040                                                          
         B     EXITL                                                            
*                                  HELD STATUS                                  
FLTM040  CLI   LSHELD,0                                                         
         BE    FLTM050                                                          
         CLI   LSHELD,C'Y'         TAKE HELD AND UNHELD                         
         BE    FLTM050                                                          
         CLI   LSHELD,C'O'         TAKE HELD TRANSACTIONS ONLY                  
         BNE   *+16                                                             
         TM    TRNSTAT,TRNSHOLD                                                 
         BO    FLTM050                                                          
         B     EXITL                                                            
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   FLTM050                                                          
         B     EXITL                                                            
*&&                                                                             
*                                  BATCH REFERENCE                              
FLTM050  OC    LSBAT,LSBAT                                                      
         BZ    FLTM060                                                          
         SR    RF,RF                                                            
         IC    RF,LSBATSTL         RF=L'START CODE                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNBREF(0),LSBATST                                               
         BL    FLTM052             OUTSIDE SPECIFIED RANGE                      
         IC    RF,LSBATENL         RF=L'END CODE                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNBREF(0),LSBATEN                                               
         BH    FLTM052             OUTSIDE SPECIFIED RANGE                      
*                                                                               
         TM    LSBATFI,OPTNEQ      INSIDE RANGE                                 
         BO    EXITL                                                            
         B     FLTM060                                                          
*                                                                               
FLTM052  TM    LSBATFI,OPTNEQ      OUTSIDE RANGE                                
         BO    FLTM060                                                          
         B     EXITL                                                            
*                                  AMOUNT FILTER                                
FLTM060  OC    LSAMT,LSAMT                                                      
         BZ    FLTM070             SET RE WITH BRANCH MASK TO INCLUDE           
         LA    RE,BLQ              BL                                           
         TM    LSAMTFI,OPTLEQ                                                   
         BO    FLTM062                                                          
         LA    RE,BHQ              BH                                           
         TM    LSAMTFI,OPTGEQ                                                   
         BO    FLTM062                                                          
         LA    RE,BNEQ             BNE                                          
         TM    LSAMTFI,OPTNEQ                                                   
         BO    FLTM062                                                          
         LA    RE,BEQ              BE - THE DEFAULT                             
FLTM062  CP    TRNAMNT,LSAMOUNT                                                 
         EX    RE,*+4                                                           
         NOP   FLTM070                                                          
         B     EXITL                                                            
*                                  ALLOC=Y/N/O/P                                
FLTM070  OC    LSALLOC,LSALLOC                                                  
         BZ    FLTM080                                                          
         CLI   LSALLOC,C'Y'                                                     
         BE    FLTM080             TAKE ALL                                     
*                                                                               
         CLI   LSALLOC,C'P'        TEST PARTIAL ALLOCATIONS ONLY                
         BNE   FLTM072                                                          
         CP    PP$AALLO,BCPZERO    DON'T WANT ZERO ALLOCATIONS                  
         BE    EXITL                                                            
         CP    PM$ANVBL,BCPZERO    OR FULL ALLOCATIONS                          
         BE    EXITL                                                            
         B     FLTM080                                                          
*                                                                               
FLTM072  LA    RE,BNZQ             BNZ                                          
         CLI   LSALLOC,C'N'                                                     
         BE    *+8                                                              
         LA    RE,BZQ              BZ                                           
         CP    PP$AALLO,BCPZERO                                                 
         EX    RE,*+4                                                           
         NOP   EXITL                                                            
*                                  UNAVAILABLE=Y/N/O                            
FLTM080  CLI   LSUNAV,0                                                         
         BE    FLTM090                                                          
         CLI   LSUNAV,C'Y'         INCLUDE ALL                                  
         BE    FLTM090                                                          
*                                                                               
         CLI   CSACT,ACTALC                                                     
         BNE   *+12                                                             
         LA    RF,PP$AALLO         RF=A(ALLOCATION PENDING)                     
         B     FLTM082                                                          
         CLI   CSACT,ACTWOF                                                     
         BNE   *+12                                                             
         LA    RF,PP$AWOFF         RF=A(WRITE-OFFS PENDING)                     
         B     FLTM082                                                          
         CLI   CSACT,ACTXFR                                                     
         BNE   *+12                                                             
         LA    RF,PP$AXFER         RF=A(TRANSFERS PENDING)                      
         B     FLTM082                                                          
         B     FLTM090                                                          
*                                                                               
FLTM082  XR    RE,RE               RE=0 IF AMOUNT PENDING/AVAIABLE              
         CP    PM$ANVBL,BCPZERO    TEST ANYTHING AVAILABLE                      
         BNE   FLTM086                                                          
         CLI   TRNTYPE,99          TEST ALLOCATING AN ADVANCE                   
         BNE   FLTM084                                                          
         CLI   CSACT,ACTALC                                                     
         BNE   FLTM084                                                          
         CLC   PG$LBLNO,BCSPACES   YES - AVAILABLE IF NOTHING BILLED            
         BNH   FLTM086                                                          
         B     FLTM085                                                          
FLTM084  CP    0(L'PM$ANVBL,RF),BCPZERO                                         
         BNE   FLTM086                                                          
FLTM085  BCTR  RE,0                RE=-1 IF UNAVAILABLE FOR PENDING             
*                                                                               
FLTM086  CLI   LSUNAV,C'N'         EXCLUDE UNAVAILABLES                         
         BNE   FLTM088                                                          
         LTR   RE,RE                                                            
         BNZ   EXITL                                                            
         B     FLTM090                                                          
FLTM088  LTR   RE,RE               EXCLUDE AVAILABLES                           
         BZ    EXITL                                                            
*                                                                               
FLTM090  DS    0H                                                               
*&&US                              WCTYPE = O/P/R/T                             
         OC    LSWCTYPE,LSWCTYPE                                                
         BZ    FLTM100                                                          
         L     RF,AGOPBLK                                                       
         USING GOBLOCKD,RF                                                      
         LA    R0,X'80'+L'LSWCTCOD                                              
         GOTO1 STRCMP,STPARM,(L'GOWRKTY,GOWRKTY),((R0),LSWCTYPE)                
         BNE   EXITL                                                            
         DROP  RF                                                               
*&&                                                                             
FLTM100  DS    0H                                                               
*                                                                               
FLTM500  BAS   RE,SETMST           * SET VALUES IN TSAR LIST RECORD *           
         BAS   RE,REFMST                                                        
*                                                                               
         OC    LSLABOR,LSLABOR     LABOR=Y/N/O FILTER                           
         BZ    FLTM510                                                          
         CLI   LSLABOR,C'Y'        TAKE ALL                                     
         BE    FLTM510                                                          
         CLI   LSLABOR,C'O'        TEST ONLY                                    
         BNE   FLTM502                                                          
         TM    TLXSTAT,TLXSHOUR                                                 
         BZ    EXITL                                                            
         B     FLTM510                                                          
FLTM502  TM    TLXSTAT,TLXSHOUR    EXCLUDE LABOR                                
         BO    EXITL                                                            
*                                                                               
FLTM510  DS    0H                                                               
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SET TLSTD VALUES FROM DIRECTORY RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
SETDIR   NTR1  ,                                                                
         XC    TLREC(TLXOVR-TLREC),TLREC                                        
         MVC   TLDA,TRNKDA                                                      
         MVC   TLSTA,TRNKSTA                                                    
*                                                                               
         CLC   TRNKWORK,TRNORDER TEST AN ORDER                                  
         BNE   *+8                                                              
         OI    TLXSTAT,TLXSORD                                                  
*                                                                               
         CLI   TRNKSTYP,169        TEST FEE ADJUSTMENT                          
         BNE   *+8                                                              
         OI    TLXSTAT,TLXSFEEA                                                 
*                                                                               
         TM    TLXSTAT,TLXSORD     TEST AN ORDER                                
         BO    SDIR08                                                           
         CLI   TRNKSTYP,47         TEST AN EP                                   
         BE    SDIR08                                                           
         CLI   TRNKSTYP,48                                                      
         BE    SDIR02                                                           
         CLI   TRNKSTYP,99         TEST EXTRA BILLING ITEM                      
         BE    SDIR08              NO - CAN WRITE-OFF/TRANSFER/RECOVER          
         OI    TLXVAL,TLXVWOF+TLXVRCV                                           
SDIR02   OI    TLXVAL,TLXVXFR      CAN TRANSFER TYPE 48                         
SDIR08   L     R3,AGOPBLK                                                       
         USING GOBLOCKD,R3                                                      
         CLI   GOBILTYP,C'C'       TEST BILLING TYPE IS CLIENT                  
         BNE   SDIR10                                                           
         OI    TLXVAL,TLXVALL      EVERYTHING VALID TO ALLOCATE                 
*                                                                               
SDIR10   DS    0H                                                               
*                                                                               
SETDIRX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* SET TLSTD VALUES FROM MASTER RECORD AND GOBLK                       *         
***********************************************************************         
         SPACE 1                                                                
SETMST   NTR1  ,                                                                
         L     R3,AGOPBLK                                                       
         USING GOBLOCKD,R3                                                      
*                                                                               
         TM    TRNSTAT,TRNSNOCM  SET COMMISSIONABLE STATUS                      
         BZ    *+8                                                              
         OI    TLXSTAT,TLXSNOCM                                                 
*&&UK                                                                           
         XR    RF,RF                                                            
         CLI   STPACT,STPASET                                                   
         BE    *+8                                                              
         LA    RF,X'80'                                                         
         GOTO1 AGETWCD,STPARM,((RF),TRNKWORK)                                   
         BNL   SMST01                                                           
         CLI   CSACT,ACTDWN                                                     
         BE    SMST04                                                           
         MVC   FVXTRA(L'TRNKWORK),TRNKWORK   WORKCODE NOT ON FILE               
         MVC   FVMSGNO,=AL2(AE$MISWC)                                           
         OI    BCINDS1,BCIXERR                                                  
         GOTO1 AXITSES             RESTORE LAST ACTION                          
         L     RD,BCSVRD           EXIT TO ROOT                                 
         L     RD,8(RD)                                                         
         B     EXIT                                                             
SMST01   L     R1,0(R1)                                                         
         USING WCDATA,R1                                                        
         USING WCOELD,WCEL                                                      
         TM    WCOSTAT,WCOSHCOE    TEST WC STATUS FOR HOURLY                    
         BZ    SMST04                                                           
         DROP  R1                                                               
*&&                                                                             
*&&US                                                                           
         CLC   GOTWO,BCSPACES      TEST ANY GOTWO SETTING                       
         BH    *+12                YES                                          
         CLI   GOWRKTY,C'T'        NO - DEFAULT TIME TYPE IS T                  
         BE    SMST02                                                           
         LA    RF,GOTWO            TEST W/C TYPE IN LIST OF TIME TYPES          
         LA    R0,L'GOTWO                                                       
         CLC   GOWRKTY,0(RF)                                                    
         BE    SMST02                                                           
         LA    RF,L'GOWRKTY(RF)                                                 
         BCT   R0,*-14                                                          
         CLI   P#SKLABR,C'Y'       PROFILE SK CONTRA LABOR TOO                  
         BNE   SMST04                                                           
         CLC   =C'SK',TRNKULC                                                   
         BNE   SMST04                                                           
*&&                                                                             
SMST02   OI    TLXSTAT,TLXSHOUR                                                 
         B     *+8                                                              
SMST04   OI    TLXSTAT,TLXSCOST                                                 
*                                                                               
*&&UK*&& OI    TLXSTAT,TLXSULSE    UK - DEFAULT LEDGER IS ALWAYS SE             
*&&US                                                                           
*        TM    TLXSTAT,TLXSHOUR    TEST HOURLY ITEM                             
*        BO    SMST06                                                           
         CLC   INCUL,TRNKULC       TEST INCOME LEDGER                           
         BE    SMST06                                                           
         CLC   UL1R,TRNKULC        TEST TMS POSTING                             
         BE    SMST06                                                           
         CLC   SKUL,TRNKULC        TEST SK LEDGER                               
         BE    SMST06                                                           
         OI    TLXSTAT,TLXSULSE    NO - DEFAULT LEDGER IS SE                    
         B     *+8                                                              
SMST06   OI    TLXSTAT,TLXSULSI    YES - DEFAULT LEDGER IS SI                   
*&&                                                                             
*                                                                               
         MVC   TLKWC,TRNKWORK      SET WORK-CODE IN KEY                         
         MVC   TLKULC,TRNKULC      SET CONTRA-ACCOUNT                           
         MVC   TLKDATE,TRNKDATE    SET DATE                                     
         MVC   TLKREF,TRNKREF      SET REFERENCE NUMBER                         
         MVC   TLKSBR,TRNKSBR      SET SUB-REFERENCE NUMBER                     
*&&UK                                                                           
         TM    TLXSTAT,TLXSORD     TEST ORDER                                   
         BZ    SMST20                                                           
         LA    R1,TRNELD           YES - SET TO ORDER WORK-CODE                 
         USING OAMELD,R1                                                        
         XR    RF,RF                                                            
SMST12   CLI   OAMEL,OAMELQ                                                     
         BE    SMST14                                                           
         CLI   OAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,OAMLN                                                         
         BXH   R1,RF,SMST12                                                     
SMST14   MVC   TLKWC,OAMWORK                                                    
         DROP  R1                                                               
*                                                                               
SMST20   CLI   P#DISORD,C'M'       TEST ORDERS BEING MERGED                     
         BE    SMST30                                                           
         TM    TLXSTAT,TLXSORD     NO - ORDERS GO FIRST                         
         BO    *+8                                                              
         MVI   TLKSTYP1,TLKSORDN                                                
         B     SMST30                                                           
*&&                                                                             
*&&US                                                                           
         TM    LSSTAT1,LSSSEPTY    TEST SEPERATING BY W/C TYPE                  
         BZ    SMST30                                                           
         MVI   TLKSTYP1,TLKSTIME   SET TIME                                     
         TM    TLXSTAT,TLXSCOST                                                 
         BZ    *+8                                                              
         MVI   TLKSTYP1,TLKSOOPS   SET AN OUT-OF-POCKET EXPENSE                 
*&&                                                                             
SMST30   CP    PA$HOURS,BCPZERO    TEST VALID FOR HOURS TO BE PENDING           
         BE    *+8                                                              
         OI    TLXVAL,TLXVHOUR                                                  
*                                                                               
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    SMST40                                                           
         CLI   P#FREVFI,C'Y' '     TEST FORCE REVAL OF FOREIGN INVOICE          
         BNE   SMST40                                                           
         LA    R3,TRNRFST                                                       
         USING AFCELD,R3           FIND AFCELD                                  
         XR    RF,RF                                                            
SMST32   CLI   AFCEL,0                                                          
         BE    SMST40                                                           
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,SMST32                                                     
         CLC   AFCCURR,CSBILCUR    TEST SAME CURRENCY                           
         BNE   SMST40                                                           
         CLC   AFCXRATE,CSEXCRAT   TEST SAME EXCHANGE RATE                      
         BE    SMST40                                                           
         OI    TLXSTAT,TLXSRVAL    SET REVALUE REQUIRED                         
         DROP  R3                                                               
*                                                                               
SMST40   DS    0H                                                               
*                                                                               
SETMSTX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* REFRESH TLSTD VALUES FROM MASTER RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
REFMST   NTR1  ,                                                                
         NI    TLXVAL,FF-TLXVDEL                                                
         CLI   TRNRSTYP,99         TEST ADVANCE BILLING ITEM                    
         BNE   RMST02                                                           
         CLC   PG$LBLNO,BCSPACES   TEST BILLING UPDATED                         
         BH    RMST02                                                           
         OI    TLXVAL,TLXVDEL      NO - ITEM CAN BE DELETED                     
*                                                                               
RMST02   MVI   TLXPEND,0                                                        
         TM    PG$STAT,PG$REVS     TEST IF REVERSAL PENDING                     
         BZ    *+12                                                             
         OI    TLXPEND,TLXPREV                                                  
         B     RMST04                                                           
         CP    PP$AALLO,BCPZERO    TEST IF ANY ALLOCATION PENDING               
         BNE   *+14                                                             
         CP    PP$ACOMM,BCPZERO                                                 
         BE    RMST04                                                           
         OI    TLXPEND,TLXPALL                                                  
*                                                                               
RMST04   CP    PP$AWOFF,BCPZERO    TEST IF WRITE-OFF PENDING                    
         BE    *+8                                                              
         OI    TLXPEND,TLXPWOF                                                  
         CP    PP$AXFER,BCPZERO    TEST IF TRANSFER PENDING                     
         BE    *+8                                                              
         OI    TLXPEND,TLXPXFR                                                  
         CP    PP$AWOFR,BCPZERO    TEST IF WRITE-OFF RECOVERY PENDING           
         BE    *+8                                                              
         OI    TLXPEND,TLXPRCV                                                  
*                                                                               
         CP    PM$ANVBL,BCPZERO    TEST AMOUNT AVAILABLE FOR PENDING            
         BE    *+8                                                              
         OI    TLXPEND,TLXPAVL                                                  
*                                                                               
REFMSTX  B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* COMPARE STRING OPTIONS                                              *         
*                                                                     *         
* NTRY: P1 BYTE 0 = L'OUPUT VALUE                                     *         
*             1-3 = A(OUTOUT VALUE)                                   *         
*       P2      0 = X'80' ON IF NEGATIVE FILTER IS ALLOWED            *         
*                   X'0F' NIBBLE IS NUMBER OF ITEMS IN LIST           *         
*             1-3 = A(OPTION VALUE)                                   *         
***********************************************************************         
         SPACE 1                                                                
STRCMP   NTR1  ,                                                                
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)          R4=A(OUTPUT VALUE)                           
         XR    R3,R3                                                            
         ICM   R3,7,5(R1)          R3=A(OPTION VALUE)                           
*                                                                               
         TM    4(R1),X'80'         TEST NEGATIVE FILTER IS ALLOWED              
         BZ    SCMP02                                                           
         CLI   0(R3),OPTNEQ        TEST BIT IS ON                               
         BE    *+8                                                              
         NI    4(R1),FF-X'80'      NO - TURN PARAMETER BIT OFF                  
         LA    R3,1(R3)                                                         
*                                                                               
SCMP02   XR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LA    RE,X'0F'                                                         
         NR    R0,RE               R0 = NUMBER OF ITEMS IN LIST                 
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         BCTR  RF,0                RF = EX LENGTH OF OUTPUT                     
*                                                                               
SCMP04   EX    RF,SCMPOC           TEST ZEROS                                   
         BZ    STRCMPN                                                          
         EX    RF,SCMPCLC          COMPARE WITH OUTPUT                          
         BE    STRCMPY                                                          
         LA    R3,1(RF,R3)                                                      
         BCT   R0,SCMP04                                                        
*                                                                               
STRCMPN  TM    4(R1),X'80'         TEST NEGATING                                
         BO    EXITY                                                            
         B     EXITN                                                            
*                                                                               
STRCMPY  TM    4(R1),X'80'         TEST NEGATING                                
         BZ    EXITY                                                            
         B     EXITN                                                            
*                                                                               
SCMPOC   OC    0(0,R3),0(R3)                                                    
SCMPCLC  CLC   0(0,R3),0(R4)                                                    
         SPACE 1                                                                
***********************************************************************         
* SETTRN LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
STWORKD  DSECT                                                                  
STPARMS  DS    0XL12               INPUT PARAMETERS                             
STPACT   DS    XL1                 ACTION                                       
STPADIR  EQU   C'D'                ACCDIR RECORD                                
STPAMST  EQU   C'M'                ACCMST RECORD                                
STPAREF  EQU   C'R'                REFRESH TLSTD                                
STPASET  EQU   C'S'                SET UP TLSTD                                 
STPATRN  DS    AL3                 A(1ST TRANSACTION RECORD)                    
STPATRN2 DS    A                   A(2ND TRANSACTION RECORD)                    
STPAPROR DS    A                   A(PRO-RATA BLOCK)                            
STPARM   DS    6A                                                               
STDUB1   DS    D                                                                
STWORKL  EQU   *-STWORKD                                                        
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN OPTIONS FIELD                                *         
*                                                                     *         
* NTRY - FVAL MUST HAVE BEEN CALLED TO EXTRACT TWA FIELD INTO FVIHDR  *         
*        AND FVIFLD.                                                  *         
*        R1=A(OPTIONS VALIDATION TABLE)                               *         
*   OR                                                                *         
*       P1        = A(OPTIONS VALIDATION TABLE)                       *         
*       P2 BYTE 0 = X'80' ON IF OPEN COLUMNS DISALLOWED               *         
*                   X'40' ON TO SET DEFAULT VALUES ONLY               *         
*             1-3 = A(DEFAULT COLUMN LIST)                            *         
*       P3        = A(COLUMN PAIRS THAT MUSTN'T BOTH BE OPEN LIST)    *         
*                      - SEE NOPLSTD                                  *         
*                                                                     *         
* EXIT - CC=EQUAL IF OPTIONS FIELD IS OK                              *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                       *         
***********************************************************************         
         SPACE 1                                                                
         USING VOWORKD,RC          RC=A(LOCAL W/S)                              
VALOPT   DS    0H                                                               
         USING *,R8                                                             
         ST    R1,VOATAB                                                        
         CLI   0(R1),0             TEST R1=A(OPTION TABLE)                      
         BNE   *+10                                                             
         MVC   VOPARMS,0(R1)       OR R1=A(PARAMETER LIST)                      
         LR    RF,RB                                                            
         ICM   RF,8,=AL1(2)                                                     
         ST    RF,ASTROPT          SET A(STROPT) ROUTINE                        
*                                                                               
VALOPT02 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         TM    VOPINDS,VOPIDEFO    TEST SPECIAL DEFAULT CALL                    
         BO    VALOPT48                                                         
*                                                                               
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALOPT48                                                         
         MVC   VOPARM+08(2),=C',='                                              
         MVC   VOPARM+10(2),BCCHARS                                             
         GOTO1 VSCANNER,VOPARM,('VORHSL',FVIHDR),(X'80',VOAREA)                 
         MVC   VOAREAN(1),4(R1)    SAVE NUMBER OF LINES INPUT                   
         CLI   VOAREAN,0           TEST FOR INVALID INPUT                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTX                                                          
         MVI   FVINDX,1            SET FIRST FIELD INDEX NUMBER                 
         LA    R3,VOAREA           R3=A(SCANNER TABLE)                          
         B     VALOPT10                                                         
*                                  BUMP TO NEXT BLOCK ENTRY                     
VALOPT04 SR    R1,R1                                                            
         IC    R1,FVINDX           BUMP MULTIPLE FIELD INDEX                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,VOAREAN        TEST ALL OPTIONS PROCESSED                   
         BH    VALOPT48                                                         
         STC   R1,FVINDX                                                        
         LA    R3,VOWDTH(R3)                                                    
*                                                                               
VALOPT10 CLI   0(R3),0             TEST VALID KEYWORD LENGTH                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALOPTEL                                                         
         CLI   0(R3),OPTNAMLQ                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTEL                                                         
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=KEYWORD LENGTH-1                          
         XR    R2,R2               SAVE A(OPTIONS VALIDATION TABLE)             
         USING OPTTABD,R2          R2=A(OPTIONS VALIDATION TABLE)               
*                                  SEARCH TABLE FOR KEYWORD                     
VALOPT12 BAS   RE,NXTOPT                                                        
         BE    VALOPT20                                                         
         TM    BCINDS2,BCINTRS     TEST USER JUST NTRSES'D                      
         BO    REMOPT              YES - REMOVE OPTION                          
         MVC   FVMSGNO,=AL2(FVFKINV) EOT - INVALID KEYWORD                      
         B     VALOPTEL                                                         
*                                                                               
VALOPT20 SR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(OPTNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,OPTSHRT                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK+OPTNAMLQ(OPTSHTLQ),0(RE)                                  
         LA    RE,1(R1)                                                         
         CLM   RE,1,OPTMINKL       TEST L'INPUT LESS THAN MIN ALLOWED           
         BL    VALOPT12                                                         
         EX    R1,*+8                                                           
         BE    VALOPT26                                                         
         CLC   BCWORK(0),VOHDRL(R3) MATCH ON FULL OPTION NAME                   
         CLI   0(R3),OPTSHTLQ      TEST > SHORT KEYWORD                         
         BH    VALOPT12                                                         
         EX    R1,*+8                                                           
         BNE   VALOPT12                                                         
         CLC   BCWORK+OPTNAMLQ(0),VOHDRL(R3)                                    
*                                                                               
VALOPT26 GOTO1 TESTBIT,VOPARM,(OPTOPTN,VOMASK)                                  
         BZ    *+14                CHECK FOR DUPLICATED OPTION KEYWORD          
         MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     VALOPTEL                                                         
         GOTO1 SETBIT,VOPARM,(OPTOPTN,VOMASK) SET BIT POSITION                  
         MVI   5(R3),0             CLEAR SPECIAL CHARACTERS INDICATOR           
         CLC   OPTIADDR,=AL2(OPTDISQ)                                           
         BE    VALOPT28                                                         
         CLI   VOLHSL(R3),C'<'     TEST LESS THAN CHARACTER                     
         BNE   *+8                                                              
         OI    5(R3),OPTLEQ                                                     
         CLI   VOLHSL(R3),C'>'     TEST GREATER THAN CHARACTER                  
         BNE   *+8                                                              
         OI    5(R3),OPTGEQ                                                     
         TM    OPTINDS2,OPTNEQ     TEST NOT EQUALS CHARACTER IS SPECIAL         
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'*'     TEST NOT EQUALS CHARACTER                    
         BNE   *+8                                                              
         OI    5(R3),OPTNEQ                                                     
         TM    OPTINDS2,OPTPLQ     TEST PLUS CHARACTER IS SPECIAL               
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'+'     TEST PLUS CHARACTER                          
         BNE   *+8                                                              
         OI    5(R3),OPTPLQ                                                     
         TM    OPTINDS2,OPTMIQ     TEST MINUS CHARACTER IS SPECIAL              
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'-'     TEST MINUS CHARACTER                         
         BNE   *+8                                                              
         OI    5(R3),OPTMIQ                                                     
         ICM   RE,1,5(R3)          TEST SPECIAL CHARACTER INPUT                 
         BZ    VALOPT28                                                         
         CLI   1(R3),1             TEST DATA INPUT AFTER SPECIAL                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTER                                                         
         EX    RE,*+8                                                           
         BNZ   VALOPT28                                                         
         TM    OPTINDS2,0          TEST SPECIAL CHARACTER ALLOWED               
         MVC   FVMSGNO,=AL2(FVFIGLE) NO - INVALID                               
         B     VALOPTER                                                         
VALOPT28 SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST DATA INPUT AFTER EQUALS SIGN            
         BNZ   VALOPT30                                                         
         CLI   OPTMINDL,0          NO - TEST IF THAT'S OK                       
         BE    VALOPT30                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         MVI   FVSUBX,1                                                         
         B     VALOPTER                                                         
*                                                                               
VALOPT30 MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         LTR   RF,RF               TEST ZERO LENGTH FIELD                       
         BZ    VALOPT32                                                         
         BCTR  RF,0                                                             
         LA    R1,VOLHSL(R3)                                                    
         TM    5(R3),FF-(OPTMIQ)                                                
         BZ    *+10                                                             
         BCTR  RF,0                DROP FIRST CHARACTER (UNLESS MINUS)          
         LA    R1,1(R1)                                                         
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(R1)                                                  
VALOPT32 XC    FVIHDR,FVIHDR                                                    
         LA    RE,L'FVIHDR+1(RF)                                                
         STC   RE,FVTLEN                                                        
         LR    R0,RF               R0=LENGTH OF FIELD-1                         
         GOTO1 AFVAL,0                                                          
*                                                                               
         CLC   FVILEN,OPTMINDL     TEST L'DATA IS VALID                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALOPTER                                                         
         CLC   FVILEN,OPTMAXDL                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTER                                                         
*                                                                               
         TM    BCINDS2,BCINTRS     TEST USER JUST NTRSES'D                      
         BZ    VALOPT34                                                         
         TM    OPTINDS,OPTNRTN     TEST DIS= OPTION                             
         BZ    VALOPT34                                                         
         CLC   OPTIADDR,=AL2(OPTDISQ)                                           
         BE    REMOPT              YES - REMOVE IT                              
*                                                                               
VALOPT34 MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,3,OPTIADDR                                                    
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION RTN                
         BZ    VALOPT36                                                         
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         CLI   OPTIADDR,FF         TEST GENERAL ROUTINE                         
         BE    *+12                                                             
         ICM   RF,7,AOVERVAL+1                                                  
         B     VALOPT40                                                         
         LA    RE,VALGEN                                                        
         AR    RF,RE                                                            
         B     VALOPT40                                                         
*                                                                               
VALOPT36 TM    OPTINDS,OPTATAB                                                  
         BZ    VALOPT38                                                         
         A     RF,AOVERVAL                                                      
         B     VALOPT42                                                         
*                                                                               
VALOPT38 TM    OPTINDS,OPTARTN     TEST A(VALIDATION ROUTINE)                   
         BNZ   *+6                                                              
         DC    H'0'                NO - TABLE IS SCREWY                         
         A     RF,AOVERVAL                                                      
*                                                                               
VALOPT40 XC    BCWORK,BCWORK                                                    
         GOTO1 (RF),OPTTABD        GO TO ROUTINE                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALOPT46                                                         
         MVI   FVFLAG,X'01'        INDICATE A USER ERROR MESSAGE                
         BNE   VALOPTER                                                         
*                                                                               
VALOPT42 SR    R0,R0                                                            
         IC    R0,0(RF)            R0=L'LHS OF TABLE                            
         SR    R1,R1                                                            
         IC    R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'TABLE                                   
         SR    RE,RE                                                            
         IC    RE,FVXLEN           RE=L'DATA-1                                  
VALOPT44 CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDINV) YES - INVALID DATA VALUE                   
         B     VALOPTER                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT44                                                         
         AR    RF,R0               EXTRACT RHS OF TABLE INTO WORK               
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BCWORK(0),0(RF)                                                  
*                                                                               
VALOPT46 BAS   RE,SETOPT           MOVE DATA TO OPTION AREA                     
         B     VALOPT04                                                         
*                                                                               
VALOPT48 MVI   FVINDX,0            RESET INDEX/SUB-INDEX                        
         MVI   FVSUBX,0                                                         
         XR    R2,R2               TEST ALL REQUIRED OPTIONS WERE INPUT         
*                                  APPLY ANY DEFAULT OPTION VALUES              
VALOPT50 BAS   RE,NXTOPT           TEST E-O-T                                   
         BNE   VALOPTX                                                          
         GOTO1 TESTBIT,VOPARM,(OPTOPTN,VOMASK)                                  
         BNZ   VALOPT50            TEST OPTION NOT USED                         
         TM    OPTINDS,OPTDFLTO+OPTDFLTI                                        
         BNZ   VALOPT52            TEST OPTION HAS DEFAULT VALUE                
         TM    OPTINDS,OPTREQD     TEST OPTION REQUIRED                         
         BZ    VALOPT50                                                         
         MVC   FVMSGNO,=AL2(FVFREQD)                                            
         XR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   FVXTRA(OPTNAMLQ),0(RE)                                           
         B     VALOPTX                                                          
*                                                                               
VALOPT52 TM    OPTINDS,OPTDFLTO    TEST DEFAULT IS OUTPUT VALUE                 
         BZ    VALOPT54                                                         
         MVC   BCWORK(L'OPTDVAL),OPTDVAL                                        
         B     VALOPT56                                                         
VALOPT54 MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    FVIHDR,FVIHDR                                                    
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'OPTDVAL),OPTDVAL                                        
         LA    RE,FVIFLD+L'OPTDVAL-1                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,FVIFLD                                                        
         SR    RE,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'FVIHDR+1(RE)                                                
         STC   RF,FVTLEN                                                        
         LR    R0,RE               R0=LENGTH OF FIELD-1                         
         GOTO1 AFVAL,0                                                          
         XC    BCWORK,BCWORK                                                    
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,1,OPTIADDR+1                                                  
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION ROUTINE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         CLI   OPTIADDR,FF         TEST GENERAL ROUTINE                         
         BE    *+12                                                             
         ICM   RF,7,AOVERVAL+1                                                  
         B     *+10                                                             
         LA    RE,VALGEN                                                        
         AR    RF,RE                                                            
         GOTO1 (RF),OPTTABD        CALL APPLICATION VALIDATION ROUTINE          
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VALOPTX                                                          
*                                                                               
VALOPT56 BAS   RE,SETOPT           SET DEFAULT OPTION VALUE                     
         B     VALOPT50                                                         
*                                                                               
VALOPTER ICM   RF,1,8(R3)          ERROR ON RHS - PUT CURSOR ON DATA            
         BNZ   *+8                                                              
VALOPTEL IC    RF,4(R3)            ERROR ON LHS - PUT CURSOR ON KEYWORD         
         IC    RE,FVERRNDX                                                      
         AR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
VALOPTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* REMOVE OPTION FROM BASOPT AND RE-VALIDATE FIELD                     *         
*                                                                     *         
* R3 = A(SCANNER BLOCK ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
REMOPT   DS    0H                                                               
         MVI   VOWORK,C' '         REMOVE OFFENDING OPTION FROM INPUT           
         MVC   VOWORK+1(L'VOWORK-1),VOWORK                                      
         L     R1,FVADDR                                                        
         USING FHD,R1                                                           
         XR    R2,R2                                                            
         IC    R2,FHLN                                                          
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   VOWORK(0),FHDA                                                   
         XR    RE,RE                                                            
         ICM   RE,1,4(R3)          RE=START OF OPTION                           
         LA    RE,VOWORK(RE)                                                    
         XR    RF,RF                                                            
         ICM   RF,1,VOWDTH+4(R3)   RF=END OF OPTION                             
         BNZ   *+8                                                              
         LR    RF,R2                                                            
         BCTR  RE,0                                                             
         LA    RF,VOWORK(RF)                                                    
         MVC   0(L'FVIFLD,RE),0(RF)                                             
         EX    R2,*+4                                                           
         MVC   FHDA(0),VOWORK                                                   
         OI    FHOI,FHOITR                                                      
         NI    FHII,FF-FHIIVA                                                   
         GOTO1 AFVAL                                                            
         DROP  R1                                                               
         LA    RE,VOPARMS+L'VOPARMS                                             
         LA    RF,VOWORKL-L'VOPARMS                                             
         XR    R3,R3                                                            
         MVCL  RE,R2                                                            
         B     VALOPT02            RE-VALIDATE OPTIONS                          
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO MOVE OPTION VALUE FROM WORK INTO OPTION/FORMAT AREA      *         
*                                                                     *         
* NTRY - R2=A(OPTION TABLE ENTRY)                                     *         
*        R3=A(SCANNER BLOCK ENTRY)                                    *         
*        SCWORK=OPTION VALUE TO BE SET                                *         
***********************************************************************         
         SPACE 1                                                                
SETOPT   CLC   OPTIADDR,=AL2(OPTDISQ) DIS= SET IN VALDIS                        
         BER   RE                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,OPTOADDR                                                    
         TM    VOINDS,VOIGEN                                                    
         BO    *+12                                                             
         A     RF,AOVEROUT                                                      
         B     *+8                                                              
         A     RF,ALSVALS                                                       
         SR    R1,R1                                                            
         IC    R1,OPTOUTDL         R1=L'DATA (EXCLUDING QUALIFIER)              
         BCTR  R1,0                                                             
         TM    OPTINDS2,OPTGEQ+OPTLEQ+OPTNEQ                                    
         BZ    SETOPT02                                                         
         MVC   0(1,RF),5(R3)       MOVE DATA QUALIFIER                          
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
*                                                                               
SETOPT02 TM    OPTINDS,OPTBOOL     TEST IF A BIT VALUE                          
         BZ    *+12                                                             
         EX    R1,SETOPTOR         OR VALUE INTO OUTPUT AREA                    
         B     *+8                                                              
         EX    R1,SETOPTMV         MOVE VALUE TO OUTPUT AREA                    
         BR    RE                                                               
         SPACE 1                                                                
SETOPTOR OC    0(0,RF),BCWORK                                                   
SETOPTMV MVC   0(0,RF),BCWORK                                                   
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO GET NEXT OPTION TABLE ENTRY                              *         
*                                                                     *         
* NTRY: R2 = A(CURRENT OPTION TABLE ENTRY) OR 0 TO GET FIRST          *         
* EXIT: R2 = A(NEXT VALID OPTION TABLE ENTRY)                         *         
*       CC = LOW IF END-OF-TABLE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING OPTTABD,R2                                                       
NXTOPT   NTR1  ,                                                                
         LTR   R2,R2               TEST START TABLE                             
         BNZ   NXTOPT18                                                         
         L     R2,VOATAB                                                        
         NI    VOINDS,FF-VOIGEN                                                 
*                                                                               
NXTOPT02 CLI   OPTTABD,EOT         TEST END-OF-TABLE                            
         BNE   NXTOPT04                                                         
         TM    VOINDS,VOIGEN       TEST FINISHED GENERAL OPTIONS                
         BZ    NXTOPTL                                                          
         L     R2,VOANTRY          YES - CONTINUE WHERE LEFT OFF                
         LA    R2,L'OPTNAME(R2)                                                 
         NI    VOINDS,FF-VOIGEN                                                 
         B     NXTOPT02                                                         
*                                                                               
NXTOPT04 CLC   OPTNAME,=AL2(TRNOPTQ)                                            
         BNE   NXTOPT06            TEST STARTING GENERAL OPTIONS                
         ST    R2,VOANTRY                                                       
         LH    R2,=Y(TRNOPT-CLB42)                                              
         LA    R2,CLB42(R2)                                                     
         OI    VOINDS,VOIGEN                                                    
         B     NXTOPT02                                                         
*                                                                               
NXTOPT06 TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST THIS IS A DDS TERMINAL            
         BZ    NXTOPT18                                                         
*                                                                               
         CLI   OPTCTRY,CTRYALL     TEST OPTIONAL COUNTRY FILTER                 
         BE    NXTOPT08                                                         
         MVC   BCWORK(1),OPTCTRY                                                
         NI    BCWORK,FF-CTRYNOT                                                
         LA    RE,BNEQ             SET BNE                                      
         TM    OPTCTRY,CTRYNOT     TEST NEGATIVE FILTER                         
         BNO   *+8                                                              
         LA    RE,BEQ              SET BE                                       
         CLC   CUCTRY,BCWORK       COMPARE CONNECTED COUNTRY                    
         EX    RE,*+4                                                           
         NOP   NXTOPT18                                                         
*                                                                               
NXTOPT08 CLI   OPTACTB,0           TEST VALID FOR ALL ACTIONS                   
         BE    NXTOPT10                                                         
         CLC   CSACT,OPTACTB       NO - MATCH ON ACTION NUMBER                  
         BE    NXTOPT10                                                         
         TM    OPTACTB,ACTNOT      TEST NEGATIVE FILTER                         
         BZ    NXTOPT18                                                         
         MVC   BCWORK(1),OPTACTB                                                
         NI    BCWORK,FF-ACTNOT                                                 
         CLC   CSACT,BCWORK                                                     
         BE    NXTOPT18                                                         
*                                                                               
NXTOPT10 MVC   BCWORK(2),OPTFILT                                                
         OC    BCWORK(2),BCWORK                                                 
         BZ    NXTOPT12                                                         
         NC    BCWORK(2),MIXLOPTF                                               
         BZ    NXTOPT18                                                         
*                                                                               
NXTOPT12 CLI   OPTSECN,0           TEST SECURITY NUMBER GIVEN                   
         BE    NXTOPTY                                                          
         GOTO1 VSECRET,VOPARM,('SECPOPTP',ASECBLK),OPTSECN                      
         BE    NXTOPTY                                                          
*                                                                               
NXTOPT18 LA    R2,OPTTABL(R2)      BUMP R2                                      
         B     NXTOPT02                                                         
*                                                                               
NXTOPTL  CLI   *,FF                                                             
         B     NXTOPTX                                                          
*                                                                               
NXTOPTY  CR    RB,RB                                                            
*                                                                               
NXTOPTX  XIT1  REGS=(R2)                                                        
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET A BIT OF A BIT TABLE                                 *         
*                                                                     *         
* NTRY: P1=(BIT CODE, A(BIT TABLE))                                   *         
***********************************************************************         
         SPACE 1                                                                
SETBIT   XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         L     R0,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         LA    R1,MASKS(R1)        R1=A(MASK)                                   
         SRL   RF,3                                                             
         AR    RF,R0               RF=A(BYTE OF BIT)                            
         OC    0(1,RF),0(R1)                                                    
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO TEST A BIT OF A BIT TABLE                                *         
*                                                                     *         
* NTRY: P1=(BIT CODE, A(BIT TABLE))                                   *         
***********************************************************************         
         SPACE 1                                                                
TESTBIT  XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         L     R0,0(R1)                                                         
         LA    R1,X'07'                                                         
         NR    R1,RF                                                            
         IC    R1,MASKS(R1)        R1=MASK                                      
         SRL   RF,3                                                             
         AR    RF,R0               RF=A(BYTE OF BIT)                            
         EX    R1,TESTBITM         EX A TM                                      
         BR    RE                                                               
         SPACE 1                                                                
TESTBITM TM    0(RF),0                                                          
         SPACE 3                                                                
MASKS    DC    X'8040201008040201'                                              
         EJECT                                                                  
VOWORKD  DSECT                     ** VALOPT S/R LOCAL W/S **                   
VOPARMS  DS    0XL12                                                            
VOATAB   DS    A                   A(OPTIONS VALIDATION TABLE)                  
VOPINDS  DS    XL1                 INDICATOR BYTE                               
VOPINOPE EQU   X'80'               NO OPEN COLUMNS ALLOWED                      
VOPIDEFO EQU   X'40'               GET DEFAULT VALUES ONLY                      
VOPADEF  DS    AL3                 A(DEFAULT COLUMN LIST)                       
VOPANOPL DS    A                   A(NON-OPEN PAIR LIST)                        
         ORG   VOPARMS+L'VOPARMS                                                
VOPARM   DS    6A                                                               
ASTROPT  DS    A                   A(STROPT ROUTINE)                            
VOANTRY  DS    A                   SAVED A(OPTION TABLE)                        
VOINDS   DS    XL1                 INDICATOR BYTE                               
VOIGEN   EQU   X'80'               SCANNING GENERAL OPTIONS                     
VOMASK   DS    XL32                OPTION BIT MASK                              
VOMAXN   EQU   20                  MAXIMUM NUMBER OF SCANNER ENTRIES            
VOHDRL   EQU   12                  LENGTH OF HEADER                             
VOLHSL   EQU   VOHDRL+10           LENGTH OF LHS OF ENTRY                       
VORHSL   EQU   30                  LENGTH OF RHS OF ENTRY                       
VOWDTH   EQU   VOLHSL+VORHSL       WIDTH OF SCANNER ENTRY                       
VOAREAN  DS    XL1                 NUMBER OF SCANNER TABLE ENTRIES              
VOAREA   DS    (VOMAXN)XL(VOWDTH)  SCANNER TABLE                                
*                                                                               
VOWORK   DS    XL256                                                            
*                                                                               
VOGEN    DS    XL512               FOR GENERAL VALIDATION ROUTINES              
         DS    0D                                                               
VOWORKL  EQU   *-VOWORKD                                                        
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL VALIDATION ROUTINES                                         *         
***********************************************************************         
         SPACE 1                                                                
VALGEN   NTR1  ,                                                                
         LA    R8,VALGEN                                                        
         USING VALGEN,R8                                                        
         LR    R7,R1                                                            
         USING OPTTABD,R7                                                       
         XC    VOGEN(256),VOGEN    CLEAR VOGEN                                  
         XC    VOGEN+256(L'VOGEN-256),VOGEN+256                                 
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALDIS                DIS=LIST                                   
         B     VALYNO                YES/NO/ONLY                                
         B     VALYN                 YES/NO                                     
         B     VALWC                 WORKCODE FILTER                            
         B     VALTYP                INPUT TYPE                                 
         B     VALSTR                STRING TEXT RANGE                          
         B     VALDAT                DATE FILTER                                
         B     VALMOA                MOA RANGE                                  
         B     VALAMT                AMOUNT INPUT                               
         B     VALCMO                COMMISSION OVERRIDE RATE                   
         B     VALNY                 NO/YES                                     
         B     VALWCTYP              WCTYPE=O/P/R/T                             
         B     VALYNOP               ALLOC=Y/N/O/P                              
         B     VALPERD               PERIOD=(START)(-)(END)                     
         B     VALTXT                TEXT (INPUT=OUTPUT)                        
         B     VALWTOT               WCTOTAL=Y/N (OR SET DEFAULT)               
         B     VALSWI                SWITCH=Y/N (FOR ACTION ALLOCATE)           
         EJECT                                                                  
***********************************************************************         
* VALIDATE DIS=COLUMN LIST (+=DEFAULT LIST, ++=ALL COLUMNS)           *         
***********************************************************************         
         SPACE 1                                                                
VALDIS   DS    0H                                                               
         TM    VOPINDS,VOPIDEFO    DON'T BOTHER IF 'DEFAULT' CALL               
         BO    EXIT                                                             
*                                                                               
         GOTO1 PRCDIS                                                           
         BNE   EXIT                                                             
*                                                                               
         LA    R1,VDCLMLST                                                      
VDIS02   CLI   0(R1),EOT                                                        
         BE    VDIS10                                                           
*                                                                               
         CLC   0(L'VDCLMLST,R1),=AL2(VDCLMDEF)                                  
         BNE   VDIS04                                                           
         GOTO1 INSDEF,(R1)         INSERT DEFAULT COLS                          
         B     VDIS08                                                           
*                                                                               
VDIS04   CLC   0(L'VDCLMLST,R1),=AL2(VDCLMALL)                                  
         BNE   VDIS08                                                           
         GOTO1 INSALL,(R1)         INSERT ALL COLS                              
*                                                                               
VDIS08   LA    R1,L'VDCLMLST(R1)                                                
         B     VDIS02                                                           
*                                                                               
VDIS10   LA    RF,VDCLMLST                                                      
         SR    R1,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE AT LEAST 1 COLUMN                    
         SRL   R1,1                                                             
         STC   R1,VDCLMN           SET NUMBER OF COLUMNS                        
         CLI   VDCLMN,LSCLMMAX     TEST COLUMN LIST OVERFLOW                    
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   LSCLM,VDCLM                                                      
         B     EXIT                                                             
*                                                                               
VDISNO   MVC   FVXTRA(1),0(R3)     INVALID COLUMN                               
VDISNO02 LA    R0,FVIFLD                                                        
         SR    R3,R0                                                            
         STC   R3,FVERRNDX                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE CONVERT DIS= INPUT TO 2 CHARACTER LIST          *         
***********************************************************************         
         SPACE 1                                                                
PRCDIS   NTR1  ,                                                                
         LA    R3,FVIFLD           R3=A(INPUT)                                  
         XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FVIFLD(RE)                                                    
         MVC   0(L'BCEFFS,RE),BCEFFS   SET END OF INPUT                         
         LA    R4,VDCLMLST         R4=A(OUTPUT COLUMN LIST)                     
*                                                                               
PDIS02   CLI   0(R3),C'+'          '+' = DEFAULT COLUMNS                        
         BNE   PDIS04                                                           
         MVC   0(L'VDCLMLST,R4),=AL2(VDCLMDEF)                                  
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'+'          '++' = ALL COLUMNS                           
         BNE   PDIS18                                                           
         LA    R4,L'VDCLMLST(R4)                                                
         MVC   0(L'VDCLMLST,R4),=AL2(VDCLMALL)                                  
         LA    R3,1(R3)                                                         
         B     PDIS18                                                           
*                                                                               
PDIS04   NI    VDINDS,FF-VDIOREQ-VDI2CHAR                                       
         CLI   0(R3),C'&&'         TEST IF OPEN COLUMN REQUIRED                 
         BNE   PDIS06                                                           
         OI    VDINDS,VDIOREQ                                                   
         LA    R3,1(R3)                                                         
         CLI   0(R3),FF            ENSURE COLUMN CODE FOLLOWS                   
         BNE   PDIS06                                                           
         MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     PRCDISNO                                                         
*                                                                               
PDIS06   DS    0H                                                               
         GOTO1 GETCLM,VOPARM,(R3)                                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     PRCDISNO                                                         
         L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         MVC   VDCODE,LSCLMCOD                                                  
         TM    VDINDS,VDIOOK                                                    
         BZ    PDIS08              OPEN COLUMN NOT VALID                        
         TM    VOPINDS,VOPINOPE    TEST NO-OPENS ALLOWED                        
         BO    PDIS08                                                           
         TM    CLMINDS2,CLMIDEFO   TEST DEFAULT IS OPEN FIELD                   
         BZ    *+8                                                              
         OI    VDINDS,VDIOREQ                                                   
         B     PDIS10                                                           
*                                                                               
PDIS08   TM    VDINDS,VDIOREQ      TEST OPEN FIELD REQUIRED                     
         BZ    PDIS10              NO - OKAY                                    
         MVC   FVMSGNO,=AL2(AE$CNVIN)                                           
         B     PRCDISNO                                                         
*                                                                               
PDIS10   TM    VDINDS,VDIOREQ      TEST OPEN FIELD REQUIRED                     
         BZ    PDIS12                                                           
         NI    VDCODE+1,FF-LSCLMPRO                                             
*                                                                               
PDIS12   GOTO1 TSTDUP,VDCODE       TEST FOR DUPLICATIONS                        
         BE    PDIS14                                                           
         MVC   FVMSGNO,VDMSGNO                                                  
         BL    PRCDISNO                                                         
         BH    PRCDISN2                                                         
PDIS14   MVC   0(L'VDCLMLST,R4),VDCODE                                          
         L     R3,VDANXT                                                        
         DROP  R2                                                               
*                                                                               
PDIS18   LA    R4,L'VDCLMLST(R4)                                                
         CLI   0(R3),FF            TEST END OF INPUT LIST                       
         BNE   PDIS02                                                           
*                                                                               
PRCDISX  MVI   0(R4),EOT                                                        
         B     EXITY                                                            
*                                                                               
PRCDISNO MVC   FVXTRA(1),0(R3)                                                  
         TM    VDINDS,VDI2CHAR                                                  
         BZ    *+10                                                             
         MVC   FVXTRA(2),0(R3)                                                  
PRCDISN2 LA    R0,FVIFLD                                                        
         SR    R3,R0                                                            
         STC   R3,FVERRNDX                                                      
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INSERT DEFAULT LIST INTO VDCLMS                          *         
*                                                                     *         
* NTRY: R1 = A(POSTITION IN VDCLMS TO INSERT LIST)                    *         
***********************************************************************         
         SPACE 1                                                                
INSDEF   NTR1  ,                                                                
         LR    R4,R1                                                            
*                                                                               
         MVC   0(L'VDCLM2,R4),L'LSCLMLST(R4)   DELETE '+'                       
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,7,VOPADEF        R3=A(DEFAULT COLUMNS LIST)                   
IDEF02   CLI   0(R3),EOT           TEST END OF LIST                             
         BE    IDEF10                                                           
         GOTO1 GETCLM,VOPARM,(R3)                                               
         L     R3,VDANXT                                                        
         BNE   IDEF08                                                           
         L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         MVC   VDCODE,LSCLMCOD                                                  
         BCTR  R3,0                                                             
         MVC   VDCODE+1(1),0(R3)   COPY REQUEST FOR OPEN COLUMN                 
         LA    R3,1(R3)                                                         
*                                                                               
         TM    VDINDS,VDIOOK                                                    
         BO    *+12                                                             
         OI    VDCODE+1,LSCLMPRO   ONLY PROTECTED ALLOWD                        
         B     IDEF04                                                           
         TM    CLMINDS2,CLMIDEFO   TEST DEFAULT IS OPEN                         
         BZ    *+8                                                              
         NI    VDCODE+1,FF-LSCLMPRO                                             
         DROP  R2                                                               
*                                                                               
IDEF04   GOTO1 TSTDUP,VDCODE                                                    
         BL    IDEF08                                                           
         BNH   *+8                                                              
         OI    VDCODE+1,LSCLMPRO                                                
*                                                                               
         MVC   VDCLM2,0(R4)        INSERT COLUMN                                
         MVC   0(L'VDCLMLST,R4),VDCODE                                          
         LA    R4,L'VDCLMLST(R4)                                                
         MVC   0(L'VDCLM2,R4),VDCLM2                                            
*                                                                               
IDEF08   DS    0H                                                               
         B     IDEF02                                                           
*                                                                               
IDEF10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INSERT ALL COLUMNS INTO VDCLMS                           *         
*                                                                     *         
* NTRY: R1 = A(POSTITION IN VDCLMS TO INSERT LIST)                    *         
***********************************************************************         
         SPACE 1                                                                
INSALL   NTR1  ,                                                                
         LR    R4,R1                                                            
*                                                                               
         MVC   0(L'VDCLM2,R4),L'LSCLMLST(R4)   DELETE '++'                      
*                                                                               
         XC    ACLMDATA,ACLMDATA   CLEAR TO GET 1ST COLUMN                      
IALL02   GOTO1 ASETCLM,0           GET NEXT VALID ENTRY                         
         BL    IALL10                                                           
         L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         MVC   VDCODE,LSCLMCOD                                                  
         BH    *+8                                                              
         NI    VDCODE+1,FF-LSCLMPRO  OPEN COLUMN IF CAN BE                      
         TM    CLMINDS1,CLMIKEY    IGNORE KEY TYPE COLUMNS                      
         BO    IALL02                                                           
         TM    CLMINDS2,CLMIDEFO   TEST DEFAULT IS OPEN                         
         BO    *+8                                                              
         OI    VDCODE+1,LSCLMPRO                                                
         GOTO1 TSTDUP,VDCODE                                                    
         BL    IALL02              CC=LOW IF COLUMN ALREADY IN LIST             
         BNH   *+8                                                              
         OI    VDCODE+1,LSCLMPRO   CC=HIGH IF COLUMN MAY NOT BE OPEN            
         DROP  R2                                                               
*                                                                               
         MVC   VDCLM2,0(R4)        INSERT COLUMN                                
         MVC   0(L'VDCLMLST,R4),VDCODE                                          
         LA    R4,L'VDCLMLST(R4)                                                
         MVC   0(L'VDCLM2,R4),VDCLM2                                            
         B     IALL02                                                           
*                                                                               
IALL10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET COLUMN TABLE ENTRY                                   *         
*                                                                     *         
* NTRY: P1 = A(INPUT)                                                 *         
* EXIT: VDANST(NEXT CHARACTER)                                        *         
*       ACLMDATA SET UP                                               *         
***********************************************************************         
         SPACE 1                                                                
GETCLM   NTR1  ,                                                                
         L     R3,0(R1)                                                         
         NI    VDINDS,FF-VDIOOK-VDI2CHAR-VDIINV                                 
         GOTO1 ASETCLM,VOPARM,(R3)                                              
         BNL   *+8                                                              
         OI    VDINDS,VDIINV       INVALID COLUMN                               
         BH    *+8                                                              
         OI    VDINDS,VDIOOK       OPEN IS OKAY                                 
*                                                                               
         TM    0(R1),X'80'         TEST 2 CHARACTER CODE                        
         BNZ   *+12                                                             
         LA    R3,1(R3)                                                         
         B     GETCLMX                                                          
         LA    R3,2(R3)                                                         
         OI    VDINDS,VDI2CHAR                                                  
         B     GETCLMX                                                          
*                                                                               
GETCLMX  ST    R3,VDANXT                                                        
         TM    VDINDS,VDIINV                                                    
         BZ    EXITY                                                            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST WHETHER COLUMN CAN BE ADDED TO LIST                 *         
*                                                                     *         
* NTRY: R1=A(COLUMN CODE)                                             *         
* EXIT: CC=EQUAL IF OKAY                                              *         
*       CC=LOW IF DUPLICATED COLUMN                                   *         
*       CC=HIGH IF INVALID PAIRING                                    *         
***********************************************************************         
         SPACE 1                                                                
TSTDUP   NTR1  ,                                                                
         MVC   VDDUP1,0(R1)                                                     
         OI    VDDUP1+1,LSCLMPRO                                                
         LA    R4,VDCLMLST         R4=A(LIST OF CURRENT CODES)                  
TDUP02   CLI   0(R4),EOT                                                        
         BE    TDUP10                                                           
         MVC   VDDUP2,0(R4)                                                     
         OI    VDDUP2+1,LSCLMPRO                                                
         CLC   VDDUP1,VDDUP2       TEST DUPLICATION                             
         BE    *+12                                                             
         LA    R4,L'VDCLMLST(R4)                                                
         B     TDUP02                                                           
         MVC   VDMSGNO,=AL2(AE$DUPCM)                                           
         B     TSTDUPL                                                          
*                                                                               
TDUP10   TM    1(R1),LSCLMPRO      TEST TESTED COLUMN IS OPEN                   
         BO    TSTDUPY                                                          
         ICM   RF,15,VOPANOPL      TEST NON-OPEN PAIR LIST                      
         BZ    TSTDUPY                                                          
         USING NOPLSTD,RF                                                       
TDUP12   CLI   NOPLSTD,EOT                                                      
         BE    TSTDUPY                                                          
         CLC   VDDUP1,NOPCLM1      MATCH ON COLUMN 1                            
         BNE   *+14                                                             
         MVC   VDDUP2,NOPCLM2                                                   
         B     TDUP14                                                           
         CLC   VDDUP1,NOPCLM2      MATCH ON COLUMN 2                            
         BNE   TDUP18                                                           
         MVC   VDDUP2,NOPCLM1                                                   
*                                                                               
TDUP14   NI    VDDUP2+1,FF-LSCLMPRO                                             
         LA    R4,VDCLMLST                                                      
TDUP16   CLI   0(R4),EOT           SEARCH FOR OPEN OTHER COLUMN IN LIST         
         BE    TDUP18                                                           
         CLC   VDDUP2,0(R4)                                                     
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     TDUP16                                                           
         MVC   VDMSGNO,NOPMSGNO    SET MESSAGE NUMBER                           
         B     TSTDUPH                                                          
*                                                                               
TDUP18   LA    RF,NOPLSTL(RF)                                                   
         B     TDUP12                                                           
         DROP  RF                                                               
*                                                                               
TSTDUPY  B     EXITY               SET CC=EQUAL                                 
*                                                                               
TSTDUPL  B     EXITL               SET CC=LOW                                   
*                                                                               
TSTDUPH  B     EXITH               SET CC=HIGH                                  
         SPACE 1                                                                
VOWORKD  DSECT                     ** VALDIS LOCAL W/S **                       
         ORG   VOGEN                                                            
*                                                                               
VDINDS   DS    XL1                 INDICATOR BYTE                               
VDIOREQ  EQU   X'80'               OPEN COLUMN REQUESTED                        
VDIOOK   EQU   X'40'               OPEN COLUMN IS OK                            
VDI2CHAR EQU   X'20'               TWO CHARACTER CODE                           
VDIINV   EQU   X'10'               INVALID COLUMN                               
VDANXT   DS    A                   A(NEXT COLUMN RETURNED BY GETCLM)            
*                                                                               
VDCODE   DS    XL2                 COLUMN CODE                                  
VDDUP1   DS    XL2                 CODE 1 FOR DUPLICATION TEST                  
VDDUP2   DS    XL2                 CODE 2 FOR DUPLICATION TEST                  
VDMSGNO  DS    XL2                 ERROR MESSAGE NUMBER                         
*                                                                               
VDCLM    DS    0XL(L'LSCLM)        COPY OF LSCLM                                
VDCLMN   DS    XL1                                                              
VDCLMLST DS    (LSCLMMAX)XL(L'LSCLMLST)                                         
VDCLMDEF EQU   X'FFFF'             DEFAULT COLUMNS (DIS=+)                      
VDCLMALL EQU   X'EEEE'             ALL COLUMNS (DIS=++)                         
VDCLM2   DS    XL(LSCLMMAX*L'LSCLMLST) EXTRA SAVE AREA                          
         DS    (L'VOGEN-(*-VOGEN))X  N/D                                        
         SPACE 1                                                                
NOPLSTD  DSECT                     ** NON-OPEN PAIR LIST **                     
NOPCLM1  DS    XL1                 COLUMN 1                                     
NOPCLM2  DS    XL1                 COLUMN 2                                     
NOPMSGNO DS    XL2                 ERROR MESSAGE IF BOTH COLS ARE OPEN          
NOPLSTL  EQU   *-NOPLSTD                                                        
         SPACE 1                                                                
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE WCTOTAL=YES/NO (OR SET DEFAULT)                            *         
***********************************************************************         
         SPACE 1                                                                
VALWTOT  DS    0H                                                               
         CLI   FVIFLD,OPTDEFQ      TEST SETTING DEFAULT                         
         BNE   VALYN               NO - NORMAL YES/NO                           
         MVI   BCWORK,C'Y'                                                      
*&&US*&& B     EXIT                                                             
*&&UK                                                                           
         CLI   CUCTRY,CTRYHOL                                                   
         BE    EXIT                                                             
         MVI   BCWORK,C'N'                                                      
         B     EXIT                                                             
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE SWITCH=YES/NO (OR SET DEFAULT)                             *         
***********************************************************************         
         SPACE 1                                                                
VALSWI   DS    0H                                                               
         CLI   FVIFLD,OPTDEFQ      TEST SETTING DEFAULT                         
         BNE   VSWI02                                                           
         MVI   BCWORK,C'N'                                                      
*&&UK*&& CLI   P#ALAUTM,C'Y'       DEFALUT IS 'N', SO TEST FOR 'Y'              
*&&UK*&& BNE   EXIT                                                             
*&&US*&& CLI   P#ALAUTM,C'N'       DEFAULT IS 'Y', SO TEST FOR 'N'              
*&&US*&& BE    EXIT                                                             
         MVI   BCWORK,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
VSWI02   DS    0H                                                               
*&&UK*&& CLI   P#ALAUTM,C'Y'       DEFALUT IS 'N', SO TEST FOR 'Y'              
*&&UK*&& BNE   VALYN                                                            
*&&US*&& CLI   P#ALAUTM,C'N'       DEFAULT IS 'Y', SO TEST FOR 'N'              
*&&US*&& BE    VALYN                                                            
         B     VALNY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE Y/N/O/P OPTIONS                                            *         
***********************************************************************         
         SPACE 1                                                                
VALYNOP  DS    0H                                                               
         SR    RF,RF               TEST PARTIAL                                 
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'P'                                                      
         LH    RE,=Y(UC@PART-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)     NO - TEST Y/N/O                              
         B     VALYNO02                                                         
         SPACE 1                                                                
***********************************************************************         
* VALIDATE Y/N/O OPTIONS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALYNO   DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
*                                                                               
VALYNO02 MVI   BCWORK,C'Y'         TEST YES                                     
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'N'         TEST NO                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'O'         TEST ONLY                                    
         LH    RE,=Y(UC@ONLY-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
VALYNO2  MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE Y/N OPTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
VALYN    DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'Y'                                                      
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'N'                                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKCODE FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALWC    DS    0H                                                               
         XR    R0,R0               SPECIAL CODE TO TEST FOR '*'                 
         CLI   FVIFLD,C'*'         (TO ALLOW W/C **)                            
         BNE   VWC02                                                            
         CLI   FVIFLD+2,C' '                                                    
         BE    VWC02                                                            
         CLC   FVIFLD+2(1),BCSLASH                                              
         BE    VWC02                                                            
         LA    R0,OPTNEQ                                                        
         MVC   FVIFLD(L'FVIFLD-1),FVIFLD+1                                      
         LA    R0,L'FVIFLD                                                      
         GOTO1 AFVAL,0                                                          
         LA    R0,OPTNEQ                                                        
*                                                                               
VWC02    GOTO1 ASTROPT,VOPARM,(BCSLASH,VALWCR),(2,2),('LSWCODEN',2)             
         BNE   EXIT                                                             
         MVC   BCWORK+L'LSWC(L'LSWC),BCWORK                                     
         MVC   BCWORK+1(L'LSWC),BCWORK+L'LSWC                                   
         STC   R0,BCWORK                                                        
         B     EXIT                                                             
*                                                                               
VALWCR   NTR1  ,                                                                
         MVC   BCWORK(L'LSWCODE),FVIFLD                                         
*                                                                               
         CLC   =C'**',BCWORK                                                    
         BE    EXIT                ORDER W/C IS VALID                           
         CLC   =C'99',BCWORK                                                    
         BE    EXIT                BILL W/C IS VALID                            
         PUSH  USING                                                            
         USING WCORECD,IOKEY       READ WORKCODE RECORD                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),BCCPYPRD                            
         MVC   WCOKWRK,BCWORK                                                   
         GOTO1 AIO,IORD+IOACCDIR+IO1                                            
         BE    EXIT                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT TYPE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALTYP   DS    0H                                                               
         GOTO1 ASTROPT,VOPARM,(BCSLASH,VALTYPR),(1,3),(L'LSTYPE,1)              
         B     EXIT                                                             
         SPACE 1                                                                
VALTYPR  NTR1  ,                                                                
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         TM    FVIIND,FVINUM       TEST INPUT IS NUMERIC                        
         BZ    EXITN                                                            
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         MVC   FVMSGNO,=AL2(AE$IVTYP)                                           
         CP    BODUB1,=P'250'                                                   
         BH    EXITN                                                            
         CVB   R0,BODUB1                                                        
         LTR   R0,R0                                                            
         BZ    EXITN                                                            
         STC   R0,BCWORK                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE STRING TEXT RANGE                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSTR   DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,OPTMAXDL         MAXIMUM L'DATA                               
         BCTR  R6,0                MINUS L'SEPARATOR                            
         SRL   R6,1                R6=MAXIMUM L'START/L'END                     
         LA    R3,BCWORK                                                        
         SR    RF,RF                                                            
         ICM   RF,1,FVXLEN                                                      
         BZ    VALST1              SINGLE CHARACTER INPUT                       
         LA    RE,FVIFLD                                                        
         LA    R0,1(RF)                                                         
         LA    R2,0(RE,RF)                                                      
         CLI   0(R2),C'-'                                                       
         BE    VALST2                                                           
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
VALST1   CLM   R6,1,FVILEN                                                      
         BL    VALSTHI                                                          
         LA    R0,2                                                             
VALST1A  MVC   0(1,R3),FVILEN      START CODE LENGTH                            
         EX    RF,*+4                                                           
         MVC   1(0,R3),FVIFLD                                                   
         LA    R3,1(R6,R3)                                                      
         BCT   R0,VALST1A                                                       
         B     EXIT                                                             
*                                                                               
VALST2   SR    R2,RE               R2=L'FIRST PART                              
         BZ    VALST4              NO FIRST PART                                
         CR    R2,R6               CHECK LENGTH                                 
         BH    VALSTHI                                                          
         STC   R2,0(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   1(0,R3),FVIFLD                                                   
         LA    R2,1(R2)                                                         
         CR    R2,RF                                                            
         BE    EXIT                NO SECOND PART                               
*                                                                               
VALST4   LA    RE,1(R2,RE)         RE=A(2ND PART)                               
         SR    RF,R2                                                            
         CR    RF,R6               CHECK LENGTH                                 
         BH    VALSTHI                                                          
         LA    R3,1(R3,R6)                                                      
         STC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,R3),0(RE)                                                    
         B     EXIT                                                             
*                                                                               
VALSTHI  MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD FOR TRANSACTION DATE                                *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   DS    0H                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(CULANG,BOWORK1)                  
         USING PERVALD,RF                                                       
         LA    RF,BOWORK1                                                       
         TM    4(R1),X'03'                                                      
         BZ    VALDAT2                                                          
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXIT                                                             
VALDAT2  XC    BCWORK(L'LSDATST),BCWORK                                         
         MVC   BCWORK+(LSDATEN-LSDATST)(L'LSDATEN),=X'FFFFFF'                   
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BO    *+10                                                             
         MVC   BCWORK(L'LSDATST),PVALPSTA                                       
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FVIFLD-1(RE)                                                  
         CLI   0(RE),C'-'                                                       
         BE    *+10                                                             
         MVC   BCWORK+(LSDATEN-LSDATST)(L'LSDATEN),PVALPEND                     
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE PERIOD FOR TRANSACTION MOA                                 *         
***********************************************************************         
         SPACE 1                                                                
VALMOA   DS    0H                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(CULANG,BOWORK1)                  
         USING PERVALD,RF                                                       
         LA    RF,BOWORK1                                                       
         TM    4(R1),X'03'                                                      
         BNZ   VALMOA2                                                          
         TM    PVALASSM,PVALASD+PVALAED                                         
         BO    VALMOA4             START AND END DAY BOTH ASSUMED - OK          
VALMOA2  MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXIT                                                             
*                                                                               
VALMOA4  XC    BCWORK(L'LSDATST),BCWORK                                         
         MVC   BCWORK+(LSDATEN-LSDATST)(L'LSDATEN),=X'FFFFFF'                   
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BO    *+10                                                             
         MVC   BCWORK(L'LSDATST),PVALPSTA                                       
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FVIFLD-1(RE)                                                  
         CLI   0(RE),C'-'                                                       
         BE    *+10                                                             
         MVC   BCWORK+(LSDATEN-LSDATST)(L'LSDATEN),PVALPEND                     
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMOUNT INPUT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         MVC   BCBYTE1,CSCURCPY+(CURTDECP-CURTABD)                              
         OI    BCBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BCBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   BODMCB,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXIT                                                             
         ZAP   BCWORK(L'LSAMOUNT),BODMCB+4(8)                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMISSION OVERRIDE RATES                                  *         
***********************************************************************         
         SPACE 1                                                                
VALCMO   DS    0H                                                               
         XC    VCSCNBLK(5*SCBLKLQ),VCSCNBLK                                     
         GOTO1 VSCANNER,BODMCB,FVIHDR,(5,VCSCNBLK),C',=/ '                      
         MVC   VCSCN#,4(R1)                                                     
         CLI   VCSCN#,0            TEST ANY INPUT                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         LA    R3,BCWORK                                                        
         LA    R2,VCSCNBLK                                                      
         USING SCANBLKD,R2                                                      
VCM02    SR    RE,RE                                                            
         ICM   RE,1,VCSCN#                                                      
         BZ    EXIT                                                             
         BCTR  RE,0                                                             
         STC   RE,VCSCN#                                                        
*                                                                               
IO       USING WCORECD,IOKEY       READ WORKCODE RECORD                         
         MVC   IO.WCOKEY,BCSPACES                                               
         MVI   IO.WCOKTYP,WCOKTYPQ                                              
         MVC   IO.WCOKCPY,CUABIN                                                
         MVC   IO.WCOKUNT(L'WCOKUNT+L'WCOKLDG),BCCPYPRD                         
         MVC   IO.WCOKWRK,SC1STFLD                                              
         GOTO1 AIO,IORD+IOACCDIR+IO1                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXIT                                                             
         MVC   0(L'WCOKWRK,R3),IO.WCOKWRK                                       
         DROP  IO                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         GOTO1 VCASHVAL,BODMCB,(X'84',SC2NDFLD),(RF)                            
         CLI   BODMCB,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXIT                                                             
         ZAP   L'WCOKWRK(L'GOAGYCOM,R3),BODMCB+4(8)                             
         LA    R3,L'WCOKWRK+L'GOAGYCOM(R3)                                      
         LA    R2,SCBLKLQ(R2)                                                   
         B     VCM02                                                            
         DROP  R2                                                               
         SPACE 1                                                                
VOWORKD  DSECT                     * VALCMO W/S *                               
         ORG   VOGEN                                                            
VCSCN#   DS    XL1                                                              
VCSCNBLK DS    5XL(SCBLKLQ)                                                     
         ORG   VOGEN+L'VOGEN                                                    
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE Y/N OPTIONS (REVERSE IT SO 'Y' MEANS NO, 'N' MEANS YES     *         
***********************************************************************         
         SPACE 1                                                                
VALNY    DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'N'                                                      
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'Y'                                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE WCTYPE=O/P/R/T                                             *         
***********************************************************************         
         SPACE 1                                                                
VALWCTYP DS    0H                                                               
         GOTO1 ASTROPT,VOPARM,(BCSLASH,VALWCTR),(1,1),(L'LSWCTCOD,1)            
         B     EXIT                                                             
*                                                                               
VALWCTR  NTR1  ,                                                                
         MVC   BCWORK(1),FVIFLD                                                 
         CLI   FVIFLD,C'O'         OUT OF POCKET                                
         BE    EXIT                                                             
         CLI   FVIFLD,C'P'         PREBILLING                                   
         BE    EXIT                                                             
         CLI   FVIFLD,C'R'         RETAINER                                     
         BE    EXIT                                                             
         CLI   FVIFLD,C'T'         TIME                                         
         BE    EXIT                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD=(START)(-)(END)                                     *         
***********************************************************************         
         SPACE 1                                                                
VALPERD  DS    0H                                                               
         GOTO1 AVALPER,BOPARM,FVIHDR,0                                          
         BNE   EXIT                                                             
         MVC   BCDUB(L'PVALCSTA),BCWORK+(PVALCSTA-PERVALD)                      
         MVC   BCDUB+L'PVALCSTA(L'PVALCEND),BCWORK+(PVALCEND-PERVALD)           
         MVC   BCWORK(L'PVALCSTA+L'PVALCEND),BCDUB                              
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TEXT                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALTXT   DS    0H                                                               
         MVC   BCWORK(1),FVILEN                                                 
         IC    RE,OPTOUTDL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     EXIT                                                             
         MVC   BCWORK+1(0),FVIFLD                                               
         EJECT                                                                  
***********************************************************************         
* INTERNAL ROUTINE TO VALIDATE OPTION THAT IS A STRING                *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'SEPERATOR'                                      *         
*             1-3 = A(VALIDATION ROUTINE)                             *         
*       P2 BYTE 0 = MINIMUM L'INPUT (PER ITEM)                        *         
*               3 = MAXIMUM L'INPUT (PER ITEM)                        *         
*       P3 BYTE 0 = MAXIMUM NUMBER OF ITEMS                           *         
*               3 = L'OUTPUT (PER ITEM)                               *         
***********************************************************************         
         SPACE 1                                                                
         USING SOWORKD,RC          RC=A(LOCAL W/S)                              
STROPT   DS    0H                                                               
         USING *,R8                                                             
         MVC   SOSEP,0(R1)         SET UP INPUT PARAMETERS                      
         MVC   SOAVAL+1(L'SOAVAL-1),1(R1)                                       
         MVC   SOMINL,4(R1)                                                     
         MVC   SOMAXL,7(R1)                                                     
         MVC   SOMAX#,8(R1)                                                     
         IC    RE,11(R1)                                                        
         BCTR  RE,0                                                             
         STC   RE,SOOUTXL                                                       
         MVC   SOREGS,4(RD)                                                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,12,=C',='                                                     
         ICM   RF,2,SOSEP                                                       
         GOTO1 VSCANNER,SOPARM,FVIHDR,(X'80',SOAREA),(RF)                       
         XR    R4,R4               R4 = NUMBER OF ITEMS IN LIST                 
         ICM   R4,1,4(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SOAREA                                                        
         USING SCANBLKD,R3         R3=A(SCAN BLOCK ENTRY)                       
         LA    R2,SOOUT            R2 = A(OUTPUT)                               
*                                                                               
         CLM   R4,1,SOMAX#         TEST TOO MANY ITEMS                          
         BNH   SOPT12                                                           
         BCTR  R4,0                                                             
         MH    R4,=Y(SCBLKLQ)                                                   
         AR    R3,R4                                                            
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     STROPTN                                                          
*                                                                               
SOPT12   XC    FVIHDR,FVIHDR                                                    
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'SC1STFLD),SC1STFLD                                      
         MVC   FVMINL,SOMINL                                                    
         MVC   FVMAXL,SOMAXL                                                    
         LA    R0,L'SC1STFLD                                                    
         GOTO1 AFVAL,0                                                          
         BNE   STROPTN                                                          
*                                                                               
         GOTO1 SOVAL                                                            
         BNE   STROPTN                                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,SOOUTXL                                                       
         EX    RE,*+4                                                           
         MVC   0(0,R2),BCWORK                                                   
         LA    R2,1(RE,R2)         BUMP R2 ALONG                                
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R4,SOPT12                                                        
*                                                                               
STROPTY  MVC   BCWORK,SOOUT                                                     
         B     EXITY                                                            
*                                                                               
STROPTN  MVC   FVERRNDX,SC1STNUM                                                
         B     EXITN                                                            
         DROP  R3                                                               
         SPACE 1                                                                
SOVAL    NTR1  ,                   ** CALL USERS VALIDATION ROUTINE **          
         XC    BCWORK,BCWORK                                                    
         L     RF,SOAVAL                                                        
         L     RE,SOREGS                                                        
         LM    R2,RC,28(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* STROPT LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
SOWORKD  DSECT                                                                  
SOPARM   DS   6A                                                                
*                                                                               
SOREGS   DS    A                                                                
*                                                                               
SOAVAL   DS    A                   A(VALIDATION ROUTINE)                        
SOSEP    DS    CL1                 SEPERATOR CHARACTER                          
SOMINL   DS    XL1                 MINIMUM L'INPUT                              
SOMAXL   DS    XL1                 MAXIMUM L'INPUT                              
SOMAX#   DS    XL1                 MAXIMUM NUMBER OF ITEMS                      
SOOUTXL  DS    XL1                 EXECUTABLE L'OUTPUT PER ITEM                 
*                                                                               
SOAREA   DS    10XL(SCBLKLQ)       SCANNER TABLE                                
*                                                                               
SOOUT    DS   XL(L'BCWORK)         OUTPUT                                       
*                                                                               
SOWORKL  EQU  *-SOWORKD                                                         
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL TRANSACTION OPTIONS                                         *         
***********************************************************************         
         SPACE 1                                                                
TRNOPT   DS    0X                                                               
*&&UK                                                                           
*                                  AUTH=Y/N/O                                   
         DC    AL2(UC8AUTHD-TWAD,UC3AUTHD-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(0)                                   
         DC    AL1(0,0,0,1,4,L'LSAUTH)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSAUTH-LSVALSD)                                      
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  HELD=Y/N/O                                   
         DC    AL2(UC8HELD-TWAD,UC3HELD-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(0)                                   
         DC    AL1(0,0,0,1,4,L'LSHELD)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSHELD-LSVALSD)                                      
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
*                                                                               
*                                  WC=XX/*XX                                    
         DC    AL2(UC8WC-TWAD,UC2WC-TWAD)                                       
         DC    AL1(OPTNRTN,0),AL2(0)                                            
         DC    AL1(0,0,0,L'LSWCODE,30,L'LSWC)                                   
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTWCQ,LSWC-LSVALSD)                                         
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  TYPE=NN/*NN                                  
         DC    AL2(UC8TYPE-TWAD,UC3TYPE-TWAD)                                   
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,30,L'LSTYP-1)                                        
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTTYPQ,LSTYP-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  REF=XXXXXX-XXXXXX/*XXXXXX-XXXXXX             
         DC    AL2(UC8REF-TWAD,UC3REF-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,2*L'TRNKREF+1,L'LSREF)                               
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTSTRQ,LSREF-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  DATE=DDMMMYY-DDMMMYY                         
         DC    AL2(UC8DATE-TWAD,UC3DATE-TWAD)                                   
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,17,L'LSDATE)                                         
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTDATQ,LSDATE-LSVALSD)                                      
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  BAT=XXXX-XXXX/*XXXX-XXXX                     
         DC    AL2(UC8BAT-TWAD,UC3BAT-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,2*L'TRNBREF+1,L'LSBAT)                               
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTSTRQ,LSBAT-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  MOA=MMMYY-MMMYY                              
         DC    AL2(UC8MOA-TWAD,UC3MOA-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,15,L'LSMOA)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTMOAQ,LSMOA-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  AMT=NN.NN-NN.NN/*NN.NN-NN.NN                 
         DC    AL2(UC8AMT-TWAD,UC3AMT-TWAD)                                     
         DC    AL1(OPTNRTN,OPTGEQ+OPTLEQ+OPTNEQ),AL2(0)                         
         DC    AL1(0,0,0,1,15,L'LSAMT)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTAMTQ,LSAMT-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  COMM=AA NN.NNNN/BB NN.NNNN                   
         DC    AL2(UC8CMN-TWAD,UC3CMN-TWAD)                                     
         DC    AL1(OPTNRTN,0),AL2(OPTFCOMM)                                     
         DC    AL1(0,0,0,1,54,L'LSCOM)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTCMOQ,LSCOM-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  ALLOCATED=Y/N/O/P                            
         DC    AL2(UC@ALLOC-TWAD,UC@ALLOC-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(0)                                   
         DC    AL1(0,0,0,1,4,L'LSALLOC)                                         
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOPQ,LSALLOC-LSVALSD)                                    
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&US                                                                           
*                                  LABOR=Y/N/O                                  
         DC    AL2(UC@LABOR-TWAD,UC@LABOR-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(0)                                   
         DC    AL1(0,0,0,1,4,L'LSLABOR)                                         
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSLABOR-LSVALSD)                                     
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  WKTYPE=O/P/R/T                               
         DC    AL2(UC@WKTYP-TWAD,UC@WKTYP-TWAD)                                 
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,10,L'LSWCTYPE)                                       
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTWCTQ,LSWCTYPE-LSVALSD)                                    
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  REVERSAL=Y/N/O                               
         DC    AL2(UC@RVRS-TWAD,UC@RVRS-TWAD)                                   
         DC    AL1(OPTIDDS+OPTNRTN+OPTDFLTO,0),AL2(0)                           
         DC    AL1(0,0,0,1,4,L'LSREVERS)                                        
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSREVERS-LSVALSD)                                    
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
*                                  REVERSAL=Y/N/O                               
         DC    AL2(UC@RVRS-TWAD,UC@RVRS-TWAD)                                   
         DC    AL1(OPTIDDS+OPTNRTN+OPTDFLTO,0),AL2(0)                           
         DC    AL1(0,0,0,1,4,L'LSREVERS)                                        
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSREVERS-LSVALSD)                                    
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  ACTDATE=DDMMMYY-DDMMMYY                      
         DC    AL2(UC8ACTYD-TWAD,UC3ACTYD-TWAD)                                 
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,17,L'LSACTDAT)                                       
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTPERDQ,LSACTDAT-LSVALSD)                                   
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  CONTRA=ULACCOUNT                             
         DC    AL2(UC8CTR-TWAD,UC3CTR-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,2,14,L'LSULC)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTTXTQ,LSULC-LSVALSD)                                       
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  WCTOT=Y/N                                    
         DC    AL2(UC8WCTOT-TWAD,UC3WCTOT-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTI,0),AL2(OPTFWCT)                             
         DC    AL1(0,0,0,1,4,L'LSWCTOT)                                         
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTWTOTQ,LSWCTOT-LSVALSD)                                    
         DC    AL1(OPTDEFQ),AL3(0)                                              
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  BILLED=Y/N/O                                 
         DC    AL2(UC8BLD-TWAD,UC3BLD-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(OPTFBLDY)                            
         DC    AL1(0,ACTREV,0,1,4,L'LSBILD)                                     
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSBILD-LSVALSD)                                      
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  BILLED=Y/N/O                                 
         DC    AL2(UC8BLD-TWAD,UC3BLD-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(OPTFBLDN)                            
         DC    AL1(0,ACTNOT+ACTREV,0,1,4,L'LSBILD)                              
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSBILD-LSVALSD)                                      
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  UNAVAILABLE=Y/N/O                            
         DC    AL2(UC@UNAV-TWAD,UC@UNAV-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(OPTFUNAV)                            
         DC    AL1(0,0,0,1,4,L'LSUNAV)                                          
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTYNOQ,LSUNAV-LSVALSD)                                      
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
TRNOPTX  DS    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
TRNORDER DC    C'**'                                                            
TRNBILL  DC    C'99'                                                            
INCUL    DC    C'SI'                                                            
EXPUL    DC    C'SE'                                                            
SKUL     DC    C'SK'                                                            
UL1R     DC    C'1R'                                                            
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         SPACE 1                                                                
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKB                                                     
* ACCLBCOLS                                                                     
       ++INCLUDE ACCLBCOLS                                                      
         SPACE 1                                                                
CLB42    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB42+(((*-CLB42)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159ACCLB42B  12/23/99'                                      
         END                                                                    
