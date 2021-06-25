*          DATA SET ACCLB42    AT LEVEL 064 AS OF 08/16/00                      
*PHASE T62142A                                                                  
CLB42    TITLE '- BILL PROGRAM OPTION ROUTINES'                                 
CLB42    CSECT                                                                  
         PRINT NOGEN                                                            
ROUT     NMOD1 VOWORKL,**CB42**,CLEAR=YES,R8                                    
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETTRN              SET TRANSACTION LIST VALUES                  
         B     VALOPT              VALIDATE OPTIONS                             
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
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
SETTRN   MVC   STPARMS,0(R1)                                                    
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
         B     ROUTE                                                            
         CLI   STPACT,STPASET      TEST SET UP TLST                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,STPAPROR         GET A(USERS PRO-RATA BLOCK)                  
         XC    TLNUM,TLNUM                                                      
         BAS   RE,SETDIR                                                        
         L     R2,STPATRN2                                                      
         BAS   RE,SETMST                                                        
         BAS   RE,REFMST                                                        
         B     ROUTE                                                            
*                                                                               
FLTDIR   DS    0H                  ** DIRECTORY RECORD **                       
         CLC   TRNKWORK,TRNBILL    IGNORE BILLS                                 
         BE    ROUTL                                                            
         TM    CUSTAT,CUSDDS       DDS REVERSALS DEALT WITH BELOW               
         BO    *+12                                                             
         TM    TRNKSTAT,TRNSREVS IGNORE REVERSALS                               
         BO    ROUTL                                                            
         TM    TRNKSTA2,TRNSEXCL   TEST EXCLUDED FROM REPORTS                   
         BO    ROUTL                                                            
         TM    MIXLIND1,MIXLIDRA   TEST INCLUDING DRAFTS                        
         BO    FLTD001                                                          
         CLI   TRNKSTYP,99         IGNORE NON-99 DRAFTS                         
         BE    FLTD001                                                          
         TM    TRNKSTAT,TRNSDRFT                                                
         BO    ROUTL                                                            
FLTD001  TM    LSSTAT1,LSSORDER    TEST INCLUDING ORDERS                        
         BO    *+14                                                             
         CLC   TRNKWORK,TRNORDER   NO - IGNORE THEM                             
         BE    ROUTL                                                            
*                                                                               
         BAS   RE,SETDIR                                                        
*                                  WORKCODE                                     
FLTD002  OC    LSWC,LSWC                                                        
         BZ    FLTD010                                                          
         LA    RE,BNEQ             B NOT EQUAL                                  
         TM    LSWCFI,OPTNEQ                                                    
         BNO   *+8                                                              
         LA    RE,BEQ              B EQUAL                                      
         CLC   LSWCODE,TRNKWORK                                                 
         EX    RE,*+4                                                           
         NOP   ROUTL                                                            
*                                  INPUT TYPE                                   
FLTD010  OC    LSTYP,LSTYP                                                      
         BZ    FLTD020                                                          
         LA    RE,BNEQ             B NOT EQUAL                                  
         TM    LSTYPFI,OPTNEQ                                                   
         BNO   *+8                                                              
         LA    RE,BEQ              B EQUAL                                      
         CLC   LSTYPE,TRNKSTYP                                                  
         EX    RE,*+4                                                           
         NOP   ROUTL                                                            
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
         BO    ROUTL                                                            
         B     FLTD030                                                          
*                                                                               
FLTD026  TM    LSREFFI,OPTNEQ      OUTSIDE RANGE                                
         BO    FLTD030                                                          
         B     ROUTL                                                            
*                                  TRANSATION DATE                              
FLTD030  OC    LSDATE,LSDATE                                                    
         BZ    FLTD040                                                          
         CLC   TRNKDATE,LSDATST                                                 
         BL    FLTD032             OUTSIDE OF RANGE                             
         CLC   TRNKDATE,LSDATEN                                                 
         BH    FLTD032             OUTSIDE OF RANGE                             
         TM    LSDATFI,OPTNEQ                                                   
         BO    ROUTL                                                            
         B     FLTD040                                                          
FLTD032  TM    LSDATFI,OPTNEQ                                                   
         BNO   ROUTL                                                            
*                                  MONTH OF ACTIVITY                            
FLTD040  OC    LSMOA,LSMOA                                                      
         BZ    FLTD050                                                          
         CLC   TRNKSMOS,LSMOAST                                                 
         BL    FLTD042             OUTSIDE OF RANGE                             
         CLC   TRNKSMOS,LSMOAEN                                                 
         BH    FLTD042             OUTSIDE OF RANGE                             
         TM    LSMOAFI,OPTNEQ                                                   
         BO    ROUTL                                                            
         B     FLTD050                                                          
FLTD042  TM    LSMOAFI,OPTNEQ                                                   
         BNO   ROUTL                                                            
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
         NOP   ROUTL                                                            
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
         NOP   ROUTL                                                            
*                                                                               
FLTD070  DS    0H                                                               
*                                                                               
FLTD500  BAS   RE,SETDIR           * SET VALUES IN TSAR LIST RECORD *           
*                                                                               
FLTDIRY  B     ROUTE                                                            
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
FLTM006  GOTO1 AGETOPT,BODMCB,(X'80',TRNRECD)                                   
         MVC   LSGOJOB,TRNKACT                                                  
         L     RF,AGOPBLK                                                       
         MVC   LSGOWC,GOSELWC-GOBLOCK(RF)                                       
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FLTM008  XR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         TM    MIXLIND1,MIXLICUR   TEST MATCH ON BILLING CURRENCY               
         BZ    FLTM010                                                          
         CLC   PG$BLCUR,BCSPACES   TEST ANY PRIOR ACTIVITY                      
         BNH   FLTM010                                                          
         CLC   CSBILCUR,PG$BLCUR   TEST CURRENCY OF PRIOR ACTIVITY              
         BE    FLTM010             MATCHES - OK                                 
         CP    PA$NETBL,BCPZERO    IF PRIOR BILLING COMES TO ZERO OK            
         BNE   ROUTL                                                            
         CP    PA$COMBL,BCPZERO                                                 
         BNE   ROUTL                                                            
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
         BO    ROUTL                                                            
         B     FLTM014                                                          
*                                                                               
         USING TRSELD,R1                                                        
FLTM018  OC    LSACTDAT,LSACTDAT   TEST ACTIVITY DATE FILTER SET                
         BZ    FLTM014                                                          
         CLC   TRSDATE,LSACTDST    TEST BEFORE START DATE                       
         BL    ROUTL                                                            
         CLC   TRSDATE,LSACTDND    TEST AFTER END DATE                          
         BH    ROUTL                                                            
         B     FLTM014                                                          
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
         NOP   ROUTL                                                            
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
         B     ROUTL                                                            
         TM    TRNSTAT,TRNSAUTH                                                 
         BNO   FLTM040                                                          
         B     ROUTL                                                            
*                                  HELD STATUS                                  
FLTM040  CLI   LSHELD,0                                                         
         BE    FLTM050                                                          
         CLI   LSHELD,C'Y'         TAKE HELD AND UNHELD                         
         BE    FLTM050                                                          
         CLI   LSHELD,C'O'         TAKE HELD TRANSACTIONS ONLY                  
         BNE   *+16                                                             
         TM    TRNSTAT,TRNSHOLD                                                 
         BO    FLTM050                                                          
         B     ROUTL                                                            
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   FLTM050                                                          
         B     ROUTL                                                            
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
         BO    ROUTL                                                            
         B     FLTM060                                                          
*                                                                               
FLTM052  TM    LSBATFI,OPTNEQ      OUTSIDE RANGE                                
         BO    FLTM060                                                          
         B     ROUTL                                                            
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
         B     ROUTL                                                            
*                                  ALLOC=Y/N/O/P                                
FLTM070  OC    LSALLOC,LSALLOC                                                  
         BZ    FLTM080                                                          
         CLI   LSALLOC,C'Y'                                                     
         BE    FLTM080             TAKE ALL                                     
*                                                                               
         CLI   LSALLOC,C'P'        TEST PARTIAL ALLOCATIONS ONLY                
         BNE   FLTM072                                                          
         CP    PP$AALLO,BCPZERO    DON'T WANT ZERO ALLOCATIONS                  
         BE    ROUTL                                                            
         CP    PM$ANVBL,BCPZERO    OR FULL ALLOCATIONS                          
         BE    ROUTL                                                            
         B     FLTM080                                                          
*                                                                               
FLTM072  LA    RE,BNZQ             BNZ                                          
         CLI   LSALLOC,C'N'                                                     
         BE    *+8                                                              
         LA    RE,BZQ              BZ                                           
         CP    PP$AALLO,BCPZERO                                                 
         EX    RE,*+4                                                           
         NOP   ROUTL                                                            
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
         CP    PA$NETBL,BCPZERO    YES - AVAILABLE IF NOTHING BILLED            
         BE    FLTM086                                                          
         B     FLTM085                                                          
FLTM084  CP    0(L'PM$ANVBL,RF),BCPZERO                                         
         BNE   FLTM086                                                          
FLTM085  BCTR  RE,0                RE=-1 IF UNAVAILABLE FOR PENDING             
*                                                                               
FLTM086  CLI   LSUNAV,C'N'         EXCLUDE UNAVAILABLES                         
         BNE   FLTM088                                                          
         LTR   RE,RE                                                            
         BNZ   ROUTL                                                            
         B     FLTM090                                                          
FLTM088  LTR   RE,RE               EXCLUDE AVAILABLES                           
         BZ    ROUTL                                                            
*                                                                               
FLTM090  DS    0H                                                               
*&&US                              WCTYPE = O/P/R/T                             
         CLI   LSWCTCOD,0                                                       
         BE    FLTM100                                                          
         L     R1,AGOPBLK                                                       
         USING GOBLOCKD,R1                                                      
         LA    RE,BEQ                                                           
         TM    LSWCTFI,OPTNEQ      TEST NEGATIVE FILTER                         
         BO    *+8                                                              
         LA    RE,BNEQ                                                          
         CLC   LSWCTCOD,GOWRKTY                                                 
         EX    RE,*+4                                                           
         NOP   ROUTL                                                            
         DROP  R1                                                               
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
         BZ    ROUTL                                                            
         B     FLTM510                                                          
FLTM502  TM    TLXSTAT,TLXSHOUR    EXCLUDE LABOR                                
         BO    ROUTL                                                            
*                                                                               
FLTM510  DS    0H                                                               
*                                                                               
         B     ROUTE                                                            
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
SETDIRX  B     ROUTX                                                            
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
         GOTO1 AGETWCD,BCPARM,((RF),TRNKWORK)                                   
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R1,0(R1)                                                         
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
         TM    LSSTAT1,LSSORDER    TEST LISTING ORDERS                          
         BZ    SMST30                                                           
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
SETMSTX  B     ROUTX                                                            
         SPACE 1                                                                
***********************************************************************         
* REFRESH TLSTD VALUES FROM MASTER RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
REFMST   NTR1  ,                                                                
         CLI   TRNRSTYP,99         TEST ADVANCE BILLING ITEM                    
         BNE   RMST02                                                           
         NI    TLXVAL,FF-TLXVDEL                                                
         CP    PA$GRSBL,BCPZERO    TEST BILLING UPDATED                         
         BNE   RMST02                                                           
         OI    TLXVAL,TLXVDEL       NO - ITEM CAN BE DELETED                    
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
REFMSTX  B     ROUTX                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
STWORKD  DSECT                     * SETTRN W/S *                               
STPARMS  DS    0XL12               INPUT PARAMETERS                             
STPACT   DS    XL1                 ACTION                                       
STPADIR  EQU   C'D'                ACCDIR RECORD                                
STPAMST  EQU   C'M'                ACCMST RECORD                                
STPAREF  EQU   C'R'                REFRESH TLSTD                                
STPASET  EQU   C'S'                SET UP TLSTD                                 
STPATRN  DS    AL3                 A(1ST TRANSACTION RECORD)                    
STPATRN2 DS    A                   A(2ND TRANSACTION RECORD)                    
STPAPROR DS    A                   A(PRO-RATA BLOCK)                            
STDUB1   DS    D                                                                
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
VALOPT   ST    R1,VOATAB                                                        
         CLI   0(R1),0             TEST R1=A(OPTION TABLE)                      
         BNE   *+10                                                             
         MVC   VOPARMS,0(R1)       OR R1=A(PARAMETER LIST)                      
*                                                                               
VALOPT01 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         TM    VOPINDS,VOPIDEFO    TEST SPECIAL DEFAULT CALL                    
         BO    VALOPT38                                                         
*                                                                               
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALOPT38                                                         
         MVC   BCPARM+08(2),=C',='                                              
         MVC   BCPARM+10(2),BCCHARS                                             
         GOTO1 VSCANNER,BCPARM,('VORHSL',FVIHDR),(X'80',VOAREA)                 
         MVC   VOAREAN(1),4(R1)    SAVE NUMBER OF LINES INPUT                   
         CLI   VOAREAN,0           TEST FOR INVALID INPUT                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTX                                                          
         MVI   FVINDX,1            SET FIRST FIELD INDEX NUMBER                 
         LA    R3,VOAREA           R3=A(SCANNER TABLE)                          
         B     VALOPT04                                                         
*                                  BUMP TO NEXT BLOCK ENTRY                     
VALOPT02 SR    R1,R1                                                            
         IC    R1,FVINDX           BUMP MULTIPLE FIELD INDEX                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,VOAREAN        TEST ALL OPTIONS PROCESSED                   
         BH    VALOPT38                                                         
         STC   R1,FVINDX                                                        
         LA    R3,VOWDTH(R3)                                                    
*                                                                               
VALOPT04 CLI   0(R3),0             TEST VALID KEYWORD LENGTH                    
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
VALOPT06 BAS   RE,NXTOPT                                                        
         BE    VALOPT10                                                         
         TM    BCINDS2,BCINTRS     TEST USER JUST NTRSES'D                      
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKINV) EOT - INVALID KEYWORD                      
         B     VALOPTEL                                                         
*                                                                               
         MVI   VOGEN,C' '          REMOVE OFFENDING OPTION FROM INPUT           
         MVC   VOGEN+1(L'VOGEN-1),VOGEN                                         
         L     R1,FVADDR                                                        
         USING FHD,R1                                                           
         XR    R2,R2                                                            
         IC    R2,FHLN                                                          
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   VOGEN(0),FHDA                                                    
         XR    RE,RE                                                            
         ICM   RE,1,4(R3)          RE=START OF OPTION                           
         LA    RE,VOGEN(RE)                                                     
         XR    RF,RF                                                            
         ICM   RF,1,VOWDTH+4(R3)   RF=END OF OPTION                             
         BNZ   *+8                                                              
         LR    RF,R2                                                            
         BCTR  RE,0                                                             
         LA    RF,VOGEN(RF)                                                     
         MVC   0(L'FVIFLD,RE),0(RF)                                             
         EX    R2,*+4                                                           
         MVC   FHDA(0),VOGEN                                                    
         OI    FHOI,FHOITR                                                      
         NI    FHII,FF-FHIIVA                                                   
         GOTO1 AFVAL                                                            
         DROP  R1                                                               
         LA    RE,VOPARMS+L'VOPARMS                                             
         LA    RF,VOWORKL-L'VOPARMS                                             
         XR    R3,R3                                                            
         MVCL  RE,R2                                                            
         B     VALOPT01            RE-VALIDATE OPTIONS                          
*                                                                               
VALOPT10 SR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(OPTNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,OPTSHRT                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK+OPTNAMLQ(OPTSHTLQ),0(RE)                                  
         LA    RE,1(R1)                                                         
         CLM   RE,1,OPTMINKL       TEST L'INPUT LESS THAN MIN ALLOWED           
         BL    VALOPT06                                                         
         EX    R1,*+8                                                           
         BE    VALOPT16                                                         
         CLC   BCWORK(0),VOHDRL(R3) MATCH ON FULL OPTION NAME                   
         CLI   0(R3),OPTSHTLQ      TEST > SHORT KEYWORD                         
         BH    VALOPT06                                                         
         EX    R1,*+8                                                           
         BNE   VALOPT06                                                         
         CLC   BCWORK+OPTNAMLQ(0),VOHDRL(R3)                                    
*                                                                               
VALOPT16 GOTO1 TESTBIT,BCPARM,(OPTOPTN,VOMASK)                                  
         BZ    *+14                CHECK FOR DUPLICATED OPTION KEYWORD          
         MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     VALOPTEL                                                         
         GOTO1 SETBIT,BCPARM,(OPTOPTN,VOMASK) SET BIT POSITION                  
         MVI   4(R3),0             CLEAR SPECIAL CHARACTERS INDICATOR           
         CLI   VOLHSL(R3),C'<'     TEST LESS THAN CHARACTER                     
         BNE   *+8                                                              
         OI    4(R3),OPTLEQ                                                     
         CLI   VOLHSL(R3),C'>'     TEST GREATER THAN CHARACTER                  
         BNE   *+8                                                              
         OI    4(R3),OPTGEQ                                                     
         TM    OPTINDS2,OPTNEQ     TEST NOT EQUALS CHARACTER IS SPECIAL         
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'*'     TEST NOT EQUALS CHARACTER                    
         BNE   *+8                                                              
         OI    4(R3),OPTNEQ                                                     
         TM    OPTINDS2,OPTPLQ     TEST PLUS CHARACTER IS SPECIAL               
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'+'     TEST PLUS CHARACTER                          
         BNE   *+8                                                              
         OI    4(R3),OPTPLQ                                                     
         TM    OPTINDS2,OPTMIQ     TEST MINUS CHARACTER IS SPECIAL              
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'-'     TEST MINUS CHARACTER                         
         BNE   *+8                                                              
         OI    4(R3),OPTMIQ                                                     
         ICM   RE,1,4(R3)          TEST SPECIAL CHARACTER INPUT                 
         BZ    VALOPT18                                                         
         CLI   1(R3),1             TEST DATA INPUT AFTER SPECIAL                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTER                                                         
         EX    RE,*+8                                                           
         BNZ   VALOPT18                                                         
         TM    OPTINDS2,0          TEST SPECIAL CHARACTER ALLOWED               
         MVC   FVMSGNO,=AL2(FVFIGLE) NO - INVALID                               
         B     VALOPTER                                                         
VALOPT18 SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST DATA INPUT AFTER EQUALS SIGN            
         BNZ   VALOPT20                                                         
         CLI   OPTMINDL,0          NO - TEST IF THAT'S OK                       
         BE    VALOPT20                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         MVI   FVSUBX,1                                                         
         B     VALOPTER                                                         
*                                                                               
VALOPT20 MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         LTR   RF,RF               TEST ZERO LENGTH FIELD                       
         BZ    VALOPT22                                                         
         BCTR  RF,0                                                             
         LA    R1,VOLHSL(R3)                                                    
         TM    4(R3),FF-(OPTMIQ)                                                
         BZ    *+10                                                             
         BCTR  RF,0                DROP FIRST CHARACTER (UNLESS MINUS)          
         LA    R1,1(R1)                                                         
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(R1)                                                  
VALOPT22 XC    FVIHDR,FVIHDR                                                    
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
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,3,OPTIADDR                                                    
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION RTN                
         BZ    VALOPT26                                                         
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         CLI   OPTIADDR,FF         TEST GENERAL ROUTINE                         
         BE    *+12                                                             
         ICM   RF,7,AOVERVAL+1                                                  
         B     VALOPT30                                                         
         LA    RE,VALGEN                                                        
         AR    RF,RE                                                            
         B     VALOPT30                                                         
*                                                                               
VALOPT26 TM    OPTINDS,OPTATAB                                                  
         BZ    VALOPT28                                                         
         A     RF,AOVERVAL                                                      
         B     VALOPT32                                                         
*                                                                               
VALOPT28 TM    OPTINDS,OPTARTN     TEST A(VALIDATION ROUTINE)                   
         BNZ   *+6                                                              
         DC    H'0'                NO - TABLE IS SCREWY                         
         A     RF,AOVERVAL                                                      
*                                                                               
VALOPT30 XC    BCWORK,BCWORK                                                    
         GOTO1 (RF),OPTTABD        GO TO ROUTINE                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALOPT36                                                         
         MVI   FVFLAG,X'01'        INDICATE A USER ERROR MESSAGE                
         BNE   VALOPTER                                                         
*                                                                               
VALOPT32 SR    R0,R0                                                            
         IC    R0,0(RF)            R0=L'LHS OF TABLE                            
         SR    R1,R1                                                            
         IC    R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'TABLE                                   
         SR    RE,RE                                                            
         IC    RE,FVXLEN           RE=L'DATA-1                                  
VALOPT34 CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDINV) YES - INVALID DATA VALUE                   
         B     VALOPTER                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT34                                                         
         AR    RF,R0               EXTRACT RHS OF TABLE INTO WORK               
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BCWORK(0),0(RF)                                                  
*                                                                               
VALOPT36 BAS   RE,SETOPT           MOVE DATA TO OPTION AREA                     
         B     VALOPT02                                                         
*                                                                               
VALOPT38 MVI   FVINDX,0            RESET INDEX/SUB-INDEX                        
         MVI   FVSUBX,0                                                         
         XR    R2,R2               TEST ALL REQUIRED OPTIONS WERE INPUT         
*                                  APPLY ANY DEFAULT OPTION VALUES              
VALOPT40 BAS   RE,NXTOPT           TEST E-O-T                                   
         BNE   VALOPTX                                                          
         GOTO1 TESTBIT,BCPARM,(OPTOPTN,VOMASK)                                  
         BNZ   VALOPT40            TEST OPTION NOT USED                         
         TM    OPTINDS,OPTDFLTO+OPTDFLTI                                        
         BNZ   VALOPT42            TEST OPTION HAS DEFAULT VALUE                
         TM    OPTINDS,OPTREQD     TEST OPTION REQUIRED                         
         BZ    VALOPT40                                                         
         MVC   FVMSGNO,=AL2(FVFREQD)                                            
         XR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,TWAD(RE)                                                      
         MVC   FVXTRA(OPTNAMLQ),0(RE)                                           
         B     VALOPTX                                                          
*                                                                               
VALOPT42 TM    OPTINDS,OPTDFLTO    TEST DEFAULT IS OUTPUT VALUE                 
         BZ    VALOPT44                                                         
         MVC   BCWORK(L'OPTDVAL),OPTDVAL                                        
         B     VALOPT46                                                         
VALOPT44 MVC   FVMSGNO,=AL2(FVFOK)                                              
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
VALOPT46 BAS   RE,SETOPT           SET DEFAULT OPTION VALUE                     
         B     VALOPT40                                                         
*                                                                               
VALOPTER ICM   RF,1,8(R3)          ERROR ON RHS - PUT CURSOR ON DATA            
         BNZ   *+8                                                              
VALOPTEL IC    RF,4(R3)            ERROR ON LHS - PUT CURSOR ON KEYWORD         
         IC    RE,FVERRNDX                                                      
         AR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
VALOPTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO MOVE OPTION VALUE FROM WORK INTO OPTION/FORMAT AREA      *         
*                                                                     *         
* NTRY - R2=A(OPTION TABLE ENTRY)                                     *         
*        R3=A(SCANNER BLOCK ENTRY)                                    *         
*        SCWORK=OPTION VALUE TO BE SET                                *         
***********************************************************************         
         SPACE 1                                                                
SETOPT   SR    RF,RF                                                            
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
         BZ    *+14                                                             
         MVC   0(1,RF),4(R3)       MOVE DATA QUALIFIER                          
         LA    RF,1(RF)                                                         
         TM    OPTINDS,OPTBOOL     TEST IF A BIT VALUE                          
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
         LA    R2,TRNOPT                                                        
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
NXTOPT10 OC    OPTFILT,OPTFILT                                                  
         BZ    NXTOPT12                                                         
         MVC   BCWORK(2),OPTFILT                                                
         NC    BCWORK(2),MIXLOPTF                                               
         BZ    NXTOPT18                                                         
*                                                                               
NXTOPT12 CLI   OPTSECN,0           TEST SECURITY NUMBER GIVEN                   
         BE    NXTOPTY                                                          
         GOTO1 VSECRET,BCPARM,('SECPOPTP',ASECBLK),OPTSECN                      
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
VOWORKD  DSECT                     ** VALOPT S/R LOCAL W/S **                   
VOPARMS  DS    0XL12                                                            
VOATAB   DS    A                   A(OPTIONS VALIDATION TABLE)                  
VOPINDS  DS    XL1                 INDICATOR BYTE                               
VOPINOPE EQU   X'80'               NO OPEN COLUMNS ALLOWED                      
VOPIDEFO EQU   X'40'               GET DEFAULT VALUES ONLY                      
VOPADEF  DS    AL3                 A(DEFAULT COLUMN LIST)                       
VOPANOPL DS    A                   A(NON-OPEN PAIR LIST)                        
         ORG   VOPARMS+L'VOPARMS                                                
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
VOGEN    DS    XL256               FOR GENERAL VALIDATION ROUTINES              
         DS    0D                                                               
VOWORKL  EQU   *-VOWORKD                                                        
CLB42    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL VALIDATION ROUTINES                                         *         
***********************************************************************         
         SPACE 1                                                                
VALGEN   NTR1  ,                                                                
         XC    VOGEN,VOGEN                                                      
         LR    R4,R1                                                            
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALDIS              DIS=LIST                                     
         B     VALYNO              YES/NO/ONLY                                  
         B     VALYN               YES/NO                                       
         B     VALWC               WORKCODE FILTER                              
         B     VALTYP              INPUT TYPE                                   
         B     VALSTR              STRING TEXT RANGE                            
         B     VALDAT              DATE FILTER                                  
         B     VALMOA              MOA RANGE                                    
         B     VALAMT              AMOUNT INPUT                                 
         B     VALCMO              COMMISSION OVERRIDE RATE                     
         B     VALNY               NO/YES (YES=NO AND NO=YES)                   
         B     VALWCTYP            WCTYPE=O/P/R/T                               
         B     VALYNOP             ALLOC=Y/N/O/P                                
         B     VALPERD             PERIOD=(START)(-)(END)                       
         B     VALTXT              TEXT (INPUT=OUTPUT)                          
         B     VALWTOT             WCTOTAL=Y/N (OR SET DEFAULT)                 
*                                                                               
VALGENX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE DIS=COLUMN LIST (+=DEFAULT LIST, ++=ALL COLUMNS)           *         
***********************************************************************         
         SPACE 1                                                                
VALDIS   TM    VOPINDS,VOPIDEFO    DON'T BOTHER IF 'DEFAULT' CALL               
         BO    VALGENX                                                          
         LA    R2,VDDISLST         R2=A(COLUMN LIST)                            
         LA    R3,FVIFLD           R3=A(INPUT)                                  
         XR    R0,R0                                                            
         IC    R0,FVILEN           R0=L(INPUT)                                  
*                                                                               
VDIS02   NI    VDINDS,FF-VDIINP                                                 
         CLI   0(R3),C'&&'         TEST IF OPEN COLUMN REQUIRED                 
         BNE   VDIS04                                                           
         OI    VDINDS,VDIINP                                                    
         BCT   R0,*+14             ENSURE COLUMN CODE FOLLOWS                   
         MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     VDISNO                                                           
         LA    R3,1(R3)                                                         
         B     VDIS06                                                           
*                                                                               
VDIS04   CLI   0(R3),C'+'                                                       
         BNE   VDIS06                                                           
         MVI   0(R2),LSDISDEF      '+' = DEFAULT COLUMNS                        
         LR    RE,R2                                                            
         BCTR  RE,0                                                             
         CLI   0(RE),LSDISDEF      '++' = ALL COLUMNS                           
         BNE   *+8                                                              
         MVI   0(R2),LSDISALL                                                   
         B     VDIS18                                                           
*                                                                               
VDIS06   GOTO1 ASETCLM,BODMCB,(R3)                                              
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     VDISNO                                                           
         BH    VDIS08              OKAY TO READ ONLY                            
         TM    VOPINDS,VOPINOPE    TEST NO-OPENS ALLOWED                        
         BO    VDIS08                                                           
         L     RF,ACLMDATA         TEST DEFAULT IS OPEN FIELD                   
         TM    CLMINDS2-CLMTABD(RF),CLMIDEFO                                    
         BZ    *+8                                                              
         OI    VDINDS,VDIINP                                                    
         B     VDIS10                                                           
*                                                                               
VDIS08   TM    VDINDS,VDIINP       TEST OPEN FIELD REQUIRED                     
         BZ    VDIS10              NO - OKAY                                    
         MVC   FVMSGNO,=AL2(AE$CNVIN)                                           
         B     VDISNO                                                           
*                                                                               
VDIS10   TM    VDINDS,VDIINP       SET COLUMN OPEN IF REQUIRED                  
         BZ    *+8                                                              
         NI    0(R3),FF-LSDISPRO                                                
*                                                                               
         GOTO1 TSTDUP,(R3)         TEST FOR DUPLICATIONS                        
         BE    VDIS12                                                           
         MVC   FVMSGNO,VDMSGNO                                                  
         BL    VDISNO                                                           
         BH    VDISNO02                                                         
VDIS12   MVC   0(1,R2),0(R3)       COPY FIELD                                   
*                                                                               
VDIS18   LA    R3,1(R3)            NEXT INPUT                                   
         LA    R2,1(R2)            NEXT OUTPUT                                  
         BCT   R0,VDIS02                                                        
         MVI   0(R2),EOT                                                        
*                                                                               
         LA    R2,VDDISLST         R2=A(COLUMN LIST)                            
VDIS22   CLI   0(R2),EOT           TEST END-OF-LIST                             
         BE    VDIS60                                                           
*                                                                               
         CLI   0(R2),LSDISDEF      TEST DEFAULT LIST                            
         BNE   VDIS40                                                           
         MVC   0(L'VDDIS,R2),1(R2) DELETE COLUMN                                
         XR    R3,R3                                                            
         ICM   R3,7,VOPADEF        R3=A(DEFAULT COLUMNS LIST)                   
VDIS32   CLI   0(R3),EOT           TEST END-OF-LIST                             
         BE    VDIS22                                                           
         MVC   VDCLM,0(R3)                                                      
         GOTO1 ASETCLM,BCPARM,(R3) TEST COLUMN ALLOWED                          
         BL    VDIS38                                                           
         BNH   *+12                                                             
         OI    VDCLM,LSDISPRO      ONLY PROTECTED ALLOWED                       
         B     VDIS34                                                           
         L     RF,ACLMDATA         TEST DEFAULT IS OPEN                         
         TM    CLMINDS2-CLMTABD(RF),CLMIDEFO                                    
         BZ    *+8                                                              
         NI    VDCLM,FF-LSDISPRO                                                
VDIS34   GOTO1 TSTDUP,VDCLM                                                     
         BL    VDIS38              CC=LOW IF COLUMN ALREADY IN LIST             
         BNH   *+8                                                              
         OI    VDCLM,LSDISPRO      CC=HIGH IF COLUMN MAY NOT BE OPEN            
         MVC   VDDIS2,0(R2)        INSERT COLUMN                                
         MVC   0(L'VDCLM,R2),VDCLM                                              
         MVC   1(L'VDDIS,R2),VDDIS2                                             
         LA    R2,1(R2)                                                         
VDIS38   LA    R3,1(R3)                                                         
         B     VDIS32                                                           
*                                                                               
VDIS40   CLI   0(R2),LSDISALL                                                   
         BNE   VDIS58                                                           
         MVC   0(L'VDDIS,R2),1(R2) DELETE COLUMN                                
*                                                                               
         XC    ACLMDATA,ACLMDATA   CLEAR TO GET 1ST COLUMN                      
VDIS42   GOTO1 ASETCLM,0           GET NEXT VALIDE ENTRY                        
         BL    VDIS22                                                           
         L     R3,ACLMDATA                                                      
         USING CLMTABD,R3          R3=A(COLUMN DATA ENTRY)                      
         MVC   VDCLM,CLMCHAR                                                    
         BH    *+8                                                              
         NI    VDCLM,FF-LSDISPRO   OPEN COLUMN IF CAN BE                        
         TM    CLMINDS1,CLMIKEY    IGNORE KEY TYPE COLUMNS                      
         BO    VDIS42                                                           
         TM    CLMINDS2,CLMIDEFO   DEST DEFAULT IS OPEN                         
         BO    *+8                                                              
         OI    VDCLM,LSDISPRO      NO - CLOSE IT                                
         GOTO1 TSTDUP,VDCLM                                                     
         BL    VDIS42              CC=LOW IF COLUMN ALREADY IN LIST             
         BNH   *+8                                                              
         OI    VDCLM,LSDISPRO      CC=HIGH IF COLUMN MAY NOT BE OPEN            
         MVC   VDDIS2,0(R2)        INSERT COLUMN                                
         MVC   0(L'VDCLM,R2),VDCLM                                              
         MVC   1(L'VDDIS,R2),VDDIS2                                             
         LA    R2,1(R2)                                                         
         B     VDIS42              BUMP R3 TO NEXT COLUMN                       
         DROP  R3                                                               
*                                                                               
VDIS58   LA    R2,1(R2)            BUMP R2 TO NEXT ENTRY IN LIST                
         B     VDIS22                                                           
*                                                                               
VDIS60   LA    RF,VDDISLST                                                      
         SR    R2,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE AT LEAST 1 COLUMN                    
         STC   R2,VDDISN           SET NUMBER OF COLUMNS                        
         CLI   VDDISN,L'VDDISLST   TEST COLUMN LIST OVERFLOW                    
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   BCWORK,VDDIS                                                     
         B     VALGENX                                                          
*                                                                               
VDISNO   MVC   FVXTRA(1),0(R3)     INVALID COLUMN                               
VDISNO02 LA    R0,FVIFLD                                                        
         SR    R3,R0                                                            
         STC   R3,FVERRNDX                                                      
         B     VALGENX                                                          
         SPACE 1                                                                
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
         OI    VDDUP1,LSDISPRO                                                  
         LA    R2,VDDISLST         R2=A(LIST OF CURRENT CODES)                  
TDUP02   CLI   0(R2),EOT                                                        
         BE    TDUP10                                                           
         MVC   VDDUP2,0(R2)                                                     
         OI    VDDUP2,LSDISPRO                                                  
         CLC   VDDUP1,VDDUP2       TEST DUPLICATION                             
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     TDUP02                                                           
         MVC   VDMSGNO,=AL2(AE$DUPCM)                                           
         B     TSTDUPL                                                          
*                                                                               
TDUP10   TM    0(R1),LSDISPRO      TEST TESTED COLUMN IS OPEN                   
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
TDUP14   NI    VDDUP2,FF-LSDISPRO                                               
         LA    R2,VDDISLST                                                      
TDUP16   CLI   0(R2),EOT           SEARCH FOR OPEN OTHER COLUMN IN LIST         
         BE    TDUP18                                                           
         CLC   VDDUP2,0(R2)                                                     
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     TDUP16                                                           
         MVC   VDMSGNO,NOPMSGNO    SET MESSAGE NUMBER                           
         B     TSTDUPH                                                          
*                                                                               
TDUP18   LA    RF,NOPLSTL(RF)                                                   
         B     TDUP12                                                           
         DROP  RF                                                               
*                                                                               
TSTDUPY  CR    RE,RE               SET CC=EQUAL                                 
         B     VALGENX                                                          
TSTDUPL  CLI   *,FF                SET CC=LOW                                   
         B     VALGENX                                                          
TSTDUPH  CLI   *,0                 SET CC=HIGH                                  
         B     VALGENX                                                          
         SPACE 1                                                                
VOWORKD  DSECT                     ** VALDIS LOCAL W/S **                       
         ORG   VOGEN                                                            
*                                                                               
VDINDS   DS    XL1                 INDICATOR BYTE                               
VDIINP   EQU   X'80'               INPUT REQUIRED                               
*                                                                               
VDCLM    DS    XL1                 COLUMN CODE                                  
VDDUP1   DS    XL1                 BYTE 1 FOR DUPLICATION TEST                  
VDDUP2   DS    XL1                 BYT2 2 FOR DUPLICATION TEST                  
VDMSGNO  DS    XL2                 ERROR MESSAGE NUMBER                         
*                                                                               
VDDIS    DS    0XL(L'LSDIS)        COPY OF LSDIS                                
VDDISN   DS    XL1                                                              
VDDISLST DS    XL(L'LSDISLST)                                                   
VDDIS2   DS    XL(L'LSDIS)         EXTRA SAVE AREA                              
         ORG   VOGEN+L'VOGEN                                                    
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
VALWTOT  CLI   FVIFLD,OPTDEFQ      TEST SETTING DEFAULT                         
         BNE   VALYN               NO - NORMAL YES/NO                           
         MVI   BCWORK,C'Y'                                                      
*&&US*&& B     VALGENX                                                          
*&&UK                                                                           
         CLI   CUCTRY,CTRYHOL                                                   
         BE    VALGENX                                                          
         MVI   BCWORK,C'N'                                                      
         B     VALGENX                                                          
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE Y/N/O/P OPTIONS                                            *         
***********************************************************************         
         SPACE 1                                                                
VALYNOP  SR    RF,RF               TEST PARTIAL                                 
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'P'                                                      
         LH    RE,=Y(UC@PART-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)     NO - TEST Y/N/O                              
         B     VALYNO02                                                         
         SPACE 1                                                                
***********************************************************************         
* VALIDATE Y/N/O OPTIONS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALYNO   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
*                                                                               
VALYNO02 MVI   BCWORK,C'Y'         TEST YES                                     
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'N'         TEST NO                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'O'         TEST ONLY                                    
         LH    RE,=Y(UC@ONLY-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
VALYNO2  MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE Y/N OPTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
VALYN    SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'Y'                                                      
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'N'                                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKCODE FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALWC    XC    BCWORK(L'LSWC),BCWORK                                            
         CLI   FVILEN,L'LSWCODE                                                 
         BH    *+14                                                             
         MVC   BCWORK+L'LSWCFI(L'LSWCODE),FVIFLD                                
         B     VALWC02                                                          
*                                                                               
         CLI   FVIFLD,C'*'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALGENX                                                          
         OI    BCWORK,OPTNEQ                                                    
         MVC   BCWORK+L'LSWCFI(L'LSWCODE),FVIFLD+1                              
*                                                                               
VALWC02  CLC   =C'**',BCWORK+L'LSWCFI                                           
         BE    VALGENX             ORDER W/C IS VALID                           
         CLC   =C'99',BCWORK+L'LSWCFI                                           
         BE    VALGENX             BILL W/C IS VALID                            
         PUSH  USING                                                            
         USING WCORECD,IOKEY       READ WORKCODE RECORD                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),BCCPYPRD                            
         MVC   WCOKWRK,BCWORK+L'LSWCFI                                          
         GOTO1 AIO,IORD+IOACCDIR+IO1                                            
         BE    VALGENX                                                          
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     VALGENX                                                          
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT TYPE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALTYP   MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         TM    FVIIND,FVINUM       TEST INPUT IS NUMERIC                        
         BZ    VALGENX                                                          
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         MVC   FVMSGNO,=AL2(AE$IVTYP)                                           
         CP    BODUB1,=P'250'                                                   
         BH    VALGENX                                                          
         CVB   R0,BODUB1                                                        
         STC   R0,BCWORK                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE STRING TEXT RANGE                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSTR   SR    R6,R6                                                            
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
         B     VALGENX                                                          
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
         BE    VALGENX             NO SECOND PART                               
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
         B     VALGENX                                                          
*                                                                               
VALSTHI  MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD FOR TRANSACTION DATE                                *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(CULANG,BOWORK1)                  
         USING PERVALD,RF                                                       
         LA    RF,BOWORK1                                                       
         TM    4(R1),X'03'                                                      
         BZ    VALDAT2                                                          
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     VALGENX                                                          
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
         B     VALGENX                                                          
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE PERIOD FOR TRANSACTION MOA                                 *         
***********************************************************************         
         SPACE 1                                                                
VALMOA   GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(CULANG,BOWORK1)                  
         USING PERVALD,RF                                                       
         LA    RF,BOWORK1                                                       
         TM    4(R1),X'03'                                                      
         BNZ   VALMOA2                                                          
         TM    PVALASSM,PVALASD+PVALAED                                         
         BO    VALMOA4             START AND END DAY BOTH ASSUMED - OK          
VALMOA2  MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     VALGENX                                                          
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
         B     VALGENX                                                          
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMOUNT INPUT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         MVC   BCBYTE1,CSCURCPY+(CURTDECP-CURTABD)                              
         OI    BCBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BODMCB,(BCBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   BODMCB,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     VALGENX                                                          
         ZAP   BCWORK(L'LSAMOUNT),BODMCB+4(8)                                   
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMISSION OVERRIDE RATES                                  *         
***********************************************************************         
         SPACE 1                                                                
VALCMO   XC    VCSCNBLK(5*SCBLKLQ),VCSCNBLK                                     
         GOTO1 VSCANNER,BODMCB,FVIHDR,(5,VCSCNBLK),C',=/ '                      
         MVC   VCSCN#,4(R1)                                                     
         CLI   VCSCN#,0            TEST ANY INPUT                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     VALGENX                                                          
         LA    R3,BCWORK                                                        
         LA    R2,VCSCNBLK                                                      
         USING SCANBLKD,R2                                                      
VCM02    SR    RE,RE                                                            
         ICM   RE,1,VCSCN#                                                      
         BZ    VALGENX                                                          
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
         B     VALGENX                                                          
         MVC   0(L'WCOKWRK,R3),IO.WCOKWRK                                       
         DROP  IO                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         GOTO1 VCASHVAL,BODMCB,(X'84',SC2NDFLD),(RF)                            
         CLI   BODMCB,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     VALGENX                                                          
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
VALNY    SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         MVI   BCWORK,C'N'                                                      
         LH    RE,=Y(UC@YES-TWAD)                                               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVI   BCWORK,C'Y'                                                      
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    VALGENX                                                          
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         MVC   FVMSGNO,=AL2(AE$ICODO)                                           
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE WCTYPE=O/P/R/T                                             *         
***********************************************************************         
         SPACE 1                                                                
VALWCTYP XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BCWORK(0),FVIFLD                                                 
         CLI   FVIFLD,C'O'         OUT OF POCKET                                
         BE    VALGENX                                                          
         CLI   FVIFLD,C'P'         PREBILLING                                   
         BE    VALGENX                                                          
         CLI   FVIFLD,C'R'         RETAINER                                     
         BE    VALGENX                                                          
         CLI   FVIFLD,C'T'         TIME                                         
         BE    VALGENX                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALGENX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD=(START)(-)(END)                                     *         
***********************************************************************         
         SPACE 1                                                                
VALPERD  GOTO1 AVALPER,BOPARM,FVIHDR,0                                          
         BNE   VALGENX                                                          
         MVC   BCDUB(L'PVALCSTA),BCWORK+(PVALCSTA-PERVALD)                      
         MVC   BCDUB+L'PVALCSTA(L'PVALCEND),BCWORK+(PVALCEND-PERVALD)           
         MVC   BCWORK(L'PVALCSTA+L'PVALCEND),BCDUB                              
         B     VALGENX                                                          
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TEXT                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALTXT   MVC   BCWORK(1),FVILEN                                                 
         IC    RE,OPTOUTDL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     VALGENX                                                          
         MVC   BCWORK+1(0),FVIFLD                                               
         EJECT                                                                  
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
FF       EQU   X'FF'                                                            
ALL      EQU   0                                                                
         LTORG                                                                  
TRNORDER DC    C'**'                                                            
TRNBILL  DC    C'99'                                                            
INCUL    DC    C'SI'                                                            
EXPUL    DC    C'SE'                                                            
SKUL     DC    C'SK'                                                            
UL1R     DC    C'1R'                                                            
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
*                                  WC=XX/*XX                                    
         DC    AL2(UC8WC-TWAD,UC2WC-TWAD)                                       
         DC    AL1(OPTNRTN,0),AL2(0)                                            
         DC    AL1(0,0,0,L'LSWCODE,L'LSWC,L'LSWC)                               
         DC    AL1(255-(*-TRNOPT)/OPTTABL)                                      
         DC    AL2(OPTWCQ,LSWC-LSVALSD)                                         
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  TYPE=NN/*NN                                  
         DC    AL2(UC8TYPE-TWAD,UC3TYPE-TWAD)                                   
         DC    AL1(OPTNRTN,OPTNEQ),AL2(0)                                       
         DC    AL1(0,0,0,1,4,L'LSTYPE)                                          
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
         DC    AL1(0,0,0,L'LSWCTCOD,L'LSWCTYPE,L'LSWCTYPE)                      
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
*                                  ACTDATE=DDMMMYY-DDMMMYY                      
         DC    AL2(UC8ACTYD-TWAD,UC3ACTYD-TWAD)                                 
         DC    AL1(OPTNRTN,0),AL2(0)                                            
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
         DC    AL1(0,ACTNOT+ACTREV,0,1,4,L'LSBILD)                              
         DC    AL1((*-TRNOPT)/OPTTABL)                                          
         DC    AL2(OPTYNOQ,LSBILD-LSVALSD)                                      
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  BILLED=Y/N/O                                 
         DC    AL2(UC8BLD-TWAD,UC3BLD-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(OPTFBLDN)                            
         DC    AL1(0,ACTNOT+ACTREV,0,1,4,L'LSBILD)                              
         DC    AL1((*-TRNOPT)/OPTTABL)                                          
         DC    AL2(OPTYNOQ,LSBILD-LSVALSD)                                      
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
*                                  UNAVAILABLE=Y/N/O                            
         DC    AL2(UC@UNAV-TWAD,UC@UNAV-TWAD)                                   
         DC    AL1(OPTNRTN+OPTDFLTO,0),AL2(OPTFUNAV)                            
         DC    AL1(0,0,0,1,4,L'LSUNAV)                                          
         DC    AL1((*-TRNOPT)/OPTTABL)                                          
         DC    AL2(OPTYNOQ,LSUNAV-LSVALSD)                                      
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
TRNOPTX  DS    AL1(EOT)                                                         
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
       ++INCLUDE ACCLBWORK                                                      
* ACCLBCOLS                                                                     
       ++INCLUDE ACCLBCOLS                                                      
         SPACE 1                                                                
CLB42    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB42+(((*-CLB42)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064ACCLB42   08/16/00'                                      
         END                                                                    
