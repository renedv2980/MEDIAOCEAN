*          DATA SET RECNT73    AT LEVEL 013 AS OF 08/05/02                      
*PHASE T80273A                                                                  
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE CENTER                                                                 
         TITLE 'T80273 - MAKEGOOD FORM GENERATOR'                               
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT73 (T80273) --- MAKEGOOD FORM GENERATOR                *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* 02AUG00 JRD FIX DATE RANGE                                      *             
* 11OCT00 SKU ENABLE FAX CAPABILITY                               *             
* 02MAY00 SKU SUPPORT MULTI-MAKEGOOD                              *             
* 10NOV98 RHV CREATION DATE                                       *             
*                                                                 *             
*******************************************************************             
* INPUT: P1: (RC)                                                 *             
*                                                                 *             
* INPUT: P2: A(SELECT FIELD) CONTAINING 'P/F'                     *             
*                                                                 *             
*******************************************************************             
T80273   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T80273,R9,CLEAR=YES,RR=R3                        
         LR    R7,RC                                                            
         USING MYWORKD,R7                                                       
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         L     R2,4(R1)            POINTS TO CURRENT SELECT FIELD               
         L     R8,ASPULAR                                                       
         USING SPOOLD,R8                                                        
         ST    R8,ASPOOLD                                                       
         ST    R3,RELO                                                          
         USING TWAD,RA                                                          
*                                                                               
         L     RE,ASPULAR                                                       
         XCEF  (RE),6500                                                        
         BAS   RE,INITIAL                                                       
*                                                                               
         CLI   8(R2),C'F'                                                       
         BE    MAIN100                                                          
         CLI   8(R2),C'P'                                                       
         BE    MAIN200                                                          
         CLI   8(R2),C'!'          FOR TESTING                                  
         BE    MAIN200                                                          
         DC    H'0'                                                             
*                                                                               
MAIN100  DS    0H                  GENERATE FAXED FORM TO AGENCY                
         BAS   RE,GETAFAX                                                       
         BAS   RE,OPENFAXQ         OPEN PRTQ FOR FAXED FORM                     
         BAS   RE,EDICT                                                         
         BAS   RE,PRTFORM                                                       
*                                                                               
         MVC   WORK2(34),FOKMESS   GEN SPOOLED MSG                              
         MVC   WORK2(3),SPOOLKEY+12                                             
         LA    R1,WORK2                                                         
         EDIT  (2,SPOOLRPN),(5,4(R1)),ALIGN=LEFT                                
         GOTO1 SQUASHER,DMCB,WORK2,34                                           
         B     MAIN300                                                          
*                                                                               
MAIN200  DS    0H                  GENERATE PRINTED FORM TO PRTQ                
         BAS   RE,OPENPRTQ         OPEN PRTQ FOR PRINTED FORM                   
         MVI   LINE,1                                                           
         BAS   RE,PRTFORM                                                       
*                                                                               
         MVC   WORK2(48),OKMESS    GEN SPOOLED MSG                              
         MVC   WORK2(3),SPOOLKEY+12                                             
         LA    R1,WORK2                                                         
         EDIT  (2,SPOOLRPN),(5,4(R1)),ALIGN=LEFT                                
         EDIT  (2,SPOOLPAG),(3,34(R1)),ALIGN=LEFT                               
         EDIT  (2,SPOOLLIN),(4,44(R1)),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,WORK2,48                                           
*                                                                               
MAIN300  DS    0H                                                               
         OI    CONMSGH+6,X'80'     XMIT MESSAGE                                 
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(48),WORK2                                                 
         B     EXXMOD                                                           
**********************************************************************          
* PRINT MAKEGOOD FORM                                                           
**********************************************************************          
PRTFORM  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         XC    MGFLAGS,MGFLAGS                                                  
*                                                                               
* READ THE SELECTED MG GROUP RECORD                                             
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         OC    TWAMKGD2,TWAMKGD2                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE AN ADDRESS                         
         MVI   DMOUTBTS,0                                                       
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         CLI   DMOUTBTS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN  GROUP RECORD?                              
         BZ    *+6                                                              
         DC    H'0'                MUST BE                                      
*                                                                               
         MVC   MYGRP,RMKGKGRP      SAVE MG GROUP                                
         MVC   MYGRPST,RMKGSCST    SAVE CURRENT STATUS                          
         DROP  R6                                                               
                                                                                
***>     BAS   RE,FHEAD            PRINT HEADER                                 
*                                                                               
* DISPLAY OVERALL GROUP COMMENTS, IF ANY                                        
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTF200                                                          
         USING RMKGGCEM,R6                                                      
         MVC   P+9(8),=C'GRP CMT:'                                              
*                                                                               
PRTF100  DS    0H                                                               
         ZIC   R1,RMKGGCLN                                                      
         SHI   R1,3                OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+18(0),RMKGGCCM                                                 
         BAS   RE,GOSPOOL                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    PRTF100                                                          
         B     PRTF200                                                          
         DROP  R6                                                               
*                                                                               
PRTF150 DS     0H                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BH    PRTF160             YES - DISPLAY MISSED DATA                    
         DROP  R6                                                               
*                                                                               
         BAS   RE,MSHEAD           PRINT MISSED LINE HEADER                     
         BAS   RE,MSBUY            DISPLAY INFO FROM MISSED BUYLINE             
*                                                                               
PRTF160 DS     0H                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'                                                    
         BO    PRTF500             SKIP FOR PREEMPT/CREDIT                      
*                                                                               
* START FROM THE FIRST DETAIL RECORD IF LATE RUN                                
*                                                                               
         TM    RMKGRTS,X'08'+X'04'                                              
         BZ    PRTF190                                                          
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   KEY(27),IOAREA                                                   
         GOTO1 VHIGH                                                            
         GOTO1 VSEQ                                                             
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
PRTF190  DS    0H                                                               
         CLC   RMKGKLIN,LASTOFR    SAME OFFER AS LAST MG?                       
         BE    *+8                 YES - SKIP HEADER                            
         BAS   RE,MGHEAD                                                        
         MVC   LASTOFR,RMKGKLIN                                                 
         BAS   RE,MGFORMAT                                                      
         OI    MGFLAGS,X'80'       PROCESSING MAKEGOOD PORTION                  
***>     BAS   RE,MGDETCMT                                                      
         DROP  R4,R6                                                            
                                                                                
PRTF200  DS    0H                  ADVANCE TO NEXT MG                           
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         MVC   KEY,RMKGKEY         RESTORE SEQ LOOP                             
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE     SAME GROUP?                    
         BNE   PRTF500             NO - DONE                                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    MGFLAGS,X'80'                                                    
         BO    PRTF190                                                          
         B     PRTF150                                                          
*                                                                               
PRTF500  DS    0H                  DONE WITH GROUP                              
         BAS   RE,MGFOOT           MG FOOTER                                    
         MVI   SPMODE,X'FF'        CLOSE PQ AND EXIT                            
         BAS   RE,GOSPOOL                                                       
         B     EXXMOD                                                           
         DROP  R6                                                               
*                                                                               
* MISSED SPOT SECTION HEADER                                                    
*    IOAREA: FIRST MG IN CURRENT OFFER                                          
*                                                                               
MSHEAD   NTR1                                                                   
         MVI   P1,0                                                             
         MVC   P2,=132C'='         SEPARATOR                                    
         MVI   P3,0                                                             
         BAS   RE,GOSPOOL                                                       
         MVI   ALLOWLIN,4                                                       
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   MSHEADX             BONUS, NO MISSED DISPLAYED                   
*                                                                               
         MVC   P1+0(20),=C'*** MISSED SPOTS ***'                                
         BAS   RE,GOSPOOL                                                       
*&&DO                                                                           
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   MSH050                                                           
                                                                                
         USING RMKGCDEL,R6                                                      
         CLI   RMKGCDLN,RMKGCDDS-RMKGCDEL                                       
         BNH   MSH050                                                           
                                                                                
         ZIC   R1,RMKGCDLN                                                      
         SH    R1,=H'11'           OVERHEAD                                     
         BM    MSH050                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1+9(0),RMKGCDDS                                                 
         MVC   P1(8),=C'MSL CMT:'                                               
         BAS   RE,GOSPOOL                                                       
         DROP  R6                                                               
*&&                                                                             
*                                                                               
MSH050   DS    0H                                                               
         LA    R2,P1                                                            
         USING PMBUY,R2                                                         
         MVC   PMBLINE(4),=C'LINE'                                              
         MVC   PMBDATE(5),=C'DATES'                                             
         MVC   PMBDYTM(8),=C'DAY/TIME'                                          
         MVC   PMBNPW(3),=C'NPW '                                               
         MVC   PMBDP(2),=C'DP'                                                  
         MVC   PMBLEN(3),=C'LEN'                                                
         MVC   PMBPGM(7),=C'PROGRAM'                                            
         MVC   PMBCOST(4),=C'COST'                                              
         MVC   PMBDEMO(4),=C'DEMO'                                              
*                                                                               
         LA    R2,132(R2)                                                       
         MVC   PMBLINE,DASHES                                                   
         MVC   PMBDATE,DASHES                                                   
         MVC   PMBDYTM,DASHES                                                   
         MVC   PMBNPW,DASHES                                                    
         MVC   PMBDP,DASHES                                                     
         MVC   PMBLEN,DASHES                                                    
         MVC   PMBPGM,DASHES                                                    
         MVC   PMBCOST,DASHES                                                   
         MVC   PMBDEMO,=C'--1--2--3--4--'                                       
         DROP  R2                                                               
*                                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
MSHEADX  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* MADEGOOD SPOT HEADER                                                          
*                                                                               
MGHEAD   NTR1                                                                   
         BAS   RE,GOSPOOL                                                       
         MVI   ALLOWLIN,4                                                       
*                                                                               
         MVC   P1(19),=C'*** BONUS SPOTS ***'                                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   MGHEAD10                                                         
*                                                                               
         MVC   P1(22),=C'*** MADEGOOD SPOTS ***'                                
*                                                                               
MGHEAD10 DS    0H                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R2,P1                                                            
         USING PMG,R2                                                           
         MVC   PMGOFFR(4),=C'LINE'                                              
         MVC   PMGDATE(5),=C'DATES'                                             
         MVC   PMGDYTM(8),=C'DAY/TIME'                                          
         MVC   PMGNPW(3),=C'NPW '                                               
         MVC   PMGDP(2),=C'DP'                                                  
         MVC   PMGLEN(3),=C'LEN'                                                
         MVC   PMGPGM(7),=C'PROGRAM'                                            
         MVC   PMGCOST(4),=C'COST'                                              
         MVC   PMGDEMO(4),=C'DEMO'                                              
*                                                                               
         LA    R2,132(R2)                                                       
         MVC   PMGOFFR,DASHES                                                   
         MVC   PMGDATE,DASHES                                                   
         MVC   PMGDYTM,DASHES                                                   
         MVC   PMGNPW,DASHES                                                    
         MVC   PMGDP,DASHES                                                     
         MVC   PMGLEN,DASHES                                                    
         MVC   PMGPGM,DASHES                                                    
         MVC   PMGCOST,DASHES                                                   
         MVC   PMGDEMO,=C'--1--2--3--4--'                                       
         DROP  R2                                                               
*                                                                               
         BAS   RE,GOSPOOL                                                       
         B     EXXMOD                                                           
*                                                                               
* MAKEGOOD FORM FOOTER                                                          
*                                                                               
MGFOOT   NTR1                                                                   
         BAS   RE,GOSPOOL                                                       
         BAS   RE,GOSPOOL                                                       
         BAS   RE,GOSPOOL                                                       
         MVC   P(16),=C'BUYER SIGNATURE:'                                       
         MVC   P+17(20),=20C'_'                                                 
         BAS   RE,GOSPOOL                                                       
         B     EXXMOD                                                           
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
OPENPRTQ NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
         MVC   PLSUBID,=C'MGF'                                                  
         MVC   PLDESC,=CL11'MKG FORM'                                           
         MVI   PLCLASS,C'K'                                                     
         DROP  R3                                                               
                                                                                
         LA    RE,TWASPKEY                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVI   QLSYS,C'R'                                                       
         MVC   QLPRG,=C'CO'                                                     
         OI    QLATTB,QLATWIDE                                                  
         DROP  R4,RE                                                            
*                                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
         L     RF,=A(FHEAD)                                                     
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
         MVC   SPOOLRPN,SPOOLKEY+19                                             
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* GET AGENCY FAX NUMBER IF WE DON'T HAVE IT ALREADY                             
**********************************************************************          
GETAFAX  NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         OC    TWAAFAX,TWAAFAX                                                  
         BNZ   GAFAXX                                                           
*                                                                               
         XC    IOAREA(32),IOAREA   PHONE/FAX NUMBERS                            
         MVI   RAGK2TYP,RAGK2TYQ   AGENCY REC TYPE                              
         MVC   RAGK2AGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         LA    R3,877              NO AGENCY FAX NUMBER PROVIDED                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         OC    RAGY2FAX,RAGY2FAX                                                
         BZ    ERROR                                                            
*                                                                               
         MVC   TWAAFAX,RAGY2FAX                                                 
*                                                                               
GAFAXX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE FOR FAXING                                               
**********************************************************************          
OPENFAXQ NTR1                                                                   
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
         MVC   PLSUBID,=C'MGF'                                                  
         MVC   PLDESC,=CL11'MKG FORM'                                           
         MVI   PLCLASS,C'G'                                                     
         DROP  R3                                                               
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         LA    RE,TWASPKEY                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'XX'                                                     
         DROP  R4,RE                                                            
*                                                                               
         BAS   RE,GOSPOOL                                                       
         L     RF,=A(FHEAD)                                                     
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     EXXMOD                                                           
***********************************************************************         
* INITALIZATION                                                                 
***********************************************************************         
INITIAL  NTR1                                                                   
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
         SPACE 2                                                                
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
*                                                                               
         L     RF,=V(OUTDAY)                                                    
         A     RF,RELO                                                          
         ST    RF,OUTDAY                                                        
*                                                                               
         L     RF,=V(UNTIME)                                                    
         A     RF,RELO                                                          
         ST    RF,UNTIME                                                        
*                                                                               
         MVI   LASTOFR,0                                                        
*                                                                               
         B     EXXMOD                                                           
***********************************************************************         
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
***********************************************************************         
EDICT    NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
*                                                                               
         MVC   P+9(5),=C'FAXW '                                                 
         MVC   P+13(3),TWAAFAX                                                  
         MVI   P+16,C'-'                                                        
         MVC   P+17(3),TWAAFAX+3                                                
         MVI   P+20,C'-'                                                        
         MVC   P+21(4),TWAAFAX+6                                                
         DROP  R4                                                               
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
         MVI   P+35,C'P'         REPL X'89' IN REP W/ EASYLINK /PAGE            
         MVI   P+68,C'L'         LANDSCAPE                                      
*                                                                               
         BAS   RE,GOSPOOL                                                       
***>     BAS   RE,GOSPOOL                                                       
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R4,P                                                             
         USING EDICTD,R4                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
         MVC   EDIPROG,=C'FMG'     MAKEGOOD FORM                                
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
*                                                                               
         MVC   EDIRCNRP,RCONKREP   REP CODE                                     
         MVC   EDIRCNOF,RCONKOFF   OFF CODE                                     
         MVC   EDIRCNSP,RCONSAL    SALESPERSON CODE                             
         MVC   EDIRCNAG,RCONKAGY   AGENCY CODE                                  
         MVC   EDIRCNAO,RCONKAOF   CITY CODE                                    
         MVC   EDIRCNAD,RCONKADV   ADVERTISER CODE                              
         MVC   EDIRCNCT,RCONKCON   CONTRACT TYPE                                
         BAS   RE,GOSPOOL                                                       
***>     BAS   RE,GOSPOOL                                                       
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY INFO FROM MISSED BUYLINE                                              
*                                                                               
MSBUY    NTR1                                                                   
*                                                                               
MISB000  DS    0H                                                               
         LA    R3,P                                                             
         USING PMBUY,R3                                                         
*                                                                               
         XC    MKGDMSDT,MKGDMSDT   CLEAR POINTERS TO MISSED DATE                
*                                  ORDER LATER                                  
MGD      USING RMKGREC,IOAREA                                                   
         TM    MGD.RMKGRTS,X'24'                                                
         BO    MISSBUYX                                                         
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   MISSBUYX                                                         
*                                                                               
MISB005  DS    0H                                                               
         XC    MKGDMSDY,MKGDMSDY   CLEAR POINTER TO DAY/TIME ELEMENTS           
*                                                                               
         XC    KEY,KEY             CONSTRUCT TARGET MISSED BUYLINE KEY          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,MGD.RMKGKCON                                       
         MVC   BUYD.RBUYKPLN,MGD.RMKGKPLN                                       
         GOTO1 VHIGH                                                            
         DROP  MGD                                                              
*                                                                               
MISB010  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BNE   MISSBUYX                                                         
*                                                                               
         USING RMKGMGEL,R6                                                      
         CLC   BUYD.RBUYKLIN,RMKGMGLI                                           
         BE    MISB020             INCASE WE HAVE MAKEGOOD FOR MAKEGOOD         
         GOTO1 VSEQ                                                             
         B     MISB010                                                          
         DROP  BUYD                                                             
*                                                                               
MISB020  DS    0H                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
* MISSED LINE NUMBER                                                            
*                                                                               
MISB030  DS    0H                                                               
*        EDIT  RBUYKLIN,(3,PMBLINE),ALIGN=LEFT                                  
*                                                                               
* MISSED BUY NPW/LEN/COST/DP                                                    
*                                                                               
         EDIT  RBUYNW,(3,PMBNPW),ALIGN=LEFT                                     
*                                                                               
         MVC   HALF,RBUYDUR                                                     
                                                                                
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,PMBLEN),ALIGN=LEFT                                   
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LA    RF,PMBLEN                                                        
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,RF                                                            
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
*                                                                               
         EDIT  RBUYCOS,(10,PMBCOST),2,ALIGN=LEFT                                
*                                                                               
         MVC   PMBDP(1),RBUYDPT                                                 
*                                                                               
         BAS   RE,PROGNAME                                                      
*                                                                               
* MISSED DATE(SPOT)                                                             
*                                                                               
MISB040  DS    0H                                                               
         ST    R6,MKGDMSDT         STORE POINTER TO MISSED DATE ELEM            
*                                                                               
         EDIT  RMKGMGLI,(3,PMBLINE),ALIGN=LEFT                                  
*                                                                               
         LA    R4,PMBDATE                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(5,(R4))                                
         AHI   R4,8                                                             
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MISB045                                                          
         MVI   0(R4),C'-'          INSERT RANGE INDICATOR                       
         AHI   R4,1                                                             
         GOTO1 DATCON,DMCB,(3,RMKGMGD2),(5,(R4))                                
         AHI   R4,8                                                             
*                                                                               
MISB045  DS    0H                                                               
*        MVI   0(R4),C'-'          INSERT RANGE INDICATOR                       
*        AHI   R4,1                                                             
*        EDIT  RMKGMGLI,(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                        
*        AR    R4,R0                                                            
*                                                                               
         MVI   0(R4),C'('         INSERT PAREN                                  
         AHI   R4,1                                                             
         EDIT  RMKGMGSP,(3,(R4)),ALIGN=LEFT                                     
         AR    R4,R0               ADD SIGNIFICANT CHARS FROM EDIT              
         MVI   0(R4),C')'          INSERT OTHER PAREN                           
         AHI   R4,1                                                             
                                                                                
         BAS   RE,NEXTEL                                                        
         BNE   MISB050                                                          
*        MVI   0(R4),C'*'          FLAG IF MORE MISSED DATES                    
         DROP  R6                                                               
                                                                                
MISB050  DS    0H                                                               
         OC    MKGDMSDY,MKGDMSDY   ARE WE PRINTING MORE DAY/TIME                
         BZ    MISB060             STRINGS? NULLS IF FIRST TIME                 
         L     R6,MKGDMSDY                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BE    MISB070                                                          
         B     MISB120                                                          
*                                                                               
MISB060  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'02'                                                     
         USING RBUYDYEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DISPLAY DAY-TIME FIELDS                      
MISB070  EQU   *                                                                
         GOTO1 OUTDAY,DMCB,RBUYDAYS,RBUYDYIN,PMBDYTM                            
*                                  DISPLAY DAY-TIME FIELDS                      
         LA    R4,PMBDYTM          CHECK FOR LAST POSITION                      
         LA    RF,L'PMBDYTM        MAX FIELD SIZE                               
*                                                                               
MISB080  EQU   *                                                                
         CLI   0(R4),C' '          SPACE FOUND?                                 
         BE    MISB090             YES                                          
         LA    R4,1(R4)            GO BACK FOR NEXT                             
         BCT   RF,MISB080                                                       
         DC    H'0'                NOT FOUND - ERROR                            
*                                                                               
MISB090  EQU   *                                                                
         MVI   0(R4),C'/'          INSERT SEPARATOR                             
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
*                                  TIME                                         
         GOTO1 UNTIME,DMCB,RBUYDYT1,0(R4)                                       
*                                                                               
         ST    R6,MKGDMSDY         STORE POINTER TO MISSED DAY/TIME ELM         
*                                                                               
         BAS   RE,NEXTEL           ANYMORE DAY/TIME ELM LEFT?                   
         BNE   MISB120                                                          
*                                                                               
MISB100  DS    0H                                                               
         CLI   0(R4),C' '          FIND END OF STRING                           
         BE    MISB110                                                          
         LA    R4,1(R4)                                                         
         B     MISB100                                                          
*                                                                               
MISB110  DS    0H                                                               
         MVI   0(R4),C'*'          SHOW WE'LL CONTINUE ON NEXT LINE             
*                                                                               
MISB120  DS    0H                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
MISB130  DS    0H                                                               
         L     R6,MKGDMSDY         ANY MORE MISSED DAY/TIME STRING?             
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BE    MISB070             YES, KEEP GOING                              
         DROP  R6,R3                                                            
*                                                                               
         LA    R3,P                                                             
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,4                                                         
         BAS   RE,GETEL                                                         
         BNE   MISB170                                                          
*                                                                               
         LA    R0,L'PMBPGM                                                      
         ZIC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         CR    R1,R0                                                            
         BL    MISB170             WAS INSERTED IN PROGRAM NAME                 
*                                                                               
         CLC   =C'MG=',2(R6)                                                    
         BE    MISB170                                                          
         CLC   =C'CR=',2(R6)                                                    
         BE    MISB170                                                          
*                                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         USING PMBUY,R3                                                         
         MVC   PMBCMT(0),2(R6)                                                  
         BAS   RE,GOSPOOL                                                       
         DROP  R3                                                               
*                                                                               
MISB170  DS    0H                                                               
         L     R6,MKGDMSDT         ANY MORE MISSED DATE(S)?                     
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BE    MISB005             YES, KEEP GOING                              
*                                                                               
MGD      USING RMKGREC,IOAREA                                                   
         TM    MGD.RMKGRTS,X'10'+X'08'+X'04'                                    
         BZ    MISSBUYX                                                         
         TM    MGD.RMKGRTS,X'20'                                                
         BO    MISSBUYX                                                         
         MVC   KEY(27),IOAREA                                                   
         GOTO1 VHIGH                                                            
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MISSBUYX                                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         B     MISB000                                                          
*                                                                               
MISSBUYX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY FIRST COMMENT LINE AS PROGRAM NAME                                    
*                                                                               
PROGNAME NTR1                                                                   
         LA    R3,P                                                             
         USING PMBUY,R3                                                         
                                                                                
         ZIC   R4,ELCODE                                                        
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   PNX                                                              
         USING RBUYCMEL,R6                                                      
*                                                                               
         ZIC   R1,RBUYCMLN                                                      
         SHI   R1,3                                                             
         CHI   R1,L'PMBPGM                                                      
         BL    *+8                                                              
         LA    R1,L'PMBPGM-1                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PMBPGM(0),RBUYCMNT                                               
*                                                                               
PNX      DS    0H                                                               
         STC   R4,ELCODE                                                        
         DROP  R3,R6                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY FIRST COMMENT LINE AS PROGRAM NAME                                    
*                                                                               
MPRGNAME NTR1                                                                   
         LA    R3,P                                                             
         USING PMG,R3                                                           
                                                                                
         ZIC   R4,ELCODE                                                        
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   MPNX                                                             
                                                                                
         USING RMKGCMEL,R6                                                      
*                                                                               
         ZIC   R1,RMKGCMLN                                                      
         SHI   R1,3                                                             
         CHI   R1,L'PMGPGM                                                      
         BL    *+8                                                              
         LA    R1,L'PMGPGM-1                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PMGPGM(0),RMKGCMNT                                               
*                                                                               
MPNX     DS    0H                                                               
         STC   R4,ELCODE                                                        
         DROP  R3,R6                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* FORMAT MAKEGOOD OFFER                                                         
*                                                                               
MGFORMAT NTR1                                                                   
         LA    R3,P                                                             
         USING PMG,R3                                                           
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
                                                                                
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BNH   MGFMT10             YES - NO FLAGS TO SET                        
         MVC   PMGOFFR(4),=C'AND ' NO  - INSERT 'AND' INDICATOR                 
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT20             NO  - LEAVE 'AND' SET                        
         MVC   PMGOFFR(4),=C'OR  ' YES - SET 'OR' INDICATOR                     
         B     MGFMT18                                                          
                                                                                
MGFMT10  DS    0H                                                               
* OFFER NUMBER                                                                  
         EDIT  RMKGKLIN,(3,PMGOFFR),ALIGN=LEFT                                  
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT20             NO  - LEAVE 'AND' SET                        
         DROP  R6                                                               
                                                                                
MGFMT18  DS    0H                                                               
         MVI   ELCODE,X'20'        STATUS CONTROL ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      TAG WITH '*' IF SELECTED?                    
         BZ    MGFMT20                                                          
         MVI   PMGOFFR+2,C'*'                                                   
         DROP  R3,R6                                                            
                                                                                
* OFFERED DATE(SPOT)                                                            
MGFMT20  DS    0H                                                               
         BAS   RE,OFFRDATE                                                      
*                                                                               
         LA    R3,P                                                             
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   MGFMTX                                                           
         USING RMKGDCEL,R6                                                      
*                                                                               
         LA    R0,L'PMGPGM                                                      
         ZIC   R1,RMKGDCLN                                                      
         SHI   R1,3                MINUS OVERHEAD                               
         CR    R1,R0               IF GREATER THAN PROGNAME NAME FIELD          
         BNL   MGFMT55             WANT TO PRINT ENTIRE NAME/COMMENT            
*                                                                               
MGFMT50  BAS   RE,NEXTEL           IT WAS PRINTED IN PROGRAM NAME               
         BNE   MGFMT60                                                          
         ZIC   R1,1(R6)                                                         
         SHI   R1,3                                                             
*                                                                               
MGFMT55  DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         USING PMG,R3                                                           
         MVC   PMGCMT(0),2(R6)                                                  
         LA    R3,132(R3)                                                       
         B     MGFMT50                                                          
         DROP  R3,R6                                                            
*                                                                               
MGFMT60  BAS   RE,GOSPOOL                                                       
                                                                                
MGFMTX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  OFFRDATE:  RETRIEVE/DISPLAY OFFER DATE(S) ON SECOND LINE OF                  
*        DISPLAY.  INSERT # SPOTS/WEEK OFFERED.                                 
***********************************************************************         
OFFRDATE NTR1                                                                   
         SR    R2,R2                                                            
         SR    R5,R5                                                            
*                                                                               
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
*                                                                               
         LA    R3,P                                                             
         USING PMG,R3                                                           
*                                                                               
         TM    RMKGRTS,X'10'       SKIP OFFER INFO IF PREEMPT                   
         BZ    OFDT0005                                                         
         MVC   PMGDATE(7),=C'PREEMPT'                                           
         BAS   RE,GOSPOOL                                                       
         B     OFDTX                                                            
* NPW                                                                           
OFDT0005 DS    0H                                                               
         EDIT  RMKGNW,(3,PMGNPW),ALIGN=LEFT                                     
* LENGTH                                                                        
         MVC   HALF,RMKGDUR                                                     
                                                                                
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,PMGLEN),ALIGN=LEFT                                   
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LA    RF,PMGLEN                                                        
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,RF                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
* COST                                                                          
         EDIT  RMKGCOS,(10,PMGCOST),2,ALIGN=LEFT                                
*                                                                               
         MVC   PMGDP(1),RMKGDPT                                                 
         DROP  R6                                                               
*                                                                               
         BAS   RE,MPRGNAME                                                      
*                                                                               
* DATE/DAY/TIME                                                                 
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OFDT0010 DS    0H                                                               
         LR    R5,R6                                                            
                                                                                
*        LA    R4,P                OUTPUT                                       
*        USING PMG,R4                                                           
         LA    R4,PMGDATE                                                       
*        DROP  R4                                                               
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R4))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R4,5(R4)                                                         
         B     OFDT0020                                                         
*                                                                               
         MVI   5(R4),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R4))                                    
*                                  END DATE                                     
         LA    R4,11(R4)                                                        
*                                                                               
OFDT0020 TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R4),C'A'                                                       
         LA    R4,1(R4)                                                         
*                                  DISPLAY NPW IF NOT = RBUYNW                  
         LA    RF,IOAREA                                                        
         USING RMKGREC,RF                                                       
         CLC   RMKGNW,9(R6)                                                     
         BE    OFDT0030                                                         
         DROP  RF                                                               
                                                                                
         MVI   0(R4),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R4)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R4),C'0'                                                       
         LA    R4,2(R4)                                                         
         MVI   2(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),C')'                                                       
*                                                                               
*   MKGDETLS:  DISPLAY DAY/TIME/LENGTH/COST OF SPOTS OF MAKEGOOD                
*        OFFER.                                                                 
*                                                                               
OFDT0030 DS    0H                                                               
         ST    R6,MKGDOSDY                                                      
         BAS   RE,NEXTEL                                                        
         BNE   OFDT0035                                                         
*                                                                               
         MVI   1(R4),C'*'                                                       
*                                                                               
OFDT0035 DS    0H                                                               
         L     R6,MKGDOSDY         RESTORE POINTER TO MISSED DATE ELEM          
                                                                                
         LTR   R2,R2                                                            
         BZ    OFDT0040                                                         
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   OFDT0090                                                         
         B     OFDT0050                                                         
                                                                                
OFDT0040 DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OFDT0050 DS    0H                                                               
         LR    R2,R6                                                            
         MVC   PMGDYTM,SPACES                                                   
*                                  CLEAR DAY/TIME STRING AREA                   
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 OUTDAY,DMCB,3(R6),2(R6),PMGDYTM                                  
*                                  TIME                                         
         LA    R4,PMGDYTM          CHECK FOR LAST POSITION                      
         LA    RF,L'PMGDYTM        MAX FIELD SIZE                               
OFDT0060 EQU   *                                                                
         CLI   0(R4),C' '          SPACE FOUND?                                 
         BE    OFDT0070            YES                                          
         LA    R4,1(R4)            GO BACK FOR NEXT                             
         BCT   RF,OFDT0060                                                      
         DC    H'0'                NOT FOUND - ERROR                            
OFDT0070 EQU   *                                                                
         MVI   0(R4),C'/'          INSERT SEPARATOR                             
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 UNTIME,DMCB,4(R6),0(R4)                                          
*                                                                               
         ST    R6,MKGDOSDY                                                      
         BAS   RE,NEXTEL                                                        
         BNE   OFDT0080                                                         
*                                                                               
OFDT0075 DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BE    OFDT0078                                                         
         LA    R4,1(R4)                                                         
         B     OFDT0075                                                         
*                                                                               
OFDT0078 DS    0H                                                               
         MVI   0(R4),C'*'                                                       
*                                                                               
OFDT0080 DS    0H                                                               
         L     R6,MKGDOSDY         RESTORE POINTER TO MISSED DATE ELEM          
                                                                                
OFDT0090 EQU   *                                                                
         BAS   RE,GOSPOOL                                                       
                                                                                
         LR    R6,R5                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BE    OFDT0010                                                         
                                                                                
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BE    OFDT0050                                                         
*                                                                               
OFDTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* MAKEGOOD FORM HEADER                                                          
*                                                                               
FHEAD    NTR1                                                                   
         MVC   SAVEP1,P1                                                        
         MVC   SAVEP2,P2                                                        
         MVC   SAVEP3,P3                                                        
         MVC   SAVEP4,P4                                                        
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
*                                                                               
         MVC   P+59(13),=C'MAKEGOOD FORM'                                       
         MVC   P2+59(13),DASHES                                                 
         MVC   P+120(4),=C'PAGE'                                                
         EDIT  PAGE,(3,P+125),ALIGN=LEFT                                        
         CLC   PAGE,=AL2(1)        PAGE 1?                                      
         BNE   FHEAD010                                                         
*                                                                               
         MVC   P1+5(20),=C'INPUT BY  : ________'  PAGE 1 ONLY                   
         MVC   P2+5(20),=C'INPUT DATE: ________'                                
         MVI   P3,0                                                             
*                                                                               
FHEAD010 DS    0H                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
         CLC   PAGE,=AL2(1)        PAGE 1?                                      
         BNE   FHEAD100                                                         
*                                                                               
         MVC   P1(132),DASHES                                                   
         MVC   P3(132),DASHES                                                   
         MVI   P4,0                                                             
*                                                                               
         BAS   RE,GETTYPE                                                       
*        GOTO1 =V(CENTER),DMCB,P2,132,RR=RELO                                   
         MVI   P2,C'/'                                                          
         MVI   P2+131,C'/'                                                      
*                                                                               
*&&DO                                                                           
         MVI   P2,C'/'                                                          
         MVI   P2+131,C'/'                                                      
         MVC   P2+2(6),=C'( )1:1'                                               
         MVC   P2+10(6),=C'( )1:M'                                              
         MVC   P2+18(6),=C'( )M:1'                                              
         MVC   P2+26(6),=C'( )M:M'                                              
         MVC   P2+34(10),=C'( )STA-STA'                                         
         MVC   P2+46(14),=C'( )MONTH-MONTH'                                     
         MVC   P2+62(12),=C'( )MG FOR MG'                                       
         MVC   P2+76(13),=C'( )PARTIAL MG'                                      
         MVC   P2+91(13),=C'( )PARTIAL CR'                                      
         MVC   P2+106(10),=C'( )FULL CR'                                        
         MVC   P2+118(13),=C'( )PREEMPTION'                                     
*&&                                                                             
         BAS   RE,GOSPOOL                                                       
*                                                                               
         MVC   P1+4(12),=C'CLIENT CODE:'                                        
         MVC   P2+4(12),=C'  PROD CODE:'                                        
         MVC   P3+4(12),=C' EST NUMBER:'                                        
         MVC   P4+4(12),=C'     MEDIUM:'                                        
         MVC   P4+17(8),=12C'_'                                                 
*                                                                               
         MVC   P1+34(10),=C' MKT NAME:'                                         
         MVC   P1+45(12),=12C'_'                                                
         MVC   P2+34(10),=C'    BUYER:'                                         
         MVC   P4+34(10),=C'ASSISTANT:'                                         
         MVC   P4+45(12),=12C'_'                                                
*                                                                               
         MVC   P2+45(16),RCONBUYR                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   FHEAD030                                                         
         USING RCONIEL,R6                                                       
*                                                                               
         MVC   P1+17(4),RCONIADV                                                
         MVC   P2+17(4),RCONIPRD                                                
         OC    RCONIPR2,RCONIPR2                                                
         BZ    *+14                                                             
         MVI   P2+21,C'/'                                                       
         MVC   P2+22(4),RCONIPR2                                                
*                                                                               
         MVC   P3+17(10),RCONXEST  DEFAULT USING EXPANDED EST #                 
                                                                                
         OC    P3+17(10),MYSPACES  IF NO EXPANDED ESTIMATE NUMBER FOUND         
         CLC   P3+17(10),MYSPACES  USE OLD FORMAT                               
         BNE   FHEAD040                                                         
         MVC   P3+17(4),RCONIEST                                                
         B     FHEAD040                                                         
         DROP  R6                                                               
*                                                                               
FHEAD030 DS    0H                                                               
         MVC   P1+17(8),=12C'_'                                                 
         MVC   P2+17(8),=12C'_'                                                 
         MVC   P3+17(8),=12C'_'                                                 
*                                                                               
FHEAD040 DS    0H                                                               
         MVC   P1+66(66),DASHES                                                 
         MVI   P2+66,C'/'                                                       
         MVI   P2+131,C'/'                                                      
         MVC   P2+67(7),=C'STATION'                                             
         MVC   P2+75(13),=C'/SUBMITTED BY'                                      
         MVC   P2+97(5),=C'/DATE'                                               
         MVC   P2+104(15),=C'/CONFIRMED WITH'                                   
         MVC   P2+124(5),=C'/DATE'                                              
         MVC   P3+66(66),DASHES                                                 
         MVI   P4+66,C'/'                                                       
         MVI   P4+75,C'/'                                                       
         MVI   P4+97,C'/'                                                       
         MVI   P4+104,C'/'                                                      
         MVI   P4+124,C'/'                                                      
         MVI   P4+131,C'/'                                                      
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,P4+67                          
         BAS   RE,GOSPOOL                                                       
         MVC   P1+66(66),DASHES                                                 
*        MVI   P2+66,C'/'                                                       
*        MVI   P2+75,C'/'                                                       
*        MVI   P2+97,C'/'                                                       
*        MVI   P2+104,C'/'                                                      
*        MVI   P2+124,C'/'                                                      
*        MVI   P2+131,C'/'                                                      
*        MVC   P3+66(66),DASHES                                                 
         MVI   P2,0                                                             
         BAS   RE,GOSPOOL                                                       
*                                                                               
FHEAD100 DS    0H                                                               
         MVC   P1+8(9),=C'CONTRACT:'                                            
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(6,P1+18)                  
*                                                                               
         MVC   P2+8(9),=C'MG GROUP:'                                            
         MVC   P2+18(2),MYGRP                                                   
*                                                                               
         MVC   P3+10(7),=C'STATUS:'                                             
*                                                                               
         MVC   P3+18(3),=C'NEW'                                                 
                                                                                
         TM    MYGRPST,RMKGSAPQ                                                 
         BZ    *+14                                                             
         MVC   P3+18(7),=C'APPLIED'                                             
         B     FHEAD110                                                         
         TM    MYGRPST,RMKGSBOQ                                                 
         BZ    *+14                                                             
         MVC   P3+18(10),=C'BACKED-OUT'                                         
         B     FHEAD110                                                         
         TM    MYGRPST,RMKGSRCQ                                                 
         BZ    *+14                                                             
         MVC   P3+18(8),=C'RECALLED'                                            
         B     FHEAD110                                                         
         TM    MYGRPST,RMKGSRJQ                                                 
         BZ    *+14                                                             
         MVC   P3+18(8),=C'REJECTED'                                            
         B     FHEAD110                                                         
         TM    MYGRPST,RMKGSRVQ                                                 
         BZ    *+10                                                             
         MVC   P3+18(7),=C'REVISED'                                             
FHEAD110 DS    0H                                                               
*        MVC   P3+30(9),=C'ASSUME OK'                                           
*        TM    MYGRPST,RMKGSPAQ                                                 
*        BZ    *+10                                                             
*        MVC   P3+30(13),=C'PLEASE ADVISE'                                      
*                                                                               
         MVC   P1+56(6),=C'BUYER:'                                              
         MVC   P1+63(L'RCONBUYR),RCONBUYR                                       
*                                                                               
         MVC   P2+50(12),=C'SALESPERSON:'                                       
         MVC   P2+63(L'MGCSALN),MGCSALN                                         
*                                                                               
         MVC   P3+54(8),=C'PRODUCT:'                                            
         MVC   P3+63(L'MGCPRD),MGCPRD                                           
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         MVC   P1+90(11),=C'ADVERTISER:'                                        
         GOTO1 (RFADVOUT,VREPFACS),DMCB,RCONKADV,P1+102,0,DUB                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P2+94(7),=C'AGENCY:'                                             
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,DUB,P2+102,DUB                 
*                                                                               
         MVI   P4,0                                                             
         BAS   RE,GOSPOOL                                                       
         MVC   P1,SAVEP1                                                        
         MVC   P2,SAVEP2                                                        
         MVC   P3,SAVEP3                                                        
         MVC   P4,SAVEP4                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* GET MAKEGOOD OFFER TYPE                                                       
*                                                                               
GETTYPE  NTR1                                                                   
         MVC   SAVEKEY,IOAREA                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         GOTO1 VHIGH               GET HEADER                                   
         GOTO1 VSEQ                THEN FIRST DETAIL RECORD                     
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE     SAME GROUP?                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
*                                                                               
         TM    RMKGRTS,X'04'                                                    
         BZ    GTYPE02                                                          
         MVC   P2+53(25),=C'LATE RUN WITH BONUS OFFER'                          
         B     GTYPE10                                                          
*                                                                               
GTYPE02  DS    0H                                                               
         TM    RMKGRTS,X'20'                                                    
         BZ    GTYPE03                                                          
         MVC   P2+60(11),=C'BONUS OFFER'                                        
         B     GTYPE10                                                          
*                                                                               
GTYPE03  DS    0H                                                               
         TM    RMKGRTS,X'10'                                                    
         BZ    GTYPE04                                                          
         MVC   P2+59(13),=C'PREEMPT OFFER'                                      
         B     GTYPE10                                                          
*                                                                               
GTYPE04  DS    0H                                                               
         TM    RMKGRTS,X'08'                                                    
         BZ    GTYPE05                                                          
         MVC   P2+59(14),=C'LATE RUN OFFER'                                     
         B     GTYPE10                                                          
*                                                                               
GTYPE05  DS    0H                                                               
         MVC   P2+59(14),=C'MAKEGOOD OFFER'                                     
         DROP  R6                                                               
*                                                                               
GTYPE10  DS    0H                                                               
         MVC   KEY(27),SAVEKEY     RESTORE KEY                                  
         GOTO1 VHIGH                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
GETDARE  NTR1                                                                   
AGY2D    USING RAGY2REC,KEY                                                     
         XC    AGY2D.RAGY2KEY,AGY2D.RAGY2KEY                                    
         MVI   AGY2D.RAGK2TYP,RAGK2TYQ                                          
         MVC   AGY2D.RAGK2AGY,RCONKAGY                                          
         MVC   AGY2D.RAGK2AOF,RCONKAOF                                          
         MVC   AGY2D.RAGK2REP,RCONKREP                                          
         DROP  AGY2D                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,AIO4                                                
                                                                                
DARD     USING RDARKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   DARD.RDARKTYP,X'51'                                              
         MVC   DARD.RDARKREP,REPALPHA                                           
         MVC   DARD.RDARKSTA(5),RCONKSTA                                        
         CLI   DARD.RDARKSTA+4,C' '                                             
         BNE   *+8                                                              
         MVI   DARD.RDARKSTA+4,C'T' MUST SPECIFY IF TV                          
         OC    DARD.RDARKSTA,MYSPACES                                           
*                                                                               
         L     R6,AIO4                                                          
         USING RAGY2REC,R6                                                      
         MVC   DARD.RDARKAGY(5),RAGY2DAR EQUIVALENCY CODE                       
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DARD.RDARKORD,RCONDRLK ORDER NUMBER                              
         MVI   DARD.RDARKRT,X'10'  AGENCY HEADER ONLY                           
         DROP  R6                                                               
                                                                                
         L     R6,AIO4                                                          
         USING RAGY2REC,R6                                                      
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
                                                                                
GETDAR10 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    GETDAR20                                                         
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,5(R4)                                                         
         MVC   DARD.RDARKAGY(5),0(R4) EQUIVALENCY CODE                          
         BCT   R3,GETDAR10                                                      
         B     GETDARNO                                                         
         DROP  DARD                                                             
*                                                                               
GETDAR20 DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO4   USE IO4                                      
*                                                                               
GETDARYS DS    0H                                                               
         SR    RC,RC                                                            
GETDARNO LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CONSOLIDATE GOTO1 SPOOL CALLS                                                 
*                                                                               
GOSPOOL  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
DASHES   DC    132C'-'                                                          
OKMESS   DC    C'XXX,12345 HAS BEEN SPOOLED. PAGES=NNN,LINES=NNNN'              
FOKMESS  DC    C'XXX,12345 HAS BEEN SPOOLED TO FAX.'                            
         DC    C'NNNN'                                                          
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTD1D                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPDARMKGDD                                                     
       ++INCLUDE REGENDAR                                                       
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*                                                                               
***********************************************************************         
* LOCAL STORAGE                                                                 
***********************************************************************         
MYWORKD  DSECT                                                                  
RELO     DS    F                                                                
MGFLAGS  DS    X                                                                
MYGRP    DS    CL2                 MAKEGOOD GROUP IDENTIFIER                    
MYGRPST  DS    CL1                 MAKEGOOD GROUP STATUS                        
MKGDMSDT DS    A                   MAKEGOOD MISSED DATE ELEMENT POINTER         
MKGDMSDY DS    A                   MAKEGOOD MISSED DAY/TIME ELEM PTR            
MKGDOSDY DS    A                   MAKEGOOD OFFER MISSED DATE/DAY/TIME          
MKGSTAT  DS    X                   STATUS FLAG FOR MAKEGOOD PROCESSING          
LASTOFR  DS    C                   LAST MG OFFER #                              
SAVEP1   DS    CL132                                                            
SAVEP2   DS    CL132                                                            
SAVEP3   DS    CL132                                                            
SAVEP4   DS    CL132                                                            
SAVEKEY  DS    CL27                                                             
MYWORKX  EQU   *                                                                
*                                                                               
PMBUY    DSECT                     MISSED BUY PRINT LINE                        
PMBLINE  DS    CL4                                                              
         DS    CL2                                                              
PMBCMT   DS    0C                                                               
PMBDATE  DS    CL28                                                             
         DS    CL2                                                              
PMBDYTM  DS    CL22                                                             
         DS    CL2                                                              
PMBNPW   DS    CL3                                                              
         DS    CL2                                                              
PMBDP    DS    CL2                                                              
         DS    CL2                                                              
PMBLEN   DS    CL3                                                              
         DS    CL2                                                              
PMBPGM   DS    CL18                                                             
         DS    CL2                                                              
PMBCOST  DS    CL10                                                             
         DS    CL2                                                              
PMBDEMO  DS    CL14                                                             
*                                                                               
PMG      DSECT                     MAKEGOOD PRINT LINE                          
PMGOFFR  DS    CL4                                                              
         DS    CL2                                                              
PMGCMT   DS    0C                                                               
PMGDATE  DS    CL28                                                             
         DS    CL2                                                              
PMGDYTM  DS    CL22                                                             
         DS    CL2                                                              
PMGNPW   DS    CL3                                                              
         DS    CL2                                                              
PMGDP    DS    CL2                                                              
         DS    CL2                                                              
PMGLEN   DS    CL3                                                              
         DS    CL2                                                              
PMGPGM   DS    CL18                                                             
         DS    CL2                                                              
PMGCOST  DS    CL10                                                             
         DS    CL2                                                              
PMGDEMO  DS    CL14                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013RECNT73   08/05/02'                                      
         END                                                                    
