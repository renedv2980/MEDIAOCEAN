*          DATA SET RECNT60    AT LEVEL 048 AS OF 07/27/12                      
*PHASE T80260A,*                                                                
         TITLE 'T80260 - AVAIL/PROPOSAL/CONTRACT PRINT CONTROLLER'              
***********************************************************************         
*                                                                     *         
*  RECNT60  (T80260 )  --  REP CONTRACT PRINT CONTROLLER              *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
*  UPDATE HISTORY:                                                    *         
*                                                                     *         
*  11/14/89  PJS  CHANGES FOR 3RD WORKSHEET COPY.                     *         
*                 (USE SENDPASS FIELD INSTEAD OF SENDID TO DETERMINE  *         
*                  WHICH PASS WE ARE ON)                              *         
*                                                                     *         
*  11/27/89  PJS  CHANGES FOR 'CNF' 3RD WORKSHEET.                    *         
*                                                                     *         
* 12FEB91  (EFJ)  --- DISABLE KEEP STATUS FOR FMT G (WU)              *         
*                                                                     *         
* 05AUG92  (SKU)  --- COMBO PRINT SUPPORT.  ADJUST SPOOLKEY TO NEW    *         
*                     133 BYTES SIZE.                                 *         
*                                                                     *         
* 18JAN95  (SKU)  --- CLEAR MESSAGE BEFORE DISPLAYING OK MESSAGE      *         
*                                                                     *         
* 16AUG95  (SKU)  --- SPECIFY MGO FOR ACTION MGS (MAKEGOOD OFFERS)    *         
*                     INSTEAD OF ORD FOR REPORT-ID                    *         
*                                                                     *         
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                               *         
*                                                                     *         
* 02MAR96 (SKU) --- ENABLE MESSAGE FOR CFX AND TWAACFPT=Y             *         
*                                                                     *         
* 24JUL97 (SKU) --- 4K CONTRACT SUPPORT                               *         
*                                                                     *         
* 14JUL00 (SKU) --- ALLOW 'LAST=XXX' WHERE XXX WILL BE THE REPORT ID  *         
*                                                                     *         
* 21NOV00 (SKU) --- CHANGE CALL TO CORE RESIDENT RECNT63 MODULE       *         
*                                                                     *         
* 10JUL02 (BU ) --- 'ORS' AS REPORT ID FOR WEB CONFIRM STATIONS       *         
*                                                                     *         
* 29JAN03 (SKU) --- USE SENDID IF GRAPH RADIO EDI CONTRACT/ORDER      *         
*                                                                     *         
* 26AUG03 (SKU) --- CHANGE RETENTION TIME TO 21 HOURS                 *         
*                                                                     *         
* 02MAR04 (HQ ) --- ACCOMMODATE 8 DIGIT CONTRACT NUMBER               *         
*                                                                     *         
* 24JAN11 (SKU) --- SET RETENTION TO 48HRS FOR UNIVISION ORS REPORTS  *         
*                                                                     *         
* 23JUN11 (BOB) --- ARCHIVE REPORTS BASED ON PROFILE                  *         
***********************************************************************         
T80260   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80260**                                                       
         L     RC,0(R1)                                                         
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GENOLD,RC,R9                                                     
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPULAR                                                       
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         ST    R8,ASPOOLD                                                       
         ST    RB,SAVERB                                                        
         ST    RD,SAVERD                                                        
         BAS   RE,INITIAL                                                       
         EJECT                                                                  
*              LOAD CONTROL                                                     
         SPACE 3                                                                
LOAD     DS    0H                                                               
         GOTO1 CALLOV,DMCB,0,X'D9080263',0                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,0(R1)                                                         
         SPACE 2                                                                
LOAD10   DS    0H                                                               
         GOTO1 (R4),PARAS,(RC)     GOTO APPLICATION                             
*                                                                               
LOAD10A  CLI   NOPQCLOS,X'1'       SKIP PRINT Q CLOSE?                          
         BE    BEXIT                                                            
*                                                                               
         CLC   CONACT,=C'CONX'     IF CONX, NOTHING WAS GENERATED.              
         BNE   LOAD11              SKIP PQ CLOSE AND PUT OUT                    
         MVC   IOAREA(48),CONXMSG  APPROPRIATE MESSAGE                          
         B     LOAD20                                                           
*                                                                               
LOAD11   MVC   INTREPNO,SPOOLRPN                                                
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
*- ONLY EDIT OUT REPORT NUMBER ON 1ST PASS (ORIGINATOR COPY)                    
         CLI   SENDPASS,1                                                       
         BE    LOAD15                                                           
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BO    LOAD15                                                           
         CLI   SENDPASS,2          FOR CFX AND REP CONFIRMED WORKSHEET          
         BNE   BEXIT               ONLY, DISPLAY MESSAGE TO USER                
         CLC   =C'CFX',CONACT                                                   
         BNE   BEXIT                                                            
*                                                                               
LOAD15   MVC   INTPAGNO,SPOOLPAG                                                
         MVC   INTLINNO,SPOOLLIN                                                
         MVC   IOAREA(48),OKMESS                                                
         MVC   IOAREA(3),SPOOLKEY+12                                            
         LA    R1,IOAREA                                                        
         EDIT  (2,SPOOLRPN),(5,4(R1)),ALIGN=LEFT                                
         EDIT  (2,SPOOLPAG),(3,34(R1)),ALIGN=LEFT                               
         EDIT  (2,SPOOLLIN),(4,44(R1)),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,IOAREA,48                                          
LOAD20   DS    0H                                                               
         OI    CONMSGH+6,X'80'     XMIT MESSAGE                                 
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(48),IOAREA                                                
         B     BEXIT                                                            
         EJECT                                                                  
*              INITIALIZATION                                                   
         SPACE 3                                                                
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
         SPACE 1                                                                
         MVI   DMCB+7,X'E0'        USE DEMOCON INSTEAD OF DEMCON                
         GOTO1 CALLOV,DMCB                                                      
         MVC   DEMCON,DMCB                                                      
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,AANY                                                          
         LA    R5,VCOUNT                                                        
         SPACE 2                                                                
INIT4    ST    R2,0(R4)            A(COMMON)                                    
         STC   R3,0(R4)            ROUTINE NUMBER                               
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,INIT4                                                         
         SPACE 1                                                                
         ST    R2,PQOPEN                                                        
         STC   R3,PQOPEN                                                        
         SPACE 2                                                                
         MVC   FILENAME,SPACES                                                  
         MVI   DMSOURCE,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
         SPACE 2                                                                
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         MVI   PAVMEDIA,C'T'                                                    
         MVI   ION,1                                                            
         LA    R2,RCONREC                                                       
         ST    R2,AIO                                                           
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   DMFILE,C'R'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES ENTERABLE FROM BASE OR OVERLAY                          
         SPACE 3                                                                
VCOMMON  NTR1  BASE=SAVERB                                                      
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 2                                                                
VBRANCH  B     BANY                                                             
         B     BHIGH                                                            
         B     BSEQ                                                             
         B     BREAD                                                            
         B     BGETREC                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         B     VPQOPEN                                                          
         SPACE 2                                                                
BANY     B     XIT                 SPARE ROUTINE                                
         SPACE 2                                                                
BANY2    MVC   WORK,SPACES                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
BHIGH    MVC   COMMAND,=C'DMRDHI'                                               
         MVI   DMCOM,C'H'                                                       
         MVC   KEYSAVE,KEY                                                      
         B     VDIR                                                             
         SPACE 2                                                                
BSEQ     MVC   COMMAND,=C'DMRSEQ'                                               
         MVI   DMCOM,C'S'                                                       
         B     VDIR                                                             
         SPACE 2                                                                
BREAD    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   DMCOM,C'R'                                                       
         SPACE 2                                                                
VDIR     MVC   FILENAME(6),=C'DEMFIL'                                           
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   FILENAME(6),=C'DEMDIR'                                           
         CLI   KEY,C'A'                                                         
         BE    *+10                                                             
         MVC   FILENAME(3),=C'PAV'                                              
         CLI   DMFILE,C'R'                                                      
         BNE   VALL                                                             
         MVC   FILENAME(6),=C'REPDIR'                                           
         CLI   KEY,X'12'           USE PARENT REP ON INVENTORY                  
         BNE   VALL                                                             
         MVC   KEY+10(2),INTREPNO                                               
         MVC   KEYSAVE+10(2),INTREPNO                                           
         SPACE 2                                                                
VALL     ZIC   R3,ION              1,2,3                                        
         BCTR  R3,0                0,1,2                                        
         SLL   R3,2                0,4,8                                        
         L     R3,AIO(R3)                                                       
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R3),(RF)                                                       
         CLI   DMFILE,C'D'         FOR DEMFIL                                   
         BNE   *+16                                                             
         MVC   0(20,R3),KEY        PUT WHOLE KEY INTO I/O                       
         MVC   KEY(4),DAMAJOR      AND D/A OF MAJOR INTO KEY                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),                         X        
               FILENAME,KEY,(R3),(TERMNAL,0)                                    
         MVC   KEY,0(R3)                                                        
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   DAMAJOR,KEY+19                                                   
         CLI   DMCOM,C'R'                                                       
         BNE   DMCHECK                                                          
         CLC   KEY(18),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         CLI   DMFILE,C'M'                                                      
         BE    DMCHECK                                                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         CLI   DMFILE,C'R'                                                      
         BNE   DMCHECK                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         B     DMCHECK                                                          
         SPACE 2                                                                
BGETREC  ZIC   R3,ION                                                           
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     R3,AIO(R3)                                                       
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),                      X        
               =C'REPFILE',KEY+28,(R3),(TERMNAL,DMWORK)                         
         SPACE 2                                                                
DMCHECK  MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         BZ    XIT                                                              
BEXIT    L     RD,SAVERD                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* 'BYTE' CONTAINS A PARAMETER TO THIS ROUTINE.  IF BYTE IS A 'W',               
* THEN CLASS 'G' REPORTS WILL BE CREATED WITH STATUS 'KEEP'.  THIS              
* WILL CAUSE THEM TO BE PICKED UP BY WESTERN UNION, NOT GRAPHNET.               
*                                                                               
VPQOPEN  XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,X'40'   ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         MVC   SPOOLKEY+12(3),=C'ORD'                                           
         CLI   TWAACCS,C'$'        STATION SIDE SEND?                           
         BNE   VPQO0010            NO  -                                        
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASTAOB,X'08'      YES - VIA WEB CONFIRM?                       
*                                    (STN OPT BIT SET IN 'RSTAOPTB')            
         BNO   VPQO0010            NO  - LEAVE AS IS                            
         DROP  RF                                                               
*                                                                               
         MVC   SPOOLKEY+12(3),=C'ORS'                                           
*                                  YES - SET REPORT ID TO 'ORS'                 
VPQO0010 DS    0H                                                               
*                                                                               
* CHECK IF USER SPECIFIED UNIQUE REPORT NAME INSTEAD OF ORD                     
* FORMAT IS LAST=XXX, WHERE XXX WILL REPLACE ORD                                
*                                                                               
         CLC   =C'LAST=',CONCACT                                                
         BNE   VPQO0040                                                         
         CLI   CONCACTH+5,8        NO XXX, USE ORD AS DEFAULT                   
         BE    VPQO0020                                                         
         LA    R2,CONCACTH         MUST BE 3 CHARACTERS                         
         LA    R3,871                                                           
         B     ERROR                                                            
*                                                                               
VPQO0020 DS    0H                                                               
         MVC   SPOOLKEY+12(3),CONCACT+5                                         
*                                                                               
* SPECIFY IF ORDER WORKSHEET IS ACTUALLY A MAKEGOOD OFFER                       
*                                                                               
VPQO0040 DS    0H                                                               
         CLC   =C'MGS',CONCACT                                                  
         BNE   *+10                                                             
         MVC   SPOOLKEY+12(3),=C'MGO'                                           
*                                                                               
* IF OFFICE PROF SET, ID REPORT WITH SALESMAN CODE RATHER THAN 'ORD'            
* FOR REP ORDER WORKSHEETS                                                      
*                                                                               
         CLI   DUB,X'FF'   **FORMAT WAS PUT IN DUB BY RECNT63**                 
         BNE   VPQO0050                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAOFFPR,X'04'                                                   
         BZ    VPQO0050                                                         
         MVC   SPOOLKEY+12(3),RCONSAL                                           
         DROP  RF                                                               
VPQO0050 DS    0H                                                               
*                                                                               
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   1(8,R3),CONCNUM                                                  
*                                                                               
         CLC   CONCNUM,SPACES      SKIP IF ALL SPACES                           
         BE    VPQO0090                                                         
*                                                                               
         CLI   TWAACCS,C'$'        IF STATION SIDE SEND                         
         BNE   VPQO0090                                                         
*                                                                               
         LA    RF,8(R3)            FIND END OF CONTRACT NUMBER                  
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         MVC   1(2,RF),=C'-S'         FLAG REPORT NAME                          
*                                                                               
VPQO0090 DS    0H                                                               
*                                                                               
VPQO0100 CLI   1(R3),C'0'                                                       
         BNE   VPQO0120                                                         
         MVC   1(9,R3),2(R3)                                                    
         XC    10(1,R3),10(R3)       CLEAN UP TRAILING DIGIT                    
         B     VPQO0100                                                         
         SPACE 2                                                                
VPQO0120 DS    0H                                                               
         GOTO1 SQUASHER,DMCB,SPOOLKEY+1,11                                      
         MVI   SPOOLKEY+16,68      68 LINES TO A PAGE                           
         MVI   SPMODE,0                                                         
         SPACE 1                                                                
         CLI   INTTYPE,C'O'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         MVI   PLCLASS,C'K'        CLASS K                                      
*                                                                               
         CLC   CONACT,=C'LAST'     IF ACTION 'LAST'                             
         BE    VPQO0130               IGNORE SENDID CODE                        
*                                                                               
         OC    SENDID,SENDID       IF SEND ID, STORE IN PLUSER                  
         BZ    VPQO0130                                                         
         MVC   PLUSER(2),SENDID                                                 
         SPACE 1                                                                
         CLC   PLUSER(2),=X'0406'   FOR GRAPHNET                                
         BNE   VPQO0130                                                         
         MVI   PLCLASS,C'G'        CLASS G INSTEAD OF K AND                     
         CLI   BYTE,C'W'           TEST WESTERN UNION STATION                   
         BNE   *+8                 NO                                           
*         OI    PLSTAT,X'08'        'KEEP' STATUS MEANS IT'S FOR W.U.           
         XC    PLUSER(2),PLUSER    USE ORIGINATING ID, NOT 'GRAPH'              
         DROP  RF                                                               
*                                                                               
* FOR RADIO EDI, IF PLUSER IS 'GRAPH' AND SIGNON IS MASTER, USE                 
* RCONSSID FROM X'20' ELEMENT IN CONTRACT SO REPORT IS CREATED IN THE           
* CORRECT PQ                                                                    
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAGENFG,TWQMASTR   MASTER PROCESS FROM DARE?                    
         BNO   VPQO0130                                                         
         DROP  RF                                                               
*                                                                               
         CLI   RCONKSTA+4,C'A'                                                  
         BE    VPQO0125                                                         
         CLI   RCONKSTA+4,C'F'                                                  
         BNE   VPQO0130                                                         
*                                                                               
VPQO0125 DS    0H                                                               
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'20'        EXTENDED DESCRIPTION ELEMENT                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NOT FOUND                                    
*                                                                               
         USING RCONSEND,R6                                                      
         OC    RCONSSID,RCONSSID   ANY SENDING ID?                              
         BZ    VPQO0130            NO  - USE TWA                                
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         MVC   PLUSER,RCONSSID     YES - USE IT                                 
         DROP  R6,RF                                                            
*                                                                               
VPQO0130 LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    RE,TWASPKEY                                                      
         DROP  RF                                                               
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'      ORDER WORKSHEETS                             
*                                                                               
         CLC   CONACT,=C'LAST'     IF ACTION 'LAST'                             
         BE    *+10                                                             
         CLC   CONACT,=C'SEND'     IF ACTION 'SEND'                             
         BNE   VPQO0131                                                         
*                                                                               
         TM    PROFILES+CNTARCHB,CNTARCHA    IF PROFILE SET                     
         BNO   *+8                                                              
         OI    QLTYP1,QLTYAR          SET REPORT IS TO BE ARCHIVED              
*                                                                               
VPQO0131 DS    0H                                                               
*                                                                               
* CHECK STAD ADDITIONAL OPTIONS 3 IF SET ORS RETENTION TO 48 HRS                
*                                                                               
         CLC   =C'ORS',SPOOLKEY+12                                              
         BNE   VPQO0132                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASTADF,X'20'                                                   
         BZ    VPQO0132                                                         
         DROP  RF                                                               
         MVC   QLRETNL,=H'48'                                                   
         B     VPQO0140                                                         
*                                                                               
* REDUCE RETENTION TIME FOR THE FOLLOWING REPS                                  
*                                                                               
VPQO0132 DS    0H                                                               
         LA    R6,RETLIST                                                       
VPQO0133 CLC   =X'FF',0(R6)                                                     
         BE    VPQO0140                                                         
         CLC   REPALPHA,0(R6)                                                   
         BE    VPQO0135                                                         
         AHI   R6,2                                                             
         B     VPQO0133                                                         
*                                                                               
VPQO0135 DS    0H                                                               
         MVC   QLRETNL,=H'24'      ORDER WORKSHEETS                             
*                                                                               
VPQO0140 DS    0H                                                               
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'OR'                                                     
         SPACE 1                                                                
         CLC   SENDID(2),=X'0406'  FOR GRAPHNET COPY, USE                       
         BNE   VPQO0150                                                         
         MVC   QLRETND,=H'26'      PRTD/SENT RETENTION OF 26, NOT 2             
         DROP  RE                                                               
VPQO0150 GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     XIT                                                              
         SPACE 3                                                                
RETLIST  DC    C'AM',C'CQ',C'SZ',C'AQ',C'MG',C'IF',C'D4',C'IB',C'NX'            
         DC    C'UO',C'KU',C'KF',C'CR',C'NU',C'QD',C'G8',C'WC',C'RA'            
         DC    C'SJ',C'B3',C'U1',C'U2',C'UR'                                    
         DC    X'FFFF'                                                          
*                                                                               
OKMESS   DC    C'XXX,12345 HAS BEEN SPOOLED. PAGES=NNN,LINES=NNNN'              
CONXMSG  DC    C'**** CONTRACT CONFIRMED ****                    '              
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RECNT60   07/27/12'                                      
         END                                                                    
