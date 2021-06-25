*          DATA SET ACREP1002  AT LEVEL 023 AS OF 06/04/04                      
*PHASE AC1002A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DDS BILLING DOWNLOAD RECORDS'                                   
AC1002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC10**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC10D,RC                                                         
         USING BILDWND,DWNLINE                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVA                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PRCTRN                                                           
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  RUN FIRST - SET BUFFALO, INITIALIZE DOWNLOAD                                 
******************************************************************              
*                                                                               
RUNF     MVC   VTYPES(VTYPLNQ),ADCONS     RELOCATE ADDRESSES                    
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         L     RF,GETOPT           A(GETOPT)                                    
         MVC   0(2,RF),=X'07FE'    BR   RE                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         NI    DWNSTAT,X'FF'-DWNHDLN       TURN OFF HEADLINES                   
         XC    SVAID,SVAID                                                      
*                                                                               
         ZAP   TOTMGROS,=P'0'                                                   
         ZAP   TOTYGROS,=P'0'                                                   
RUNFX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REQUEST FIRST                                                                
******************************************************************              
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT2,C'Y'                                                       
         BE    REQF03                                                           
         XC    HEADHOOK,HEADHOOK                                                
         MVI   RCSUBPRG,9         NO HEADER INFORMATION                         
         B     REQF05                                                           
*                                                                               
REQF03   CLI   QOPT1,C'Y'                                                       
         BE    REQF05                                                           
         MVI   RCSUBPRG,1                                                       
*                                                                               
REQF05   MVI   RCREQREP,C'N'      DONT PRINT ADDITIONAL REQUEST PAGES           
         CLI   QOPT2,C'Y'                                                       
         BE    REQF10                                                           
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNINIT    INITIALIZE THE DOWNLOAD               
*                                                                               
REQF10   CLC   QSTART,SPACES       IF NO QSTART THEN ASSUME LAST MONTH          
         BE    REQF20                                                           
         OC    QSTART,QSTART                                                    
         BZ    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(9,MONTH)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,DATE3)                                 
         B     REQFX                                                            
*                                                                               
REQF20   GOTO1 DATCON,DMCB,(X'35',0),(9,MONTH),(5,WORK)  PREV MONTH             
         GOTO1 DATCON,DMCB,(X'35',0),(1,DATE3),(5,WORK)  PREV MONTH             
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  LEVEL A FIRST - GET OFFICE, CLIENT CODE AND NAME                             
******************************************************************              
*                                                                               
LEVA     ZAP   BDWNGBA,=P'0'                                                    
         ZAP   BDWNYTD,=P'0'                                                    
*                                                                               
         L     R6,ADHEIRA                                                       
         MVC   SVCLI,3(R6)           CLIENT CODE                                
*                                                                               
LEVA10   MVC   SVCOM,SPACES                                                     
         L     R6,ADLVANAM                                                      
         ZIC   RF,1(R6)                                                         
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVCOM(0),2(R6)         CLIENT NAME                               
*                                                                               
LEVAX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PROCESS TRANSACTIONS                                                         
******************************************************************              
*                                                                               
PRCTRN   L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   PRCTRNX                                                          
         CLC   TRNSANAL,=C'99'     FIND BILLING ELEMENTS                        
         BNE   PRCTRNX                                                          
         CLI   TRNSTYPE,X'13'      EXCLUDE TYPE 19 ONE SIDED POSTINGS           
         BE    PRCTRNX                                                          
         CLI   TRNSTYPE,X'01'      EXCLUDE TYPE 01 INVOICE/BILLABLE             
         BE    PRCTRNX                                                          
         CLI   TRNSTYPE,62         EXCLUDE TYPE 62                              
         BE    PRCTRNX                                                          
         MVC   SVDATE3,TRNSDATE                                                 
         LR    R5,R2                                                            
*                                                                               
PRTR10   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R5)                    "                                    
         AR    R5,RF                       "                                    
         CLI   0(R5),X'00'         END OF RECORD ?                              
         BE    PRTR20              YES                                          
         CLI   0(R5),X'60'         NO, 60 ELEMENT ?                             
         BNE   PRTR10              NO, KEEP LOOKING                             
         USING TRSTATD,R5                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SVDATE3)                             
*                                                                               
PRTR20   DS    0H                                                               
*                                                                               
         CLC   DATE3(1),SVDATE3          COMPARE YEAR                           
         BNE   PRCTRNX                                                          
         CLC   DATE3+1(1),SVDATE3+1      MONTH                                  
         BL    PRCTRNX                                                          
*                                                                               
         ZAP   MTRN,=P'0'                                                       
         OC    TRNSNARR+39(6),TRNSNARR+39   MEDIA COSTS INCLUDE                 
         BZ    *+10                                                             
         ZAP   MTRN,TRNSNARR+39(6)                                              
*                                                                               
         ZAP   MONGROSS,=P'0'                                                   
         ZAP   YTDGROSS,TRNSAMNT         NET                                    
         AP    YTDGROSS,TRNSNARR+15(6)   COMMISSION                             
         SP    YTDGROSS,MTRN                                                    
*                                                                               
         CLC   DATE3,SVDATE3       TRANSACTION ADDED IN GIVEN MONTH             
         BNE   PRTR30                                                           
*                                                                               
         ZAP   MTRN,=P'0'                                                       
         OC    TRNSNARR+39(6),TRNSNARR+39   MEDIA COSTS INCLUDE                 
         BZ    *+10                                                             
         ZAP   MTRN,TRNSNARR+39(6)                                              
*                                                                               
         ZAP   MONGROSS,TRNSAMNT         NET                                    
         AP    MONGROSS,TRNSNARR+15(6)   COMMISSION                             
         SP    MONGROSS,MTRN                                                    
*                                                                               
PRTR30   AP    TOTMGROS,MONGROSS                                                
         AP    TOTYGROS,YTDGROSS                                                
*                                                                               
         ZAP   BUFBIL,MONGROSS       GROSS BILL AMOUNT                          
         ZAP   BUFYTD,YTDGROSS       YTD BILL AMOUNT                            
         MVC   BUFAID,ALPHAID                                                   
         MVC   BUFCLI,SVCLI          CLIENT CODE                                
         MVC   BUFCLN,SVCOM          CLIENT NAME                                
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BE    PRTR40                                                           
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
PRTR40   MVC   BUFCLI,=X'FFFFFF'                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
PRCTRNX  DS    0H                                                               
         DROP  R2                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REQUEST LAST - PRINT OFF DOWNLOAD RECORDS                                    
******************************************************************              
REQL     DS    0H                                                               
         MVC   SVAGN,SPACES                                                     
         USING NAMELD,R3                                                        
         L     R3,ADCMPNAM         AGENCY NAME                                  
         ZIC   R4,NAMLN                                                         
         SHI   R4,NAMLN1Q                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SVAGN(0),NAMEREC                                                 
         DROP  R3                                                               
*                                                                               
         MVC   SVCTRYN,SPACES                                                   
         MVC   SVCTRYN(3),=C'USA'  AGENCY COUNTRY                               
         USING CT5REC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY         READ ACCESS RECORD TO ESTABLISH              
         MVI   CT5KTYP,CT5KTYPQ    AGENCY COUNTRY CODE                          
         MVC   CT5KALPH,ALPHAID                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,=C'CTFILE ',IOKEY,IO,0                       
         BNE   REQL25                                                           
         LA    R2,IO                                                            
         LA    R1,CT5DATA                                                       
         USING CTAGDD,R1                                                        
         SR    R0,R0                                                            
REQL20   CLI   CTAGDEL,0                                                        
         BE    REQL25                                                           
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    R0,CTAGDLEN                                                      
         AR    R1,R0                                                            
         B     REQL20                                                           
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    REQL25                                                           
         CLI   CTAGDCTY,CTRYCAN                                                 
         BNE   *+10                                                             
         MVC   SVCTRYN(6),=C'CANADA'                                            
         DROP  R1,R2                                                            
*                                                                               
REQL25   MVC   BUFKEY,SPACES                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
REQL30   TM    DMCB+8,X'80'           ANY MORE RECORDS?                         
         BO    REQL50                                                           
*                                                                               
         MVC   DWNLINE,SPACES                                                   
         MVC   BDWNAGN,SVAGN                                                    
         MVC   BDWNCTY(6),SVCTRYN                                               
         MVC   BDWNAID,BUFAID                                                   
         MVC   BDWNCLN,BUFCLN         CLIENT NAME                               
         ZAP   BDWNGBA,BUFBIL         GROSS AMOUNT                              
         ZAP   BDWNYTD,BUFYTD         YTD AMOUNT                                
         CLC   BUFCLI,=X'FFFFFF'                                                
         BNE   REQL35                                                           
         MVC   BDWNAGN,SPACES                                                   
         MVC   BDWNAGN(20),=C'--------TOTAL--------'                            
         MVC   BDWNCLI,=C'--------TOTAL--------'                                
         MVC   BDWNCLN,SVAGN                                                    
         B     *+10                                                             
REQL35   MVC   BDWNCLI,BUFCLI         CLIENT                                    
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   REQL40                                                           
         BAS   RE,HCPY                                                          
         MVC   SVAID,BUFAID         SAVE LAST AGENCY                            
         B     REQL45                                                           
*                                                                               
REQL40   GOTO1 ADWNRTE1,DMCB,(RC)   OUTPUT DDS BILLING DOWNLOAD RECS            
*                                                                               
REQL45   GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     REQL30                                                           
*                                                                               
REQL50   GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*  RUN LAST - END OF DOWNLOAD                                                   
******************************************************************              
RUNL     MVC   BDWNAID,=C'------'                                               
         MVC   BDWNAGN,SPACES                                                   
         MVC   BDWNAGN(34),=C'--------ACC FILE TOTAL------------'               
         MVC   BDWNCTY,=C'------'                                               
         MVC   BDWNCLI,=C'------'                                               
         MVC   BDWNCLN,SPACES                                                   
         ZAP   BDWNGBA,TOTMGROS                                                 
         ZAP   BDWNYTD,TOTYGROS                                                 
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   *+12                                                             
         BAS   RE,HCPY                                                          
         B     RUNLX                                                            
         GOTO1 ADWNRTE1,DMCB,(RC)                                               
         GOTO1 ADWNL,DMCB,(RC),DWNEOR                                           
*                                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*  HARDCPY - PRINTS HARDCOPY OF REPORT                                *         
***********************************************************************         
HCPY     NTR1                                                                   
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BE    HCPY10                                                           
         OC    SVAID,SVAID                                                      
         BZ    HCPY10                                                           
         CLC   BUFAID,SVAID                                                     
         BE    HCPY10                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING PLINED,R3                                                        
HCPY10   LA    R3,P                                                             
         MVC   P,SPACES                                                         
*                                                                               
         MVC   PAGY(L'BDWNAID),BDWNAID                                          
         MVC   PCTY(L'BDWNCTY),BDWNCTY                                          
         MVC   PAGN,BDWNAGN                                                     
         MVC   PCLI(L'BDWNCLI),BDWNCLI       CLIENT                             
         MVC   PDESC,BDWNCLN                                                    
         CLC   BUFCLI,=X'FFFFFF'                                                
         BNE   HCPY15                                                           
         MVC   PAGN,SPACES                                                      
         MVC   PAGN(20),=C'--------TOTAL--------'                               
         MVC   BDWNCLI,=C'--------TOTAL--------'                                
         MVC   PDESC,BDWNAGN                                                    
HCPY15   CURED (P10,BDWNGBA),PMON,2,ZERO=NOBLANK,MINUS=YES,COMMAS=Y             
         CURED (P10,BDWNYTD),PYTD,2,ZERO=NOBLANK,MINUS=YES,COMMAS=Y             
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         CLC   BUFCLI,=X'FFFF'                                                  
         BNE   HCPYX                                                            
HCPY20   GOTO1 ACREPORT               EMPTY LINE FOR OFFICE TOTALS              
*                                                                               
HCPYX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ABOXRC   DC    A(BOXRC)                                                         
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(DUMP)                                                          
         DC    A(BXHOOK)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNRTE1)          DOWNLOAD TOTALS ROUTINE                      
         DC    A(HD1TAB)                                                        
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
HD1TAB   DC    CL7'COUNTRY'                                                     
         DC    CL6'AGENCY'                                                      
         DC    CL4'NAME'                                                        
         DC    CL6'CLIENT'                                                      
         DC    CL11'DESCRIPTION'                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         BUFF  LINES=800,ROWS=1,COLUMNS=2,FLAVOR=PACKED,               X        
               COMMENT=36,KEYLIST=(5,A)                                         
********************************************************************            
* DOWNLOAD TOTALS                                                  *            
*     R2   - A(PACKED FIELDS FOR ALL SECTIONS EXCEPT A/R)          *            
*     R2   - FOR A/R A(PACKED FIELDS OR SRA BINTABLE IF BYTE=FF)   *            
*     R4   - A(PRINT LINE - A/R SECTION ONLY)                      *            
*     BYTE - TYPE OF TOTAL (CLI/DIV/CAT/RUN)                       *            
********************************************************************            
*                                                                               
DWNRTE1  DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
*                                                                               
         TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
         BO    *+8                                                              
         BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNAID),BDWNAID  AGENCY CODE                           
         LA    R1,L'BDWNAID               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNAGN),BDWNAGN  AGENCY NAME                           
         LA    R1,L'BDWNAGN               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNCTY),BDWNCTY  COUNTRY                               
         LA    R1,L'BDWNCTY               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNCLI),BDWNCLI CLIENT CODE                            
         LA    R1,L'BDWNCLI               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNCLN),BDWNCLN  CLIENT NAME                           
         LA    R1,L'BDWNCLN               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P10,BDWNGBA),(17,DWNFLD),2,ZERO=NOBLANK,MINUS=YES,COMMA+        
               S=Y                                                              
         LA    R1,17                      GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P10,BDWNYTD),(17,DWNFLD),2,ZERO=NOBLANK,MINUS=YES,COMMA+        
               S=Y                                                              
         LA    R1,17                      GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
DWNHEAD  NTR1                                                                   
         OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD          
*                                                                               
         USING HDLND,R3                                                         
         L     R3,AHD1TAB                                                       
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDAGY),HDAGY     FIRST HEADLINE FIELDS                  
         LA    R1,L'HDAGY                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDAGN),HDAGN     FIRST HEADLINE FIELDS                  
         LA    R1,L'HDAGN                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDCTY),HDCTY     FIRST HEADLINE FIELDS                  
         LA    R1,L'HDCTY                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         CLI   QOPT1,C'Y'                                                       
         BE    *+10                                                             
         MVC   DWNFLD(L'HDCLI),HDCLI                                            
         LA    R1,L'HDCLI                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDDESC),HDDESC   FIRST HEADLINE FIELDS                  
         LA    R1,L'HDDESC                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDMON),MONTH     FIRST HEADLINE FIELDS                  
         LA    R1,L'HDMON                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(4),=C'JAN-'        FIRST HEADLINE FIELDS                  
         MVC   DWNFLD+4(L'MONTH),MONTH                                          
         LA    R1,L'HDYTD                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
DWNHX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* DOWNLOAD MODULE                                                   *           
*          PARM1 - RC                                               *           
*          PARM2 - ACTION                                           *           
*********************************************************************           
*                                                                               
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         LA    R5,DWNBUF                                                        
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,P                PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
         MVI   FORCEHED,C'Y'                                                    
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVC   DLCBLEN,PRTSIZE     LEN OF FIELD                                 
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'BDWNGBA   YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD    YES, MAKE SURE NUMERIC FLD NOT ZEROS          
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
*                                                                               
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
*                                                                               
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING PLINED,R3                                                        
         LA    R3,HEAD9                                                         
         MVC   HEAD9,SPACES                                                     
*                                                                               
         MVC   PAGY,=C'AGENCY'                                                  
         CLI   QOPT1,C'Y'                                                       
         BE    PRTH10                                                           
         MVC   PCLI,=C'CLIENT'                                                  
         MVC   PCTY,=C'COUNTRY'                                                 
*                                                                               
PRTH10   MVC   PDESC,=CL36'DESCRIPTION'                                         
         MVC   PMON(L'MONTH),MONTH                                              
         MVC   PYTD(4),=C'JAN-'                                                 
         MVC   PYTD+4(L'MONTH),MONTH                                            
         LA    R3,HEAD10                                                        
         MVC   HEAD10,SPACES                                                    
         MVC   PAGY,=C'------'                                                  
         CLI   QOPT1,C'Y'                                                       
         BE    PRTH20                                                           
         MVC   PCLI,=C'------'                                                  
         MVC   PCTY,=C'-------'                                                 
*                                                                               
PRTH20   MVI   PDESC,C'-'                                                       
         MVC   PDESC+1(L'PDESC-1),PDESC                                         
         MVI   PMON,C'-'                                                        
         MVC   PMON+1(L'PMON-1),PMON                                            
         MVI   PYTD,C'-'                                                        
         MVC   PYTD+1(L'PYTD-1),PYTD                                            
*                                                                               
         XMOD1 1                                                                
         DROP  R3                                                               
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
*                                                                               
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         LA    R5,=C'2D'                                                        
*                                                                               
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(R4),(R5),(C'P',PRINT)                
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(ADDRESS OF DATA),(LENGTH OF DATA)               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*              WORKING STORAGE DSECT                                            
********************************************************************            
*                                                                               
AC10D    DSECT                     WORKING STORAGE DSECT                        
*----------------------                                                         
VTYPES   DS    0A                                                               
ADUMP    DS    A                                                                
ABXHOOK  DS    A                                                                
ADBUFC   DS    A                   BUFFALOC                                     
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNRTE1 DS    A                   DOWNLOAD DETAIL ROUTINE                      
AHD1TAB  DS    A                                                                
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*----------------------                                                         
MONTH    DS    CL6                                                              
DATE3    DS    CL2                 Y/M ONLY                                     
         DS    CL1                                                              
SVDATE3  DS    CL3                                                              
         DS    CL1                                                              
*-------------------                                                            
BUFREC   DS    0CL57                                                            
BUFKEY   DS    0CL5                                                             
BUFAID   DS    CL2                 AGENCY ID                                    
BUFCLI   DS    CL3                 CLIENT CODE                                  
BUFCLN   DS    CL36                CLIENT NAME                                  
BUFBIL   DS    PL8                 CURRENT MONTH BILL AMOUNT                    
BUFYTD   DS    PL8                 YTD BILL AMOUNT                              
*-------------------                                                            
SVCTRY   DS    XL1                                                              
SVCTRYN  DS    CL6                                                              
SVAID    DS    CL2                                                              
SVCLI    DS    CL3                                                              
SVCOM    DS    CL36                                                             
SVAGN    DS    CL36                                                             
MONGROSS DS    PL8                                                              
YTDGROSS DS    PL8                                                              
TOTMGROS DS    PL10                                                             
TOTYGROS DS    PL10                                                             
MTRN     DS    PL6                                                              
*-------------------                                                            
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                    DOWN-LOAD INITIALIZATION                    
DWNEOL   EQU   2                    MARK END OF LINE                            
DWNEOR   EQU   3                    MARK END OF REPORT                          
DWNTEXT  EQU   4                    DOWN-LOAD TEXT                              
DWNNUM   EQU   5                    DOWN-LOAD NUMBER                            
DWNPACK  EQU   6                    DOWN-LOAD NUMBER (PACKED)                   
*                                                                               
DWNLINE  DS    CL(BILDWNQ)                                                      
*                                                                               
FLAG     DS    XL1                                                              
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
*                                                                               
DWNBUF   DS    CL250               DOWNLOAD BUFFER                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
*            DDS BILLING DOWNLOAD RECORD DSECT                        *         
***********************************************************************         
BILDWND  DSECT                                                                  
*                                                                               
BDWNCTY  DS    CL6                 COUNTRY                                      
BDWNAID  DS    CL2                 AGENCY CODE                                  
BDWNAGN  DS    CL36                AGENCY NAME                                  
BDWNCLI  DS    CL3                 CLIENT CODE                                  
BDWNCLN  DS    CL36                CLIENT NAME                                  
BDWNGBA  DS    PL10                GROSS BILL AMOUNT                            
BDWNYTD  DS    PL10                YTD BILL AMOUNT                              
BILDWNQ  EQU    *-BILDWND                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DOWNLOAD HEADLINES TABLE                             *         
***********************************************************************         
         SPACE 1                                                                
HDLND    DSECT                                                                  
HDCTY    DS    CL7                                                              
HDAGY    DS    CL6                                                              
HDAGN    DS    CL4                                                              
HDCLI    DS    CL6                                                              
HDDESC   DS    CL11                                                             
HDMON    DS    CL6                                                              
HDYTD    DS    CL10                                                             
HDLNQ    EQU   *-HDLND                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PRINT LINE                                           *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PAGY     DS    CL6                                                              
         DS    CL2                                                              
PAGN     DS    CL36                                                             
         DS    CL2                                                              
PCTY     DS    CL7                                                              
         DS    CL1                                                              
PCLI     DS    CL6                                                              
         DS    CL1                                                              
PDESC    DS    CL33                                                             
         DS    CL1                                                              
PMON     DS    CL17                                                             
         DS    CL1                                                              
PYTD     DS    CL17                                                             
PLNQ     EQU   *-PLINED                                                         
         EJECT                                                                  
*                                                                               
*        DDBUFFALOD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*                                                                               
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*                                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
*        DDDLCB                                                                 
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
*                                                                               
*        ACREPPROFD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*                                                                               
*        DDMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
*        DDCTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
*                                                                               
*        CTGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREP1002 06/04/04'                                      
         END                                                                    
