*          DATA SET REREPDACA  AT LEVEL 086 AS OF 05/01/02                      
*PHASE REDACA,+0                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE DATCON                                                                 
         TITLE 'DAILY ACE CHECKLIST'                                            
*********************************************************************           
* HISTORY OF CHANGES                                                *           
*********************************************************************           
*                                                                   *           
* 11FEB/93 (BU ) --- UPGRADE UTL ACCESS FOR > 1 REP SYSTEM #        *           
*                                                                   *           
* 14MAY/93 (SKU) --- FOR STATION LISTING, PRINT RISK RATING         *           
*                                                                   *           
* 25JUL/95 (BU ) --- SKIP TAKEOVER CONTRACTS, WHICH HAVE NOT SYS ID *           
*                                                                   *           
* 09OCT/95 (BU ) --- DISPLAY ADVERTISER ERROR INFO ON DUMP          *           
*                                                                   *           
* 17NOV/95 (RHV) --- SEPARATE PAGE REPORTING OF 'LATEST VERSION     *           
*                    NOT SENT' RECORDS                              *           
*                                                                   *           
* 04JUN/96 (SKU) --- BYPASS STATIONS WITH MISSING RECEIVING ID      *           
*                                                                   *           
* 15JUL/96 (BU ) --- SOFTEN S/P MISSING ABORT                       *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
REDAC    CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY UTL                                                              
         NBASE 0,**REDAC*,AACESAVE                                              
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING REDAC+4096,RC                                                    
         LA    R4,2048(RC)                                                      
         LA    R4,2048(R4)                                                      
         USING REDAC+8192,R4                                                    
         SPACE 1                                                                
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         SPACE 1                                                                
*********GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
         SPACE 1                                                                
*  GET TODAY'S DATE IN COMPRESSED FORMAT                                        
         GOTO1 =V(DATCON),DMCB,(5,0),(2,COMDATE)                                
*                                                                               
* SEE IF THERE'S A DATE CARD TO OVERRIDE TODAY'S DATE                           
*                                                                               
DATE0010 GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'      TEST FOR END OF FILE                         
         BE    DATE0040                                                         
         CLC   CARD(5),=C'DATE='                                                
         BNE   DATE0020            GET DATE CARD DATE IN CMPRSD FORMAT          
         GOTO1 =V(DATCON),DMCB,(4,CARD+5),(2,COMDATE)                           
         B     DATE0010                                                         
DATE0020 EQU   *                                                                
         CLC   CARD(3),=C'ID='                                                  
         BNE   DATE0030                                                         
         MVC   SAVNAME,CARD+3                                                   
         B     DATE0010                                                         
DATE0030 EQU   *                                                                
         CLC   CARD(6),=C'LOCAL='                                               
         BNE   DATE0010                                                         
         MVC   SAVLOCAL,CARD+6                                                  
         B     DATE0010                                                         
         SPACE 1                                                                
DATE0040 EQU   *                                                                
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*  OPEN REP FILE                                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                   
         SPACE 1                                                                
         MVC   MID1(6),=C'OFFICE'                                               
         MVC   MID2(11),=C'SALESPERSON'                                         
         SPACE 1                                                                
         MVC   SUB1(98),RINHEAD                                                 
         MVC   SUB2(83),INHEAD2                                                 
         MVC   SUB3(105),UNDRLN                                                 
         MVC   TITLE(23),=C'REP DAILY ACE CHECKLIST'                            
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         MVI   SORTSW,C'N'                                                      
         MVI   EOFSW,C'N'                                                       
         SPACE 1                                                                
         B     IN0                                                              
         SPACE 1                                                                
RINHEAD  DC    C'ORIGIN/  MARKET                  ACE #/          ADVERX        
               TISER/           AGENCY/               TOTAL'                    
INHEAD2  DC    C'TIME                             VERSION         PRODUX        
               CT               FLIGHT DATES'                                   
INHEAD3  DC    C'/RISK'                                                         
         SPACE 1                                                                
SINHEAD  DC    C'ORIGIN/  SALESPERSON             ACE #/          ADVERX        
               TISER/           AGENCY/               TOTAL'                    
UNDRLN   DC    C'-------  --------------------    --------        -----X        
               ---------------  --------------------  ------------'             
         SPACE 1                                                                
AACESAVE DC    A(ACESAVE)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
         EJECT                                                                  
*              SORT RECOVERY FILE                                               
         SPACE 2                                                                
IN0      OPEN  (RECVIN,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
*  ONLY WANT REP ACE CONTRACT RECORDS THAT WERE SENT AT LEAST                   
*  ONCE TODAY                                                                   
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
         SPACE 1                                                                
         CLI   RFILTY,X'82'        REP FILE?                                    
         BNE   IN2                                                              
         CLI   RKEY,X'0C'          CONTRACT?                                    
         BNE   IN2                                                              
         CLI   RRECTY,2            CHANGE?                                      
         BNE   IN2                                                              
         SPACE 1                                                                
         LA    RE,RECVHDR-4                                                     
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZERO AT END OF RECORD                    
         SPACE 2                                                                
         LA    R2,RKEY+34          POINT TO 1ST ELEMENT                         
         CLI   0(R2),1                                                          
         BE    IN2A                                                             
***      DC    H'0'                                                             
***      SKIP BAD CONTRACT:                                                     
         B     IN2                                                              
IN2A     EQU   *                                                                
         TM    41(R2),X'80'        ACE                                          
         BZ    IN2                                                              
         SPACE 1                                                                
         SR    RF,RF                                                            
IN5      IC    RF,1(R2)                                                         
         AR    R2,RF               NEXT ELEMENT                                 
         CLI   0(R2),0             END OF RECORD?                               
         BNE   IN5A                                                             
***      DC    H'0'                                                             
***      SKIP BAD CONTRACT                                                      
         B     IN2                                                              
IN5A     EQU   *                                                                
         CLI   0(R2),X'20'         SEND ELEMENT                                 
         BNE   IN5                                                              
         SPACE 1                                                                
         CLC   COMDATE,6(R2)       REP SEND DATE                                
         BE    IN7                                                              
         CLC   COMDATE,15(R2)      STATION SEND DATE                            
         BNE   IN2                                                              
         SPACE 1                                                                
*  IF WE WANT THIS RECORD, FILL IN SORTKEY                                      
         SPACE 1                                                                
IN7      XC    TOTAL,TOTAL                                                      
         XC    SRTREC,SRTREC                                                    
         LA    R2,RKEY+34          POINT TO 1ST ELEMENT                         
         B     IN15                                                             
         SPACE 1                                                                
IN10     SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
IN15     CLI   0(R2),0             END OF RECORD                                
         BE    IN60                                                             
         CLI   0(R2),1                                                          
         BNE   IN20                                                             
         SPACE 1                                                                
         MVC   SRTREP,RKEY+2       REP                                          
         MVC   SRTCON,RKEY+23      CONTRACT                                     
         LH    RE,MYSEQ                                                         
         LA    RE,1(RE)                                                         
         STH   RE,MYSEQ                                                         
         MVC   SRTSEQ,MYSEQ        SEQUENCE NUMBER                              
         MVC   SRTOFF,RKEY+11      OFFICE                                       
         MVC   SRTMAN,27(R2)       SALESPERSON                                  
         MVC   SRTSTA,RKEY+6       STATION                                      
         MVC   SRTDATE,30(R2)      CONTRACT DATES                               
         MVC   SRTADV,RKEY+19      ADVERTISER                                   
         MVC   SRTPRD,22(R2)       PRODUCT CODE                                 
         MVC   SRTAGY,RKEY+13      AGENCY AND AGENCY OFFICE                     
         B     IN10                                                             
         SPACE 1                                                                
IN20     CLI   0(R2),3             BUCKET ELEMENT                               
         BNE   IN30                                                             
         L     RE,TOTAL                                                         
         A     RE,6(R2)                                                         
         ST    RE,TOTAL                                                         
         B     IN10                                                             
         SPACE 1                                                                
IN30     CLI   0(R2),5             PRODUCT NAME                                 
         BNE   IN40                                                             
         MVC   SRTPRDN,2(R2)                                                    
         B     IN10                                                             
         SPACE 1                                                                
IN40     CLI   0(R2),X'1F'         EXTENDED DESCRIPTION ELEMENT                 
         BNE   IN50                                                             
         MVC   SRTCONF,6(R2)       CONFIRMED STATUS                             
         B     IN10                                                             
         SPACE 1                                                                
IN50     CLI   0(R2),X'20'         SEND ELEMENT                                 
         BNE   IN10                                                             
         MVC   SRTRV,5(R2)         REP VERSION #                                
         MVC   SRTSV,14(R2)        STATION VERSION #                            
         MVC   SRTSENF,4(R2)       SEND STATUS                                  
         MVC   SRTRID,2(R2)        REP ID                                       
*                                                                               
         OC    SRTRID,SRTRID       TAKEOVER CONTRACT?                           
         BZ    IN2                 YES - SKIP IT - NOT TO APPEAR                
*                                                                               
         TM    4(R2),X'80'         LAST SENT BY REP                             
         BZ    IN55                                                             
         MVC   SRTDT,6(R2)         DATE (REP)                                   
         MVC   SRTTI,8(R2)         TIME (REP)                                   
         B     IN10                                                             
         SPACE 1                                                                
IN55     MVC   SRTDT,15(R2)        DATE (STATION)                               
         MVC   SRTTI,17(R2)        TIME (STATION)                               
         B     IN10                                                             
         SPACE 1                                                                
IN60     MVC   SRTTOT,TOTAL                                                     
         SPACE 2                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         MVI   SORTSW,C'Y'                                                      
         B     IN2                 GET NEXT RECOVERY RECORD                     
         SPACE 1                                                                
ENDIN1   CLOSE (RECVIN,)                                                        
         EJECT                                                                  
*        LOOK AT SORTED RECOVERY RECORDS, AND PUT LAST ONE                      
*        PER CONTRACT TO SEQUENTIAL DISK FILE FOR 2ND SORT                      
         SPACE 3                                                                
         OPEN  (REPIN,(OUTPUT))                                                 
         OPEN  (STAIN,(OUTPUT))                                                 
         SPACE 1                                                                
         CLI   SORTSW,C'Y'         IF NO RECORDS,                               
         BNE   OUT50               GO DIRECTLY TO CLOSE SORT                    
         SPACE 1                                                                
         XC    SAVREC,SAVREC                                                    
OUT3     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BNZ   OUT10                                                            
         MVI   EOFSW,C'Y'                                                       
         B     OUT30                                                            
         SPACE 1                                                                
OUT10    MVC   SRTREC,0(R6)                                                     
**       GOTO1 =V(PRNTBL),DMCB,=C'SORTOUT',SRTREC,C'DUMP',123,=C'1D'            
         OC    SAVREC,SAVREC                                                    
         BNZ   OUT20                                                            
         MVC   SAVREC,SRTREC       FIRST TIME                                   
         B     OUT3                                                             
         SPACE 1                                                                
OUT20    CLC   SAVREC(6),SRTREC    SAME CONTRACT?                               
         BNE   OUT30                                                            
         MVC   SAVREC,SRTREC                                                    
         B     OUT3                                                             
         SPACE 1                                                                
OUT30    TM    SAVCONF,X'80'       UNCONFIRMED                                  
         BO    OUT40                                                            
         CLI   EOFSW,C'Y'                                                       
         BE    OUT50                                                            
         MVC   SAVREC,SRTREC                                                    
         B     OUT3                                                             
         SPACE 1                                                                
OUT40    DS    0H'0'                                                            
         BAS   RE,GETOFFN          GET OFFICE NAME                              
         MVC   SAVOFFN,WORK                                                     
         BAS   RE,GETADVN          GET ADVERTISER NAME                          
         MVC   SAVADVN,WORK                                                     
         CLC   SAVPRD,SPACES                                                    
         BE    OUT42                                                            
         BAS   RE,GETPRDN          GET PRODUCT NAME                             
         MVC   SAVPRDN,WORK                                                     
         SPACE 1                                                                
OUT42    TM    SAVSENF,X'80'       LAST SENT BY REP                             
         BZ    OUT43                                                            
         BAS   RE,GETRID           GET REP SIGN-ON FROM 2 BYTE ID               
         MVC   SAVSINON,WORK                                                    
         OI    BYTE,X'80'          NEED TO GET STATION 2 BYTE ID                
         BAS   RE,GETSTAN                                                       
         MVC   SAVSID,WORK+20                                                   
         NI    BYTE,X'7F'          NEXT TIME DON'T NEED STATION ID              
         SPACE 1                                                                
         PUT   STAIN,SAVREC                                                     
**       GOTO1 =V(PRNTBL),DMCB,=C'STAOUT',SAVREC,C'DUMP',123,=C'1D'             
         SPACE 1                                                                
OUT43    TM    SAVSENF,X'40'       LAST SENT BY STATION                         
         BO    OUT45                                                            
         TM    SAVSENF,X'20'       REP VERSION NOT ADVANCED                     
         BO    OUT47                                                            
OUT45    PUT   REPIN,SAVREC                                                     
**       GOTO1 =V(PRNTBL),DMCB,=C'REPOUT',SAVREC,C'DUMP',123,=C'1D'             
         SPACE 1                                                                
OUT47    CLI   EOFSW,C'Y'                                                       
         BE    OUT50                                                            
         MVC   SAVREC,SRTREC                                                    
         B     OUT3                                                             
         SPACE 1                                                                
OUT50    GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SPACE 2                                                                
         CLOSE (REPIN,)                                                         
         CLOSE (STAIN,)                                                         
         SPACE 2                                                                
         CLI   SORTSW,C'Y'                                                      
         BE    OUT75                                                            
         B     EOJ                 REPORT IS DONE - NO DATA                     
         EJECT                                                                  
*  PUT RECORDS FROM REP SEQUENTIAL DISK FILE TO SORTER FOR 2ND SORT             
         SPACE 2                                                                
OUT75    MVI   SORTSW,C'N'                                                      
         GOTO1 =V(SORTER),DMCB,SRTCRD2,RECCRD2                                  
         OPEN  (REPIN,(INPUT))                                                  
OUT100   GET   REPIN,SRTREC                                                     
         MVI   SRTSEQ2,1                                                        
         TM    SRTSENF,X'30'                                                    
         BO    *+8                                                              
         MVI   SRTSEQ2,2                                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         MVI   SORTSW,C'Y'                                                      
         B     OUT100                                                           
         SPACE 1                                                                
REPX     CLOSE (REPIN,)                                                         
         SPACE 2                                                                
         CLI   SORTSW,C'Y'         IF NO RECORDS,                               
         BNE   OUT200              GO DIRECTLY TO CLOSE SORT                    
         EJECT                                                                  
*        PRINT REP REPORT                                                       
         SPACE 2                                                                
         XC    SAVREC,SAVREC                                                    
         XC    SAVSEQ2,SAVSEQ2                                                  
         XC    SAVMKTN,SAVMKTN                                                  
         XC    SAVMANN,SAVMANN                                                  
         XC    SAVAGYN,SAVAGYN                                                  
         XC    SAVID,SAVID                                                      
OUT103   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    OUT200              END OF FILE                                  
         SPACE 1                                                                
         MVC   SRTREC,0(R6)                                                     
         MVC   SAVREP,SRTREP                                                    
         OC    SAVID,SAVID         IF NOT THE FIRST TIME,                       
         BZ    OUT104                                                           
         OI    BYTE,X'40'          NEED TO CLOSE PRINT QUEUE                    
         CLC   SAVID,SRTRID       SAME REP ID - (PRINT QUEUE)                   
         BE    OUT105                                                           
OUT104   MVC   SAVID,SRTRID                                                     
         ZAP   PAGE,=P'1'                                                       
         CLI   SAVLOCAL,C'Y'       TEST RUN?                                    
         BE    OUT105              YES - NO REMOTE OUTPUT                       
         BAS   RE,REMOTE                                                        
         SPACE 1                                                                
OUT105   CLI   SAVSEQ2,0           FIRST TIME THRU?                             
         BNE   *+14                                                             
         MVC   SAVSEQ2,SRTSEQ2     INITIALIZE FOR FIRST TIME                    
         B     OUT105A                                                          
         CLC   SAVSEQ2,SRTSEQ2     SAME AS LAST?                                
         MVC   SAVSEQ2,SRTSEQ2     UPDATE LAST                                  
         BE    OUT106              SKIP IF SAME                                 
         ZAP   LINE,=P'99'         NEW PAGE                                     
OUT105A  MVC   TITLE+30(25),SPACES DEFAULT TITLE                                
         CLI   SRTSEQ2,2                                                        
         BNE   *+10                                                             
         MVC   TITLE+30(25),=C'- LATEST VERSION NOT SENT'                       
         SPACE 1                                                                
OUT106   CLC   SRTOFF(5),SAVOFF    SAME OFFICE/SALESPERSON                      
         BE    OUT115                                                           
         ZAP   LINE,=P'99'                                                      
         CLC   SAVOFF,SRTOFF                                                    
         BE    OUT110                                                           
         MVC   SAVOFF,SRTOFF                                                    
         MVC   MID1+14(2),SRTOFF                                                
         MVC   MID1+21(20),SRTOFFN                                              
OUT110   CLC   SRTMAN,SAVMAN                                                    
         BE    OUT115                                                           
         MVC   SAVMAN,SRTMAN                                                    
         BAS   RE,GETMANN          GET SALESPERSON NAME                         
         MVC   SAVMANN,WORK                                                     
         MVC   MID2+14(3),SAVMAN                                                
         MVC   MID2+21(20),SAVMANN                                              
         SPACE 1                                                                
OUT115   CLC   SAVSTA,SRTSTA                                                    
         BE    OUT130                                                           
         MVC   SAVSTA,SRTSTA                                                    
         BAS   RE,GETSTAN          GET MARKET NAME                              
         MVC   SAVMKTN,WORK                                                     
OUT130   CLC   SAVAGY,SRTAGY                                                    
         BE    OUT140                                                           
         MVC   SAVAGY,SRTAGY                                                    
         BAS   RE,GETAGYN          GET AGENCY NAME                              
         MVC   SAVAGYN,WORK                                                     
OUT140   LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   PORIG,SRTSTA                                                     
         UNPK  WORK(9),SRTCON(5)                                                
         MVC   PCON,WORK                                                        
         MVC   PADV,SRTADVN        ADVERTISER NAME                              
         MVC   PAGY,SAVAGYN        AGENCY NAME                                  
         EDIT  (4,SRTTOT),(12,PTOT),2,COMMAS=YES                                
         MVC   PMAN,SAVMKTN                                                     
         SPACE 1                                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R3,P                                                             
         USING P2D,R3              PRINT 2ND LINE                               
         SPACE 1                                                                
         MVC   PTIME(2),SRTTI                                                   
         MVI   PTIME+2,C'.'                                                     
         MVC   PTIME+3(2),SRTTI+2                                               
         CLC   SRTRV,SRTSV                                                      
         BH    OUT160                                                           
         MVC   PVER(3),=C'STA'                                                  
         EDIT  (1,SRTSV),(3,PVER+4),ALIGN=LEFT                                  
         B     OUT170                                                           
OUT160   MVC   PVER(3),=C'REP'                                                  
         EDIT  (1,SRTRV),(3,PVER+4),ALIGN=LEFT                                  
         SPACE 1                                                                
OUT170   TM    SRTSENF,X'30'                                                    
         BO    *+10                                                             
         MVC   PUPPED,=C'**'                                                    
         SPACE 1                                                                
         MVC   PPRD,SRTPRDN        PRODUCT NAME                                 
         GOTO1 =V(DATCON),DMCB,(3,SRTDATE),(5,PDATE)                            
         MVI   PDATE+8,C'-'                                                     
         GOTO1 =V(DATCON),DMCB,(3,SRTDATE+3),(5,PDATE+9)                        
         SPACE 1                                                                
         GOTO1 =V(PRINTER)         PRINT 2ND LINE                               
         SPACE 1                                                                
         GOTO1 =V(PRINTER)         PRINT BLANK LINE                             
         B     OUT103                                                           
         SPACE 1                                                                
OUT200   GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   SORTSW,C'Y'                                                      
         BNE   OUT475                                                           
         CLI   SAVLOCAL,C'Y'       TEST RUN?                                    
         BE    OUT475              YES - DON'T CLOSE PRINT QUEUE                
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'  CLOSE PRINT QUEUE                      
         B     OUT475                                                           
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*  PUT RECORDS FROM STA SEQUENTIAL DISK FILE TO SORTER FOR 2ND SORT             
         SPACE 2                                                                
OUT475   MVI   SORTSW,C'N'                                                      
         NI    BYTE,X'BF'          DON'T CLOSE PRINT QUEUE FIRST TIME           
         GOTO1 =V(SORTER),DMCB,SRTCRD3,RECCRD3                                  
         OPEN  (STAIN,(INPUT))                                                  
OUT500   GET   STAIN,SRTREC                                                     
         MVI   SRTSEQ2,1                                                        
         TM    SRTSENF,X'30'                                                    
         BO    *+8                                                              
         MVI   SRTSEQ2,2                                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         MVI   SORTSW,C'Y'                                                      
         B     OUT500                                                           
         SPACE 1                                                                
STAX     CLOSE (STAIN,)                                                         
         SPACE 2                                                                
         CLI   SORTSW,C'Y'         IF NO RECORDS,                               
         BNE   OUT600              GO DIRECTLY TO CLOSE SORT                    
         EJECT                                                                  
*        PRINT STA REPORT                                                       
         SPACE 2                                                                
         MVC   TITLE(27),=C'STATION DAILY ACE CHECKLIST'                        
         MVC   MID1(7),=C'STATION'                                              
         MVC   MID2(11),=C'OFFICE     '                                         
         MVC   MID2+14(3),SPACES                                                
         MVC   SUB1(98),SINHEAD                                                 
         MVC   SUB2+83(5),INHEAD3  RISK                                         
         XC    SAVREC,SAVREC                                                    
         XC    SAVSEQ2,SAVSEQ2                                                  
         XC    SAVMANN,SAVMANN                                                  
         XC    SAVAGYN,SAVAGYN                                                  
         XC    SAVID,SAVID                                                      
OUT503   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    OUT600              END OF FILE                                  
         SPACE 1                                                                
         MVC   SRTREC,0(R6)                                                     
         MVC   SAVREP,SRTREP                                                    
         OC    SAVID,SAVID         IF NOT THE FIRST TIME,                       
         BZ    OUT503C                                                          
         OI    BYTE,X'40'          NEED TO CLOSE PRINT QUEUE                    
OUT503C  CLC   SAVID,SRTSID        SAME STATION ID (PRINT QUEUE)                
         BE    OUT504                                                           
         MVC   SAVID,SRTSID                                                     
         ZAP   PAGE,=P'1'                                                       
         CLI   SAVLOCAL,C'Y'       TEST RUN?                                    
         BE    OUT504              YES - NO REMOTE OUTPUT                       
         BAS   RE,REMOTE                                                        
         SPACE 1                                                                
OUT504   CLI   SAVSEQ2,0           FIRST TIME THRU?                             
         BNE   *+14                                                             
         MVC   SAVSEQ2,SRTSEQ2     INITIALIZE FOR FIRST TIME                    
         B     OUT504A                                                          
         CLC   SAVSEQ2,SRTSEQ2     SAME AS LAST?                                
         MVC   SAVSEQ2,SRTSEQ2     UPDATE LAST                                  
         BE    OUT504B             SKIP IF SAME                                 
         ZAP   LINE,=P'99'         NEW PAGE                                     
OUT504A  MVC   TITLE+30(25),SPACES DEFAULT TITLE                                
         CLI   SRTSEQ2,2                                                        
         BNE   *+10                                                             
         MVC   TITLE+30(25),=C'- LATEST VERSION NOT SENT'                       
         SPACE 1                                                                
OUT504B  CLC   SAVSTA,SRTSTA       SAME STATION                                 
         BE    OUT505                                                           
         ZAP   LINE,=P'99'         NEW PAGE                                     
         MVC   SAVSTA,SRTSTA                                                    
         MVC   MID1+14(5),SRTSTA                                                
         BAS   RE,GETSTAN                                                       
         MVC   MID1+21(20),WORK                                                 
         SPACE 1                                                                
OUT505   CLC   SAVOFF,SRTOFF       SAME OFFICE                                  
         BE    OUT510                                                           
         ZAP   LINE,=P'99'         NEW PAGE                                     
         MVC   SAVOFF,SRTOFF                                                    
         MVC   MID2+14(2),SRTOFF                                                
         MVC   MID2+21(20),SRTOFFN                                              
         SPACE 1                                                                
OUT510   MVC   SAVSEQ2,SRTSEQ2                                                  
         CLC   SRTMAN,SAVMAN                                                    
         BE    OUT530                                                           
         MVC   SAVMAN,SRTMAN                                                    
         BAS   RE,GETMANN          GET SALESPERSON NAME                         
         MVC   SAVMANN,WORK                                                     
         SPACE 1                                                                
OUT530   CLC   SAVAGY,SRTAGY                                                    
         BE    OUT540                                                           
         MVC   SAVAGY,SRTAGY                                                    
         BAS   RE,GETAGYN          GET AGENCY NAME                              
         MVC   SAVAGYN,WORK                                                     
OUT540   LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   PORIG,SRTSINON                                                   
         UNPK  WORK(9),SRTCON(5)                                                
         MVC   PCON,WORK                                                        
         MVC   PADV,SRTADVN        ADVERTISER NAME                              
         MVC   PAGY,SAVAGYN        AGENCY NAME                                  
         EDIT  (4,SRTTOT),(12,PTOT),2,COMMAS=YES                                
         MVC   PMAN,SAVMANN                                                     
         SPACE 1                                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R3,P                                                             
         USING P2D,R3              PRINT 2ND LINE                               
         SPACE 1                                                                
         MVC   PTIME(2),SRTTI                                                   
         MVI   PTIME+2,C'.'                                                     
         MVC   PTIME+3(2),SRTTI+2                                               
         CLC   SRTRV,SRTSV                                                      
         BH    OUT560                                                           
         MVC   PVER(3),=C'STA'                                                  
         EDIT  (1,SRTSV),(3,PVER+4),ALIGN=LEFT                                  
         B     OUT570                                                           
OUT560   MVC   PVER(3),=C'REP'                                                  
         EDIT  (1,SRTRV),(3,PVER+4),ALIGN=LEFT                                  
         SPACE 1                                                                
OUT570   TM    SRTSENF,X'30'                                                    
         BO    *+10                                                             
         MVC   PUPPED,=C'**'                                                    
         SPACE 1                                                                
         MVC   PPRD,SRTPRDN        PRODUCT NAME                                 
         GOTO1 =V(DATCON),DMCB,(3,SRTDATE),(5,PDATE)                            
         MVI   PDATE+8,C'-'                                                     
         GOTO1 =V(DATCON),DMCB,(3,SRTDATE+3),(5,PDATE+9)                        
         SPACE 1                                                                
         GOTO1 =V(PRINTER)         PRINT 2ND LINE                               
         SPACE 1                                                                
         LA    R3,P                                                             
         USING P3D,R3              PRINT 3RD LINE                               
         CLI   SAVAGYRK,2          PRINT IF CREDIT RISK OTHER THAN OK           
         BL    OUT580              OR MISSING                                   
         CLI   SAVAGYRK,6          SHOULDN'T BE GREATER THAN 6,                 
         BH    OUT580              IF IT IS, SKIP                               
         MVC   PRISKNUM,SAVAGYRK                                                
         MVZ   PRISKNUM(1),=X'F0'  CHANGE TO PRINTABLE NUMBER                   
         LA    RE,RISKTAB          GET APPROPRIATE MESSAGE                      
         ZIC   RF,SAVAGYRK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   PRISKLIT,0(RE)                                                   
         SPACE 1                                                                
         GOTO1 =V(PRINTER)                                                      
         SPACE 1                                                                
OUT580   DS    0H                                                               
         GOTO1 =V(PRINTER)         PRINT BLANK LINE                             
         B     OUT503                                                           
         SPACE 1                                                                
OUT600   GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   SORTSW,C'Y'                                                      
         BNE   EOJ                                                              
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'  CLOSE PRINT QUEUE                      
         B     EOJ                                                              
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
EOJ      XBASE                                                                  
         EJECT                                                                  
*    READ RECORDS                                                               
         SPACE 1                                                                
*              READ THE OFFICES                                                 
         SPACE 1                                                                
GETOFFN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),SAVREP                                                 
         MVC   KEY+25(2),SAVOFF                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETOFF                                                        
         MVC   WORK(20),ROFFNAME                                                
         LR    RE,R5                                                            
         BR    RE                                                               
         SPACE 3                                                                
*              READ THE SALESPEOPLE                                             
         SPACE 1                                                                
GETMANN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),SRTREP                                                 
         MVC   KEY+24(3),SRTMAN                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    GMAN0020                                                         
         MVC   WORK(20),=C'**S/P NOT ON FILE** '                                
         B     GMAN0040                                                         
GMAN0020 EQU   *                                                                
         BAS   RE,GETMAN                                                        
         MVC   WORK(20),RSALNAME                                                
GMAN0040 EQU   *                                                                
         LR    RE,R5                                                            
         BR    RE                                                               
         SPACE 3                                                                
*              READ THE ADVERTISERS                                             
         SPACE 1                                                                
GETADVN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),SAVADV                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
ADV2     CLC   KEY(25),KEYSAVE                                                  
         BE    ADV3                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),SAVADV                                                 
         MVC   KEY+25(2),SAVREP    INCASE OF MASTER/SUBSIDIARY                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    ADV3                                                             
*                                                                               
         MVC   P+1(13),=C'ADVERT ERROR:'                                        
         MVC   P+15(4),SAVADV                                                   
         MVC   P+19(2),SAVREP                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   WORK(13),=C'ADVERT ERROR:'                                       
         MVC   WORK+14(4),SAVADV                                                
         MVC   WORK+18(2),SAVREP                                                
         B     ADV5                                                             
ADV3     EQU   *                                                                
         CLC   KEY+25(2),SAVREP                                                 
         BE    ADV4                                                             
***                                SPECIAL CHECK FOR KATZ TV                    
         CLC   =C'AM',SAVREP                                                    
         BE    ADV3A                                                            
         CLC   =C'CQ',SAVREP                                                    
         BE    ADV3A                                                            
         CLC   =C'NK',SAVREP                                                    
         BNE   ADV3M                                                            
ADV3A    DS    0H                                                               
         CLC   =C'MR',KEY+25                                                    
         BE    ADV4                                                             
***                                                                             
ADV3M    DS    0H                                                               
         CLC   KEY+25(2),=C'ZZ'                                                 
         BNE   ADV7                                                             
ADV4     BAS   RE,GETADV                                                        
         MVC   WORK(20),RADVNAME                                                
ADV5     EQU   *                                                                
         LR    RE,R5                                                            
         BR    RE                                                               
ADV7     BAS   RE,SEQ                                                           
         B     ADV2                                                             
         SPACE 3                                                                
*              READ THE PRODUCT                                                 
         SPACE 1                                                                
GETPRDN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),SAVADV                                                 
         MVC   KEY+22(3),SAVPRD                                                 
         MVC   KEY+25(2),SAVREP                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
PRD2     CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETPRD                                                        
         MVC   WORK(20),RPRDNAME                                                
         LR    RE,R5                                                            
         BR    RE                                                               
         SPACE 3                                                                
*              READ THE AGENCY                                                  
         SPACE 1                                                                
GETAGYN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),SRTAGY                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
AGY2     CLC   KEY(25),KEYSAVE                                                  
         BE    AGY3                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),SRTAGY                                                 
         MVC   KEY+25(2),SRTREP    INCASE OF MASTER/SUBSIDIARY                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    AGY3                                                             
         MVC   P+1(13),=C'AGENCY ERROR:'                                        
         MVC   P+15(6),SRTAGY                                                   
         MVC   P+21(2),SRTREP                                                   
         GOTO1 =V(PRINTER)                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(13),=C'AGENCY ERROR:'                                       
         MVC   WORK+14(6),SRTAGY                                                
         MVC   WORK+20(2),SRTREP                                                
         MVI   SAVAGYRK,1          FORCE 'RISK' VALUE                           
         B     ADV5                                                             
AGY3     EQU   *                                                                
         CLC   KEY+25(2),SRTREP                                                 
         BE    AGY4                                                             
***                                SPECIAL CHECK FOR KATZ TV                    
         CLC   =C'AM',SAVREP                                                    
         BE    AGY3A                                                            
         CLC   =C'CQ',SAVREP                                                    
         BE    AGY3A                                                            
         CLC   =C'NK',SAVREP                                                    
         BNE   AGY3M                                                            
AGY3A    DS    0H                                                               
         CLC   =C'MR',KEY+25                                                    
         BE    AGY4                                                             
***                                                                             
AGY3M    DS    0H                                                               
         CLC   KEY+25(2),=C'ZZ'                                                 
         BNE   AGY7                                                             
AGY4     BAS   RE,GETAGY                                                        
         MVC   WORK(20),RAGYNAM1                                                
         MVC   SAVAGYRK,RAGYRISK   GET AGENCY RISK RATING                       
AGY5     EQU   *                                                                
         LR    RE,R5                                                            
         BR    RE                                                               
AGY7     BAS   RE,SEQ                                                           
         B     AGY2                                                             
         SPACE 3                                                                
*              READ THE STATION FOR MARKET NAME                                 
         SPACE 1                                                                
GETSTAN  LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),SAVREP                                                 
         MVC   KEY+22(5),SAVSTA                                                 
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         BAS   RE,GETSTA                                                        
         MVC   WORK(20),RSTAMKT    GET MARKET NAME                              
         SPACE 1                                                                
         TM    BYTE,X'80'          DO WE NEED TO GET RECEIVING ID?              
         BZ    GETSTAX             NO                                           
         LA    R1,RSTAELEM                                                      
         SR    RF,RF                                                            
GETSTA20 IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),0             END OF FILE                                  
         BNE   GETSTA30                                                         
*         BE    GETSTAX                                                         
*                                                                               
         MVC   P+1(15),=C'STA MISSING ID:'                                      
         MVC   P+17(5),KEY+22                                                   
         GOTO1 =V(PRINTER)                                                      
         B     GETSTAX                                                          
*                                                                               
GETSTA30 DS    0H                                                               
         CLI   0(R1),X'05'         EXTENDED DESCRIPTION ELEMENT                 
         BNE   GETSTA20                                                         
         MVC   WORK+20(2),10(R1)   RECEIVING ID                                 
         SPACE 1                                                                
GETSTAX  LR    RE,R5                                                            
         BR    RE                                                               
         EJECT                                                                  
*              DATA MANAGER INTERFACE (DIRECTORY)                               
         SPACE 3                                                                
HIGH     LA    RF,=C'DMRDHI'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
SEQ      LA    RF,=C'DMRSEQ'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
LINKDIR  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 =V(DATAMGR),DMCB,,=C'REPDIR',KEY,KEY,0                           
         B     DMCHECK                                                          
         EJECT                                                                  
*              DATA MANAGER INTERFACE (FILE GETS)                               
         SPACE 3                                                                
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
GETMAN   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
GETPRD   LA    RF,RPRDREC                                                       
         B     LINKFILE                                                         
GETAGY   LA    RF,RAGYREC                                                       
         B     LINKFILE                                                         
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         GOTO1 =V(DATAMGR),DMCB,(DMINBTS,=C'GETREC'),=C'REPFILE',      X        
               KEY+28,(R2),(0,DMWORK)                                           
         B     DMCHECK                                                          
         SPACE 4                                                                
DMCHECK  TM    DMCB+8,X'FF'                                                     
         BZ    XIT                                                              
         DC    H'0'                DATA MANAGER ERROR                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              READ REP ID RECORD FROM CONTROL FILE                             
GETRID   LR    R5,RE                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),SAVRID                                                 
         MVC   SAVUTL,UTL+4        SAVE PRESENT UTL SE                          
         MVI   UTL+4,X'0A'         INSERT UTL SE OF CONTROL                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,REC                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UTL+4(1),SAVUTL     RESET PRESENT UTL SE                         
         CLC   KEY(25),REC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R1,REC+28                                                        
CTL20    CLI   0(R1),X'02'       TEST DESC ELEMENT                              
         BE    CTL22                                                            
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   CTL20                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
CTL22    MVC   WORK(8),2(R1)      SIGN-ON                                       
         LR    RE,R5                                                            
         BR    RE                                                               
         EJECT                                                                  
*              SET UP FOR REMOTE PRINT QUEUE                                    
         SPACE 2                                                                
REMOTE   LR    R5,RE                                                            
         TM    BYTE,X'40'          DON'T CLOSE ON FIRST TIME                    
         BZ    REMOT5                                                           
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
REMOT5   L     R8,VREMOTEC                                                      
         USING REMOTED,R8                                                       
         MVC   REMOTAOP,=V(PQOPEN)                                              
         MVC   REMOTABF,=V(PQBUFF)                                              
         MVC   REMOTADM,=V(DATAMGR)                                             
         MVC   REMOTDST,SAVID                                                   
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'DAC'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
         MVC   REMOTJID,=C'DAC'    DAILY ACE CHECKLIST                          
         DC    H'0'                                                             
         LR    RE,R5                                                            
         BR    RE                                                               
         SPACE 1                                                                
         DROP  R8                                                               
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(REDAC,65000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 2                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
DUB      DS    D                                                                
KEY      DS    XL32                                                             
KEYSAVE  DS    XL32                                                             
DMWORK   DS    XL96                                                             
TOTAL    DS    F                                                                
MYSEQ    DS    H                                                                
CARD     DS    CL80                                                             
EOFSW    DS    CL1                                                              
SORTSW   DS    CL1                                                              
BYTE     DS    X                   X'80'-GET RECEIVING ID                       
*                                  X'40'-CLOSE PRINT QUEUE                      
SAVNAME  DS    CL10                AGENCY NAME STORAGE FROM CARD I/P            
SAVLOCAL DS    CL1                                                              
SAVUTL   DS    CL1                                                              
SAVSEQ2  DS    CL1                                                              
*                                                                               
DMINBTS  DS    X                                                                
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
UTL      DC    F'0',X'0A'          FOR CONTROL SYSTEM                           
AREC     DC    A(REC)                                                           
COMDATE  DS    XL2                 TODAY'S DATE - COMPRESSED                    
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=126'                                   
         SPACE 2                                                                
*                                                                               
*********  FOR REP REPORT ************                                          
* SRT2 IS BY REP 2 BYTE ID, OFFICE NAME, SALESPERSON CODE, STATION              
* CALL LETTERS, ADVERTISER NAME, AND PRODUCT NAME                               
*                                                                               
SRTCRD2  DC    CL80'SORT FIELDS=(109,2,A,47,20,A,11,3,A,126,1,A,14,5,A,X        
               67,40,A),FORMAT=BI,WORK=1'                                       
RECCRD2  DC    CL80'RECORD TYPE=F,LENGTH=126'                                   
*                                                                               
**********  FOR STATION REPORT ************                                     
* SRT3 IS BY STATION RECVNG 2 BYTE ID, STATION CALL LETTERS, OFFICE             
* NAME, SALESPERSON CODE, REP SIGN-ON ID, ADVERTISER & PRODUCT NAME             
*                                                                               
SRTCRD3  DC    CL80'SORT FIELDS=(107,2,A,14,5,A,47,20,A,126,1,A,11,3,A,X        
               19,8,A),FORMAT=BI,WORK=1'                                        
RECCRD3  DC    CL80'RECORD TYPE=F,LENGTH=126'                                   
*                                                                               
REPIN    DCB   DDNAME=REPIN,DSORG=PS,RECFM=FB,LRECL=126,               X        
               BLKSIZE=12600,MACRF=(GM,PM),EODAD=REPX                           
*                                                                               
STAIN    DCB   DDNAME=STAIN,DSORG=PS,RECFM=FB,LRECL=126,               X        
               BLKSIZE=12600,MACRF=(GM,PM),EODAD=STAX                           
*                                                                               
*                                                                               
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4200,             X        
               MACRF=(GM,),EODAD=ENDIN1                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SORTREC'                                                      
SRTREC   DS    0XL126              SORTREC                                      
SRTKEY   DS    0XL8                SORTKEY                                      
*                                                                               
SRTREP   DS    CL2                 REP                                          
SRTCON   DS    XL4                 CONTRACT NUMBER                              
SRTSEQ   DS    CL2                 SEQUENCE NUMBER                              
*                                                                               
SRTOFF   DS    CL2                 OFFICE                                       
SRTMAN   DS    CL3                 SALESPERSON                                  
SRTSTA   DS    CL5                 STATION                                      
SRTSINON DS    CL8                 REP SIGN-ON FROM 2 BYTE ID                   
SRTADV   DS    CL4                 ADVERTISER                                   
SRTPRD   DS    CL3                 PRODUCT                                      
SRTAGY   DS    CL6                 AGENCY AND AGENCY OFFICE                     
SRTDATE  DS    CL6                 CONTRACT DATES                               
SRTCONF  DS    XL1                 CONFIRMED STATUS                             
SRTOFFN  DS    CL20                OFFICE NAME                                  
SRTADVN  DS    CL20                ADVERTISER NAME                              
SRTPRDN  DS    CL20                PRODUCT NAME                                 
SRTLEN   EQU   *-SRTREC            SORT LENGTH                                  
*                                                                               
SRTSEND  DS    0XL15               SEND STATUS                                  
SRTSID   DS    CL2                 STATION RECEIVING ID                         
SRTRID   DS    CL2                 REP ID                                       
SRTSENF  DS    XL1                 VERSION UPPED/LAST SENT BY                   
SRTRV    DS    CL1                 REP VERSION #                                
SRTSV    DS    CL1                 STATION VERSION #                            
SRTDT    DS    CL2                 LATEST SEND DATE                             
SRTTI    DS    CL6                 LATEST SEND TIME                             
*                                                                               
SRTTOT   DS    CL4                 CONTRACT TOTAL                               
SRTSEQ2  DS    CL1                                                              
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*SAVEREC'                                                      
SAVREC   DS    0XL126              SORTREC                                      
SAVKEY   DS    0XL8                SORTKEY                                      
*                                                                               
SAVREP   DS    CL2                 REP                                          
SAVCON   DS    XL4                 CONTRACT NUMBER                              
SAVSEQ   DS    CL2                 SEQUENCE NUMBER                              
*                                                                               
SAVOFF   DS    CL2                 OFFICE                                       
SAVMAN   DS    CL3                 SALESPERSON                                  
SAVSTA   DS    CL5                 STATION                                      
SAVSINON DS    CL8                 REP SIGN-ON FROM 2 BYTE ID                   
SAVADV   DS    CL4                 ADVERTISER                                   
SAVPRD   DS    CL3                 PRODUCT                                      
SAVAGY   DS    CL6                 AGENCY                                       
SAVDATE  DS    CL6                 CONTRACT DATES                               
SAVCONF  DS    XL1                 CONFIRMED STATUS                             
SAVOFFN  DS    CL20                OFFICE NAME                                  
SAVADVN  DS    CL20                ADVERTISER NAME                              
SAVPRDN  DS    CL20                PRODUCT NAME                                 
*                                                                               
SAVSEND  DS    0XL15               SEND STATUS                                  
SAVSID   DS    CL2                 STATION RECEIVING ID                         
SAVRID   DS    CL2                 REP ID                                       
SAVSENF  DS    XL1                 VERSION UPPED/LAST SENT BY                   
SAVTRV   DS    CL1                 REP VERSION #                                
SAVTSV   DS    CL1                 STATION VERSION #                            
SAVDT    DS    CL2                 LATEST SEND DATE                             
SAVTI    DS    CL6                 LATEST SEND TIME                             
*                                                                               
SAVTOT   DS    CL4                 CONTRACT TOTAL                               
         DS    CL1                 SPARE                                        
         SPACE 3                                                                
SAVMANN  DS    CL20                SALESPERSON NAME                             
SAVAGYN  DS    CL20                AGENCY NAME                                  
SAVMKTN  DS    CL20                MARKET NAME                                  
SAVID    DS    CL2                 ID FOR PRINT QUEUE                           
SAVAGYRK DS    X                   AGENCY RISK RATING                           
         EJECT                                                                  
       ++INCLUDE RERISKTAB                                                      
         SPACE 3                                                                
       ++INCLUDE REGENOFF                                                       
         SPACE 3                                                                
       ++INCLUDE REGENSTA                                                       
         SPACE 3                                                                
       ++INCLUDE REGENSAL                                                       
         SPACE 3                                                                
       ++INCLUDE REGENADV                                                       
         SPACE 3                                                                
       ++INCLUDE REGENPRD                                                       
         SPACE 3                                                                
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
         DS    F                   LENGTH OF RECOVERY RECORD                    
         SPACE 1                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 2                                                                
RKEY     DS    0XL27                                                            
REC      DS    4200X                                                            
         SPACE 1                                                                
ACESAVE  DS    3000D                                                            
         SPACE 4                                                                
       ++INCLUDE DDREMOTED                                                      
         SPACE 4                                                                
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
*              DSECT FOR 1ST PRINT LINE                                         
         SPACE 2                                                                
P1D      DSECT                                                                  
PORIG    DS    CL5                                                              
         DS    CL4                                                              
PMAN     DS    CL20                                                             
         DS    CL4                                                              
PCON     DS    CL8                                                              
         DS    CL8                                                              
PADV     DS    CL20                                                             
         DS    CL2                                                              
PAGY     DS    CL20                                                             
         DS    CL2                                                              
PTOT     DS    CL12                                                             
         DS    CL5                                                              
         SPACE 3                                                                
*              DSECT FOR 2ND PRINT LINE                                         
P2D      DSECT                                                                  
PTIME    DS    CL5                                                              
         DS    CL26                                                             
PUPPED   DS    CL2                                                              
PVER     DS    CL7                                                              
         DS    CL9                                                              
PPRD     DS    CL20                                                             
         DS    CL2                                                              
PDATE    DS    CL17                                                             
         DS    CL22                                                             
         SPACE 3                                                                
*              DSECT FOR 3RD PRINT LINE                                         
P3D      DSECT                                                                  
         DS    CL71                                                             
PRISKNUM DS    CL1                                                              
         DS    CL1                                                              
PRISKLIT DS    CL40                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086REREPDACA 05/01/02'                                      
         END                                                                    
