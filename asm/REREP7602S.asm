*          DATA SET REREP7602S AT LEVEL 145 AS OF 05/01/02                      
*PHASE RE7602A,*                                                                
*INCLUDE NUMVAL                                                                 
         TITLE 'REREP7602 (RE7602) --- AGENCY LISTING'                          
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP7602 - RE7602 - REP AGENCY LISTING                         *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR13/92 (SKU) --- ADD HEADER DECRIPTION FOR RISK/LIAB FILTERS    *           
*                                                                   *           
* JUN10/92 (SKU) --- PRINT FAX NUMBER FOR DETAIL REPORT             *           
*                    PRINT PROFILE FOR DETAIL REPORT                *           
*                    FIX BATCH PRINTING BUG                         *           
*                    PRINT AGENCY PHONE NUMBER FOR DETAIL REPORT    *           
*                                                                   *           
* SEP12/94 (SKU) --- REMOVE REP'S FILE/STANDARD FILE                *           
*                                                                   *           
* AUG04/95 (SM ) --- OPTIONALLY ONLY DISPLAY '**NOT ENTERED**'      *           
*                                                                   *           
* DEC14/95 (BG )  80 CHANGE REGENALL TO REGENALL1 2K CON            *           
*                                                                   *           
* DEC21/95 (WSB) --- ADD TERRITORY NAME                             *           
*                                                                   *           
* JAN11/96 (WSB) --- ADD KATZ EQUIVALENCY CODE                      *           
*                                                                   *           
* 16APR96  (RHV) --- SUPPORT 34 BYTE AGY ADDRESS FIELDS             *           
*                                                                   *           
* FEB26/97 (DBU)  ACTIVITY DATE FOR INDIVIDUAL RECORD MUST BE       *           
*                 WITHIN REQUEST DATES FOR RECORD TO BE DISPLAYED   *           
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* SEP09/98 (AST) --- FILTER ON LOCAL/NATIONAL OFFICES (QOPTION3)    *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE7602   CSECT                                                                  
         NMOD1 0,**RE7602,R9                                                    
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*********************************************************************           
*              CHECK MODE SETTINGS                                              
*********************************************************************           
         CLI   MODE,REQFRST                                                     
         BNE   AG20                                                             
*                                                                               
         BAS   RE,FILTER           GET RISK/LIAB FILTER(S)                      
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
*                                                                               
* MY NEW CODE FOR LOADOFF HERE                                                  
         CLI   QOPTION3,C'L'                                                    
         BE    *+12                                                             
         CLI   QOPTION3,C'N'                                                    
         BNE   *+8                                                              
         BAS   RE,LOADOFF                                                       
*                                                                               
         CLI   QOPTIONS,C' '                                                    
         BE    AGEXT                                                            
         MVI   RCSUBPRG,1                                                       
         B     AGEXT                                                            
         SPACE 2                                                                
AG20     CLI   MODE,REQLAST                                                     
         BNE   AG30                                                             
         BAS   RE,PRINTEM                                                       
         B     AGEXT                                                            
         SPACE 2                                                                
AG30     CLI   MODE,PROCAGY                                                     
         BNE   AGEXT                                                            
*                                                                               
         CLC   QSTART(6),SPACES                                                 
         BE    AG42                                                             
         MVI   KEY,X'1A'           READ AGY RECORD PART 2                       
         MVC   MYKEY(34),KEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         CLC   KEY(27),MYKEY                                                    
         BE    AG35                                                             
AG33     EQU   *                                                                
         MVI   MYKEY,X'0A'  REASTABLISH SEQUENCE                                
         MVC   KEY(34),MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',MYKEY,KEY,0                   
         B     AGEXT                                                            
AG35     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,MYIOAREA,    +        
               DMWORK                                                           
         LA    R6,MYIOAREA                                                      
         MVI   ELCODE,X'10'        AGENCY FAX ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   AG33                                                             
         USING RAGY2FXE,R6                                                      
*                                                                               
         OC    RAGY2LCD(2),RAGY2LCD   NO DATES?                                 
         BZ    AG33                YES - SKIP IT                                
*                                                                               
         GOTO1 DATCON,DMCB,(2,RAGY2LCD),(3,DATE)                                
         DROP  R6                                                               
         MVI   KEY,X'0A'                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,STDATE)                                
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDATE)                                 
         CLC   STDATE,DATE         IS DATE WITHIN REQUEST DATES?                
         BH    AGEXT               NO - SKIP IT                                 
         CLC   ENDDATE,DATE        IS DATE WITHIN REQUEST DATES?                
         BL    AGEXT               NO - SKIP IT                                 
*                                                                               
AG42     EQU   *                                                                
         OC    AGYRISK,AGYRISK                                                  
         BZ    AG45                                                             
         CLI   AGYRISK,1           RISK 1 = 0, OK CREDIT                        
         BNE   AG43                                                             
         OC    RAGYRISK,RAGYRISK                                                
         BZ    AG45                                                             
AG43     CLC   RAGYRISK,AGYRISK                                                 
         BNE   AGEXT                                                            
*                                                                               
AG45     DS    0H                                                               
         OC    AGYLIAB,AGYLIAB                                                  
         BZ    AG50                                                             
         CLC   RAGYLIAB,AGYLIAB                                                 
         BNE   AGEXT                                                            
*                                                                               
AG50     CLI   QOPTION2,C'Y'     IF OPTION 2 = Y PRINT ONLY RECORDS             
         BNE   AG52                WITH **NOT ENTERED** IN ADDRESS              
         CLC   RAGYADD1(15),=C'**NOT ENTERED**'                                 
         BNE   AGEXT                                                            
*                                                                               
* MY NEW CODE FOR CHKSCP HERE                                                   
AG52     CLI   QOPTION3,C'L'                                                    
         BE    *+12                                                             
         CLI   QOPTION3,C'N'                                                    
         BNE   AG53                                                             
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AGEXT               NO, SKIP IT                                  
*                                                                               
AG53     EQU   *                                                                
         CLC   QOFFICE,SPACES                                                   
         BNH   AG55                                                             
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   AGEXT               NO, SKIP IT                                  
*                                                                               
AG55     MVC   WORK,SPACES                                                      
         MVC   WORK+1(4),RAGYKAGY                                               
         MVC   WORK+8(2),RAGYKAOF                                               
         MVC   WORK+13(20),RAGYNAM1                                             
         BAS   RE,POSTEM                                                        
         CLI   QOPTIONS,C' '                                                    
         BE    AGEXT                                                            
         MVC   ADDR1,SPACES         OPTION TO PRINT ADDRESS AS WELL             
         MVC   ADDR1(20),RAGYADD1                                               
         MVC   ADDR2,SPACES                                                     
         MVC   ADDR3,SPACES                                                     
         MVC   ADDR3(20),RAGYCITY                                               
         MVC   ADDR3+21(2),RAGYSTAT                                             
         MVC   ADDR3+24(10),RAGYZIP                                             
         TM    RAGYFLAG,X'80'        EXTENDED ADDRESS?                          
         BZ    *+8                   NO - SKIP                                  
         BAS   RE,EXTADDR            YES - REPLACE W/EXTENDED ADDRESS           
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK+13(34),ADDR1                                                
         OC    WORK,SPACES                                                      
         BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         OC    ADDR2,SPACES                                                     
         CLC   ADDR2,SPACES                                                     
         BNE   AG55A                                                            
         MVC   ADDR2,ADDR3                                                      
         MVC   ADDR3,SPACES                                                     
AG55A    MVC   WORK+13(L'ADDR2),ADDR2                                           
         OC    WORK,SPACES                                                      
         BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         OC    ADDR3,SPACES                                                     
         CLC   ADDR3(20),ADDR2     FIX PETRY CONVERSION PROBLEM                 
         BE    *+10                                                             
         MVC   WORK+13(L'ADDR3),ADDR3                                           
         BAS   RE,POSTEM                                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK+13(10),RAGYPROS   PROFILE                                   
         OC    RAGYPROS,RAGYPROS                                                
         BNZ   AG56                                                             
         MVC   WORK+13(10),=C'NNNNNNNNNN'                                       
AG56     BAS   RE,POSTEM                                                        
*                                                                               
         BAS   RE,GETFAX           GET FAX/FON NUMBER                           
         MVC   WORK,SPACES                                                      
         OC    FAXNUM,FAXNUM                                                    
         BZ    AG57                                                             
         MVC   WORK+13(4),=C'FAX#'                                              
         MVI   WORK+17,C'('                                                     
         MVC   WORK+18(3),FAXNUM                                                
         MVI   WORK+21,C')'                                                     
         MVC   WORK+22(3),FAXNUM+3                                              
         MVI   WORK+25,C'-'                                                     
         MVC   WORK+26(4),FAXNUM+6                                              
AG57     BAS   RE,POSTEM                                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         OC    PHONENUM,PHONENUM                                                
         BZ    AG58                                                             
         MVC   WORK+13(4),=C'PH.#'                                              
         MVI   WORK+17,C'('                                                     
         MVC   WORK+18(3),PHONENUM                                              
         MVI   WORK+21,C')'                                                     
         MVC   WORK+22(3),PHONENUM+3                                            
         MVI   WORK+25,C'-'                                                     
         MVC   WORK+26(4),PHONENUM+6                                            
AG58     BAS   RE,POSTEM                                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         OC    TERRCODE,TERRCODE   TERRITORY CODE                               
         BZ    AG59                                                             
         MVC   WORK+13(4),=C'TER='                                              
         MVC   WORK+17(2),TERRCODE                                              
         OC    TERRNAME,TERRNAME                                                
         BNZ   *+14                                                             
         MVC   WORK+20(17),=C'**** MISSING ****'                                
         B     AG59                                                             
         MVC   WORK+20(17),TERRNAME                                             
AG59     BAS   RE,POSTEM                                                        
*                                                                               
         MVC   WORK,SPACES                                                      
         OC    KATZEQUI,KATZEQUI   KATZ EQUIVALENCY CODE                        
         BZ    AG65                                                             
         CLC   KATZEQUI,SPACES                                                  
         BE    AG65                                                             
         MVC   WORK+13(6),=C'EQUIV='                                            
         MVC   WORK+19(4),KATZEQUI                                              
AG65     BAS   RE,POSTEM                                                        
*                                                                               
         CLI   QOPTIONS,C'L'       OPTION TO PRINT LEGAL NAME AS WELL           
         BNE   AG100                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(33),RAGYNAM2                                              
         BAS   RE,POSTEM                                                        
         SPACE 2                                                                
AG100    MVC   WORK,SPACES                                                      
         BAS   RE,POSTEM                                                        
         SPACE 2                                                                
AGEXT    XMOD1 1                                                                
         EJECT                                                                  
*********************************************************************           
*              ROUTINE TO GET RISK AND LIAB FILTER CARDS                        
*********************************************************************           
FILTER   NTR1                                                                   
         XC    AGYRISK,AGYRISK                                                  
         XC    AGYLIAB,AGYLIAB                                                  
         XC    RISKHEAD,RISKHEAD                                                
         XC    LIABHEAD,LIABHEAD                                                
*                                                                               
         CLI   QOPTION4,C'+'       2ND CARD HAS FILTERS                         
         BNE   FILTERX                                                          
*                                                                               
         L     R5,VXADDR           GET SECOND REQUEST CARD                      
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NONE, NO FILTER                              
         BNE   FILTERX                                                          
*                                                                               
         L     R2,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R2)                                                     
         DROP  R5                                                               
*                                                                               
         CLI   Q2CREDR,C' '        FILTER FOR CREDIT RISK                       
         BE    FILTER20                                                         
         MVN   AGYRISK,Q2CREDR                                                  
*                                                                               
FILTER20 CLC   Q2LIAB,=C'  '       FILTER FOR LIABILITY POSITION                
         BE    FILTERX                                                          
         LA    RE,Q2LIAB                                                        
         LA    RF,2                                                             
         STM   RE,RF,DMCB                                                       
         GOTO1 =V(NUMVAL),DMCB                                                  
         CLI   DMCB,0                                                           
         BNE   FILTERX                                                          
         CLC   DMCB+4(4),=F'0'                                                  
         BL    FILTERX                                                          
         CLC   DMCB+4(4),=F'100'                                                
         BH    FILTERX                                                          
         MVC   AGYLIAB,DMCB+4+3                                                 
*                                                                               
FILTERX  B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              ROUTINE TO POST TO TABLE                                         
*********************************************************************           
POSTEM   NTR1                                                                   
         SPACE 2                                                                
POST2    LA    R2,PAGETAB                                                       
         LA    R3,80               LINES PER PAGE                               
         CLI   QOPTIONS,C'L'                                                    
         BNE   POST4                                                            
         LA    R3,92               LINES PER PAGE ('L' OPTION)                  
         SPACE 2                                                                
POST4    CLI   0(R2),0                                                          
         BE    POST6                                                            
         LA    R2,55(R2)                                                        
         BCT   R3,POST4                                                         
         BAS   RE,PRINTEM                                                       
         B     POST2                                                            
         SPACE 2                                                                
POST6    MVC   0(55,R2),WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              ROUTINE TO PRINT FROM TABLE                                      
*********************************************************************           
PRINTEM  NTR1                                                                   
         MVC   PMWORK(20),WORK                                                  
         LA    R2,PAGETAB                                                       
         LA    R3,40               LINES PER COLUMN                             
         CLI   QOPTIONS,C'L'                                                    
         BNE   *+8                                                              
         LA    R3,44               LINES PER COLUMN ('L' OPTION)                
         MVI   FORCEHED,C'Y'                                                    
         LA    R7,55                                                            
         LR    R8,R7                                                            
         MH    R8,=H'40'           LINES PER COLUMN                             
         CLI   QOPTIONS,C'L'                                                    
         BNE   PRINT2                                                           
         LR    R8,R7                                                            
         MH    R8,=H'44'           LINES PER COLUMN ('L' OPTION)                
*                                                                               
PRINT2   DS    0H                                                               
         OC    AGYRISK(2),AGYRISK  RISK OR LIAB OPTION??                        
         BZ    PRINT5                                                           
         BAS   RE,HEADER                                                        
*                                                                               
PRINT5   LA    R4,0(R2,R8)                                                      
         MVC   P(55),0(R2)                                                      
         MVC   P+55(55),0(R4)                                                   
         GOTO1 REPORT                                                           
*                                                                               
         XC    0(55,R2),0(R2)                                                   
         XC    0(55,R4),0(R4)                                                   
         AR    R2,R7                                                            
         BCT   R3,PRINT5                                                        
         MVC   WORK(20),PMWORK                                                  
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              ROUTINE TO PRINT FILTER HEADER                                   
*********************************************************************           
HEADER   NTR1                                                                   
         OC    AGYRISK,AGYRISK                                                  
         BZ    HEAD50                                                           
         OC    RISKHEAD,RISKHEAD   NEED HEADER FOR RISK FILTER                  
         BNZ   HEAD20                                                           
         MVC   RISKHEAD(13),=C'CREDIT RATING'                                   
         LA    R2,RISKHEAD                                                      
         EDIT  AGYRISK,(1,14(R2))                                               
*                                                                               
         LA    RE,RISKTAB                                                       
         ZIC   RF,AGYRISK                                                       
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   RISKHEAD+16(40),0(RE)                                            
*                                                                               
HEAD20   MVC   HEAD4+1(L'RISKHEAD),RISKHEAD                                     
*                                                                               
HEAD50   OC    AGYLIAB,AGYLIAB                                                  
         BZ    HEADX                                                            
         OC    LIABHEAD,LIABHEAD   NEED HEADER FOR LIAB POS                     
         BNZ   HEAD60                                                           
         MVC   LIABHEAD(23),=C'LIABILITY POSITION LIAB'                         
         LA    R2,LIABHEAD                                                      
         EDIT  AGYLIAB,(2,23(R2)),FILL=0                                        
         BAS   RE,LIABDESC                                                      
*                                                                               
HEAD60   MVC   HEAD5+1(L'LIABHEAD),LIABHEAD                                     
*                                                                               
HEADX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*          ROUTINE TO GET EXTENDED AGENCY ADDRESS                               
*********************************************************************           
EXTADDR  NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,RAGK2TYQ                                                     
         MVC   KEY+19(8),RAGYKAGY                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXTADDRX                                                         
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'        EXPANDED ADDRESS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   EXTADDRX                                                         
         USING RAGY2AE1,R6                                                      
         MVC   ADDR1(34),RAGY2AD1                                               
         MVC   ADDR2(34),RAGY2AD2                                               
         DROP  R6                                                               
*                                                                               
EXTADDRX DS    0H                  RE-ESTABLISH SEEK ORDER                      
         MVC   KEY,SVKEY                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*          ROUTINE TO GET FAX/PHONE NUMBER AND TERRITORY                        
*********************************************************************           
GETFAX   NTR1                                                                   
         XC    FAXNUM,FAXNUM                                                    
         XC    PHONENUM,PHONENUM                                                
         XC    TERRCODE,TERRCODE                                                
         XC    TERRNAME,TERRNAME                                                
         XC    KATZEQUI,KATZEQUI                                                
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,RAGK2TYQ                                                     
         MVC   KEY+19(8),RAGYKAGY                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GETFAXX                                                          
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
*                                                                               
         LA    R6,IOAREA                                                        
         USING RAGY2D,R6                                                        
         MVC   FAXNUM,RAGY2FAX                                                  
         MVC   PHONENUM,RAGY2FON                                                
         MVC   TERRCODE,RAGY2TER                                                
         MVC   KATZEQUI,RAGY2EQU                                                
         DROP  R6                                                               
*                                                                               
         OC    TERRCODE,TERRCODE                                                
         BZ    GETFAXX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTERKEY,R4          READ TERRITORY RECORD                        
         MVI   RTERKTYP,X'3D'                                                   
         MVC   RTERKREP,QREP                                                    
         MVC   RTERKTER,TERRCODE                                                
         DROP  R4                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEYSAVE(27),KEY                                                  
         BNE   GETFAXX                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'        TERRITORY ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   GETFAXX                                                          
         USING RTERELEM,R6                                                      
         MVC   TERRNAME,RTERNAME                                                
         DROP  R6                                                               
*                                                                               
GETFAXX  DS    0H                  RE-ESTABLISH SEEK ORDER                      
         MVC   KEY,SVKEY                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              ROUTINE TO GET LIABILITY DESCRIPTION                             
*********************************************************************           
LIABDESC NTR1                                                                   
         XC    KEY,KEY             GET LIAB POS COMMENT REC                     
         MVI   KEY,X'2E'                                                        
         MVC   KEY+15(2),RAGYKREP                                               
         OC    KEY+17(2),=X'FFFF'                                               
         MVC   KEY+19(8),=C'LIAB    '                                           
         EDIT  AGYLIAB,(2,KEY+23),FILL=0                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   LDESCX                                                           
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCMTELM2,R6         COMMENT TEXT ELEMENT                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   LDESCX                                                           
*                                                                               
LDESC10  DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    LDESC20                                                          
         CLI   RCMT2TXT,C' '                                                    
         BNE   LDESC20                                                          
         MVI   BYTE,2              COMMENT TEXT ELEMENT                         
         BAS   RE,NEXTEL           R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    LDESC10                                                          
         B     LDESCX                                                           
*                                                                               
LDESC20  DS    0H                                                               
         ZIC   R1,RCMT2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     LDESCX                                                           
         MVC   LIABHEAD+26(0),RCMT2TXT                                          
         DROP  R6                                                               
*                                                                               
LDESCX   B     XIT                                                              
*                                                                               
*********************************                                               
*                                                                               
* CHECK SCOPE FILTER FOR DISPLAYING ADV RECS                                    
*                                                                               
CHKSCP   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
         MVC   MYKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,RAGK2TYQ        GET AGY2 RECORD                              
         MVC   KEY+19(4),MYKEY+19  AGENCY CODE                                  
         MVC   KEY+23(2),MYKEY+23  AGYOF CODE                                   
         MVC   KEY+25(2),MYKEY+25  REP CODE                                     
         OC    KEY+19(8),SPACES                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CSNX                NO SCOPE INFO, IS NATIONAL                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,MYIOAREA,    +        
               DMWORK                                                           
         LA    R6,MYIOAREA                                                      
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   CSNX                NO ELEMS, IS NATIONAL                        
         B     CS11                                                             
CS10     BAS   RE,NEXTEL                                                        
         BNE   CSSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CS11     LA    R1,OFF2TBL          POINT TO TABLE OF OFFICES                    
         B     CSO02                                                            
CSO01    LA    R1,TBENTEQ(R1)      BUMP TO NEXT OFFICE IN TABLE                 
         CLI   0(R1),X'FF' END OF TABLE?                                        
         BNE   CSO02               NO, SKIP                                     
* END OF TABLE, OFFICE MUST BE NATIONAL                                         
         CLI   QOPTION3,C'L'       IF LOCAL FILTER                              
         BNE   CSDX                NO, DISPLAY RECORD                           
         MVI   RTNFLG,1            LOCAL FILTER SET FLAG                        
         B     CS10                CHECK NEXT ELEM                              
CSO02    CLC   0(2,R1),2(R6)       COMPARE OFFICES                              
         BNE   CSO01               CHECK NEXT TABLE ENTRY                       
         CLC   QOPTION3,2(R1)      COMPARE FILTER TO TABLE                      
         BE    CSDX                EQUAL, EXIT W/ FLAG SET TO DISPLAY           
*                                                                               
         B     CS10                CHECK NEXT ELEM                              
*                                                                               
CSNX     DS    0H                  REC HAS NO OFFICES, IS NATIONAL              
         CLI   QOPTION3,C'L'       IS FILTER LOCAL?                             
         BNE   CSDX                NATIONAL/BOTH FILTER, DISPLAY                
*                                  LOCAL FILTER, NATIONAL RECORD, SKIP          
*                                                                               
CSSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
         MVC   KEY(34),MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',MYKEY,KEY,0                   
         B     CSX                 DON'T DISPLAY                                
*                                                                               
CSDX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
         MVC   KEY(34),MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',MYKEY,KEY,0                   
CSX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* CHECK OFFICE FILTER FOR DISPLAYING ADV RECS                                   
*                                                                               
CHKOFF   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         MVC   MYKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,RAGK2TYQ        GET AGY2 RECORD                              
         MVC   KEY+19(4),MYKEY+19 AGENCY CODE                                   
         MVC   KEY+23(2),MYKEY+23 AGYOF CODE                                    
         MVC   KEY+25(2),MYKEY+25 REP CODE                                      
         OC    KEY+19(8),SPACES                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   COSX                NO OFFICE ELEM, DON'T SHOW                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,MYIOAREA,    +        
               DMWORK                                                           
         LA    R6,MYIOAREA                                                      
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   COSX                NO ELEMS, NO OFFICES, SKIP                   
         B     CO11                                                             
CO10     BAS   RE,NEXTEL                                                        
         BNE   COSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CO11     CLC   QOFFICE,2(R6)        IS OFFICE ELEMENT SAME AS FILTER?           
         BE    CODX                YES, DISPLAY RECORD                          
*                                                                               
         B     CO10                CHECK NEXT ELEM                              
*                                                                               
COSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
         MVC   KEY(34),MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',MYKEY,KEY,0                   
         B     COX                                                              
*                                                                               
CODX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
         MVC   KEY(34),MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',MYKEY,KEY,0                   
COX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOAD THE TABLE WITH OFFICES AND SCOPES                                        
*                                                                               
LOADOFF  NTR1                                                                   
         MVC   MYKEY,KEY                                                        
         LA    RE,OFF2TBL          CLEAR OFFICE TABLE                           
         LA    RF,TBLNEQ                                                        
         XCEF                                                                   
*                                                                               
         LA    R4,OFF2TBL                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           OFFICE2 RECORD                               
         MVC   KEY+23(2),QREP      USE QREP IN REQUEST                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     LO11                                                             
LO10     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
LO11     CLC   KEY(25),KEYSAVE     SAME REP?                                    
         BNE   LOX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'        GET OFFICE FAX ELEM                          
         BAS   RE,GETEL                                                         
         BNE   LO10                CHECK NEXT RECORD                            
         USING ROFF2FXE,R6                                                      
*                                                                               
         MVI   2(R4),C'N'          ASSUME NATIONAL                              
         TM    ROFF2PRF+1,X'80'    LOCAL?                                       
         BZ    *+8                 NO, SKIP                                     
         MVI   2(R4),C'L'          YES, IT'S LOCAL                              
         LA    R6,IOAREA                                                        
         USING ROFF2KEY,R6                                                      
         MVC   0(2,R4),ROFF2OFF    STORE OFFICE                                 
*                                                                               
         LA    R4,TBENTEQ(R4)      FILL IN NEXT TABLE ENTRY                     
         B     LO10                                                             
*                                                                               
LOX      DS    0H                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************                                                         
XIT      XIT1                                                                   
***********************                                                         
         GETEL R6,34,ELCODE                                                     
*                                                                               
ELCODE   DS    X                                                                
RTNFLG   DS    CL1                                                              
PMWORK   DS    CL20                                                             
AGYRISK  DS    CL1                                                              
AGYLIAB  DS    XL1                                                              
RISKHEAD DS    CL60                                                             
LIABHEAD DS    CL96                                                             
SVKEY    DS    CL34                                                             
FAXNUM   DS    CL10                                                             
PHONENUM DS    CL10                                                             
TERRCODE DS    CL2                                                              
TERRNAME DS    CL21                                                             
KATZEQUI DS    CL4                                                              
ADDR1    DS    CL34                                                             
ADDR2    DS    CL36                                                             
ADDR3    DS    CL36                                                             
CARD2    DS    CL80                                                             
DATE     DS    CL3                 LAST CHANGED DATE IN BINARY                  
STDATE   DS    CL3                 FROM: REQUEST DATE                           
ENDDATE  DS    CL3                 TO: REQUEST DATE                             
MYKEY    DS    CL34                                                             
         ORG   CARD2                                                            
       ++INCLUDE REGENREQ2                                                      
IOAREA   DS    CL1000                                                           
MYIOAREA DS    CL1000                                                           
*                                                                               
OFF2TBL  DS    0CL300              100 3-BYTE ENTRIES                           
         DS    CL2                 OFF CODE                                     
         DS    CL1                 OFF SCOPE                                    
*                                                                               
TBENTEQ  EQU   *-OFF2TBL LENGTH OF ONE ENTRY                                    
*                                                                               
         DS    99CL3               REST OF ENTRIES                              
*                                                                               
TBLNEQ   EQU   *-OFF2TBL LENGTH OF TABLE                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE RERISKTAB                                                      
         DS    F                                                                
PAGETAB  DC    6000X'00'                                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENOFF2                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
RCMTD    DSECT                                                                  
       ++INCLUDE REGENCMT                                                       
RAGY2D   DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
RTERD    DSECT                                                                  
       ++INCLUDE REGENTER                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145REREP7602S05/01/02'                                      
         END                                                                    
