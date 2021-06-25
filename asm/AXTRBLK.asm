*          DATA SET AXTRBLK    AT LEVEL 109 AS OF 10/20/20                      
*PHASE AXTRBLKC                                                                 
*INCLUDE AXTRBROT                                                               
*INCLUDE AXTRBCNV                                                               
*INCLUDE AXTRBVND                                                               
*INCLUDE LOADER                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE DDWTO                                                                  
         TITLE 'AXTRBLK - EXTRACT BLOCKCHAIN'                                   
***********************************************************************         
* BLOCKCHAIN EXTRACT CONTROL MODULE - BLK SUBSYSTEM                   *         
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                     *         
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
*   DXOPENQ  - OPEN SYSTEM FILES                                      *         
*   DXCLOSEQ - CLOSE SYSTEM FILES                                     *         
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE                        *         
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE                      *         
*                                                                     *         
* FOR DXLOADQ AND DXUPDTQ MODES,                                      *         
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                                 *         
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE - SEE DSECT      *         
*                                                      SYSTABD)       *         
*                                                                     *         
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK               *         
*                                                                     *         
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM=                          *         
* DXUSER(1)=C'M' FOR MASTER RECORD EXTRACTS FROM TAPE                 *         
* DXUSER+00 = YYMMDD-YYMMDD FOR TRANSACTION EXTRACTS                  *         
* DXUSER+00 = C'MONTHLY' OR C'WEEKLY'                                 *         
*                                                                     *         
***********************************************************************         
* GHOA 002 2018NOV16 FIX COMMISSION REPEATING DOWN                              
*                    FIX MP FOR BUFFER GROSS AND NET                            
* GHOA 003 2018NOV17 CHANGES FOR PAYBABLES                                      
* GHOA 004 2018DEC11 TEMP CODE: SKIP NO MEDIA AUTH ONES                         
* GHOA 005 2018DEC14 TEMP CODE: ONLY PRINT GOES TO AXVNDR                       
* GHOA 006 2018DEC14 REMOVE LEVEL 4 CHANGES, NO MEDIA AUTH OK NOW               
* GHOA 007 2018DEC18 USE AGENCY IF NO MEDIA AGY FOUND                           
* GHOA 008 2019FEB01 SUPPORT CASH RECEIPTS                                      
*          2019APR12 SUPPORT CASH PAY                                           
* GHOA 027 2020JAN31 USE SJ NAMES IF NONE FROM AXTRBVND                         
***********************************************************************         
         EJECT                                                                  
AXTRBLK  CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,AXTRBLK*                                                   
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     RA,=A(ADDRESS)                                                   
         USING ADDRESSD,RA                                                      
         ST    RA,AADDRESS                                                      
         L     RF,=V(HELLO)                                                     
         ST    RF,VHELLO                                                        
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
*                                                                               
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
         ST    R7,VDXBLOCK                                                      
*                                                                               
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         ST    R6,VDXTAB                                                        
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         ICM   RF,15,=V(DDSIO)                                                  
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         MVC   AGENCY,SXDTAGY                                                   
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MCLOX                                                            
         BRAS  RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOX    CLI   DXMODE,DXCLOXDQ                                                  
         BNE   MUPDTEND                                                         
         BRAS  RE,PROCCLOX         CLOSE EXTRACT FILE MODE                      
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDTEND CLI   DXMODE,DXENDQ                                                    
         BNE   MERR                                                             
         B     MXIT                                                             
*                                                                               
MERR     MVI   RTNCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XIT1                                                                   
         EJECT                                                                  
*==================================================================             
* GENERAL INITIALISATION                                                        
*==================================================================             
                                                                                
GENINIT  NTR1                                                                   
*                                                                               
         OC    VVNDBUFF,VVNDBUFF                                                
         JNZ   GENIN10                                                          
         ICM   R0,15,=AL4(VBUFFSZQ) VENDOR BUFFER SIZE                          
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,VVNDBUFF                                                      
*                                                                               
GENIN10  CLI   DXMODE,DXOPENQ                                                   
         JNE   GENINX                                                           
         ZAP   SEQNUM,=P'0'                                                     
         ZAP   TOTALDR,=P'0'                                                    
         ZAP   TOTALCR,=P'0'                                                    
GENINX   J     MXIT                                                             
         EJECT                                                                  
*==================================================================             
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                                  
*==================================================================             
                                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
                                                                                
         L     RE,VUTL                                                          
         MVI   4(RE),SYSCONQ                                                    
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,CNTRL,CTFILES,IOL                           
*                                                                               
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
         XC    IOKEY,IOKEY         GET DTF ADDRESS                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,ACCDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCOUNT,ACCFILES,IO                         
*                                                                               
         CLC   =C'MONTHLY',DXUSER  TEST MONTHLY REQUEST                         
         JNE   PROCOP2                                                          
* GET START/END DATES OF PREVIOUS MONTH                                         
         GOTO1 VDATCON,DMCB,(5,0),WORK       GET TODAY'S DATE                   
         GOTO1 (RF),(R1),(X'30',WORK),(X'20',DXUSER),(4,0)  START PRV           
         GOTO1 (RF),(R1),,(X'20',DXUSER+6),(5,0)            END PRV             
         B     PROCOP4                                                          
*                                                                               
PROCOP2  CLC   =C'WEEKLY',DXUSER   TEST WEEKLY REQUEST                          
         JNE   PROCOP4                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',WORK)   GET TODAY'S DATE               
         MVC   DXUSER+6(6),WORK                  MOVE IT                        
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
         CLI   0(R1),7                   TEST SUNDAY                            
         JE    PROCOP2A                                                         
*                                                                               
         LLC   R0,0(R1)                  BACK UP TO SUNDAY                      
         LNR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,(X'20',DXUSER+6),(R0)                           
*                                                                               
PROCOP2A LA    R0,6                      NOW BACK UP TO MONDAY                  
         LNR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,DXUSER+6,(X'20',DXUSER),(R0)                         
*                                                                               
PROCOP4  CLI   DXUSER,C'0'         TEST DATE PRESENT                            
         JL    PROCOP6                                                          
         GOTO1 VDATCON,DMCB,DXUSER,(2,MYSDATE2)                                 
         GOTO1 (RF),(R1),DXUSER+6,(2,MYEDATE2)                                  
*                                                                               
PROCOP6  OC    MYSDATE2,MYSDATE2                                                
         JZ    PROCOP10                                                         
*                                                                               
         MVI   RECTYPE,X'FD'       SET HEADER REC                               
         BRAS  RE,INITALL                                                       
*                                                                               
         L     RF,=V(AXFRH)        RECON HEADER ROUTINE                         
         GOTO1 (RF),DMCB,(RC),(C'L',DXAXREC),0                                  
*                                                                               
         GOTO1 VAXCNV,DMCB,(RC),(C'L',DXAXREC)                                  
*                                                                               
         L     R0,DXASQLB                                                       
         GOTO1 DXPUT,DMCB,(R0),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
PROCOP10 CLI   DXUSER+12,C'T'      TEST MAX RECS/LEDGER                         
         BNE   PROCOPX                                                          
         MVC   DXMAXREC,=F'100'                                                 
         CLI   DXUSER,C'0'         TEST DATE PRESENT                            
         JL    PROCOPX                                                          
         MVC   DXMAXREC,=F'1000'                                                
*                                                                               
PROCOPX  J     YES                                                              
*                                                                               
CNTRL    DC    CL8'CONTROL '                                                    
CTFILES  DC    CL8'NCTFILE '                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
*==================================================================             
* DXMODE = DXCLOSEQ - PUT OUT FILE TOTAL RECORD AND                             
*                     CLOSE ACCOUNT SYSTEM FILES                                
*==================================================================             
                                                                                
PROCCLOS NTR1  ,                                                                
*                                                                               
         MVC   WORK,MYSPACES                                                    
         MVC   WORK(12),=CL12'NARR COUNTS '                                     
*                                                                               
         LA    R1,NARCOUNT                                                      
         LA    R4,WORK+14                                                       
         LA    R5,5                                                             
*                                                                               
PROCCL2  L     R0,0(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R4),DUB                                                      
*                                                                               
         LA    R1,4(R1)            NEXT COUNTER                                 
         LA    R4,10(R4)           NEXT OUTPUT                                  
         JCT   R5,PROCCL2                                                       
*                                                                               
         GOTO1 =V(DDWTO),DMCB,WORK,(X'80',0)                                    
*                                                                               
         MVI   RECTYPE,X'FE'       SET TOTAL  REC                               
         BRAS  RE,INITALL                                                       
*                                                                               
         L     RF,=V(AXTOTS)       MASTER DATA ROUTINE                          
         GOTO1 (RF),DMCB,(RC),(C'L',DXAXREC),0                                  
         CLI   8(R1),X'FF'                                                      
         JE    PROCCL4                                                          
*                                                                               
         GOTO1 VAXCNV,DMCB,(RC),(C'L',DXAXREC)                                  
*                                                                               
         L     R0,DXASQLB                                                       
         GOTO1 DXPUT,DMCB,(R0),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
PROCCL4  L     RE,VUTL                                                          
         MVI   4(RE),SYSCONQ                                                    
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
*                                                                               
PROCCL6  L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,ACCOUNT,0,IO                                
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFILES DC    C'NACCDIR NACCMST NACCARC NACCRCV X'                             
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
*==================================================================             
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                                        
*==================================================================             
                                                                                
PROCLOAD NTR1  ,                                                                
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   TYPECODE,SXDTTYP                                                 
         GOTOR GETTYP              SET UP RECORD TYPE TABLE DATA                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
         SPACE 1                                                                
*                                                                               
*==================================================================             
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD                   
*                                                                               
* CL3    TYPE NAME                                                              
* AL1    DEPTH TO READ TO FOR INCREMENT ON RDHI FOR LEDGER LOAD                 
* AL1    TYPE FLAGS                                                             
* AL3    N/D                                                                    
* AL4    LOAD ROUTINE ADDRESS                                                   
* AL4    UPDATE ROUTINE ADDRESS                                                 
*==================================================================             
                                                                                
* NOTE THAT NAMES ARE 3 CHARS BECAUSE DYNALLOC CAN'T HANDLE                     
* TWO CHARACTER NAMES!                                                          
         SPACE 1                                                                
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,0)                      
         DC    CL3'SBX',AL1(00,00,00,00,00),AL4(LOADSB,0)                       
         DC    CL3'SCX',AL1(00,00,00,00,00),AL4(LOADSC,0)                       
         DC    CL3'SEX',AL1(00,00,00,00,00),AL4(LOADSE,0)                       
         DC    CL3'SGX',AL1(00,00,00,00,00),AL4(LOADSG,0)                       
         DC    CL3'SIX',AL1(00,00,00,00,00),AL4(LOADSI,0)                       
         DC    CL3'SJX',AL1(00,00,00,00,00),AL4(LOADSJ,0)                       
         DC    CL3'SPX',AL1(00,00,00,00,00),AL4(LOADSP,0)                       
         DC    CL3'SQX',AL1(00,00,00,00,00),AL4(LOADSQ,0)                       
         DC    CL3'SRX',AL1(00,00,00,00,00),AL4(LOADSR,0)                       
         DC    CL3'SSX',AL1(00,00,00,00,00),AL4(LOADSS,0)                       
         DC    CL3'STX',AL1(00,00,00,00,00),AL4(LOADST,0)                       
         DC    CL3'SUX',AL1(00,00,00,00,00),AL4(LOADSU,0)                       
         DC    CL3'SVX',AL1(00,00,00,00,00),AL4(LOADSV,0)                       
         DC    CL3'SWX',AL1(00,00,00,00,00),AL4(LOADSW,0)                       
         DC    CL3'SXX',AL1(00,00,00,00,00),AL4(LOADSX,0)                       
         DC    CL3'SYX',AL1(00,00,00,00,00),AL4(LOADSY,0)                       
         DC    CL3'SZX',AL1(00,00,00,00,00),AL4(LOADSZ,0)                       
         DC    X'FF'                                                            
         EJECT                                                                  
*===============================================================                
* LOAD ALL RECORD DATA                                                          
*===============================================================                
         SPACE 1                                                                
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD10   CLI   0(R3),0                                                          
         JE    YES                                                              
         OC    MYSDATE2,MYSDATE2   TEST LOADING TRANSACTION DATA                
         JNZ   LOAD14                                                           
         TM    3(R3),X'01'         TEST IGNORE FOR MASTER DATA                  
         JO    LOAD30                                                           
*                                                                               
LOAD14   MVC   TYPECODE,0(R3)                                                   
*                                                                               
LOAD20   GOTOR GETTYP                                                           
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD30   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD10                                                           
*                                                                               
* X'01'=IGNORE FOR MASTER DATA                                                  
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'SBX',X'01',AL4(LOADSB)                                       
*        DC    CL3'SJX',X'01',AL4(LOADSJ)                                       
         DC    CL3'SPX',X'00',AL4(LOADSP)                                       
         DC    CL3'SQX',X'00',AL4(LOADSQ)                                       
         DC    CL3'SRX',X'00',AL4(LOADSR)                                       
         DC    CL3'SSX',X'00',AL4(LOADSS)                                       
         DC    CL3'STX',X'00',AL4(LOADST)                                       
         DC    CL3'SUX',X'00',AL4(LOADSU)                                       
*        DC    CL3'SVX',X'00',AL4(LOADSV)                                       
*        DC    CL3'SWX',X'00',AL4(LOADSW)                                       
*        DC    CL3'SYX',X'00',AL4(LOADSY)                                       
*        DC    CL3'SZX',X'01',AL4(LOADSZ)                                       
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* LOAD DATA BY LEDGER                                                           
* IF START/END DATE PROVIDED, LOAD TRANSACTIONS IN THAT DATE RANGE              
* IF NO DATES, LOAD ACCOUNT DATA ONLY                                           
*===============================================================                
                                                                                
LOADSB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'B'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'C'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'E'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'G'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSJ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'P'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'Q'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'R'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'S'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'T'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSU   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'U'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'V'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'W'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'X'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'Y'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LOADSZ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'Z'                                                      
         BRAS  RE,LODSUB                                                        
         J     EXIT                                                             
*                                                                               
LODSUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SRLDGCLT,C' '       TEST READ SR                                 
         JH    LODSUB2                                                          
         IC    R0,LEDGER                                                        
         MVI   LEDGER,C'R'                                                      
         GOTOR RDLEDG,0                                                         
         MVC   SRLDGCLT,LDGCLT     SAVE CLT DSPL IN SR LDGR                     
         STC   R0,LEDGER           RESTORE ORIGINAL LEDGER                      
*                                                                               
LODSUB2  GOTOR RDLEDG,1            GO READ LEDGER LEVELS                        
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         MVI   IOKEY,C' '                                                       
         MVC   IOKEY+1(L'IOKEY-1),IOKEY                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTOR READHI              INITIAL READ HIGH                            
         JNE   NO                  CC EQ IF NOT FOUND                           
*                                                                               
LODSUB10 TM    DMCB+8,X'80'       ALL DONE IF EOF                               
         JO    YES                                                              
         CLC   ACTKEY(3),IOKEY                                                  
         JNE   YES                 ALL DONE IF C/U/L CHANGES                    
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
                                                                                
         GOTOR ACCLOAD,PLIST,,AINITALL,AFILTLOD                                 
         JE    LODSUB10            CONTINUE - MAXIOS NOT REACHED                
*                                                                               
         CLI   DXUSER+12,C'T'      IF TESTING, NEXT LEDGER!                     
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* FILTER DIRECTORY POINTERS FOR LOAD                                            
*===========================================================                    
                                                                                
         USING ACTRECD,R2                                                       
FILTLOD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VRECTYP,DMCB,(C'I',ACTRECD)                                      
         MVC   RECTYPE,0(R1)       SAVE RECORD TYPE                             
         DROP  R2                                                               
*                                                                               
         USING TRNRECD,R2                                                       
         OC    MYSDATE2,MYSDATE2   FILTERING ON DATE?                           
         JNZ   FLOD10              YES - PROCESS TRANSACTIONS                   
                                                                                
* LOADING MASTER RECORDS                                                        
                                                                                
         CLI   RECTYPE,ACRTTRN     ELSE IF THIS IS A TRANSACTION                
         JE    NO                   SKIP IT                                     
         CLC   TRNKWORK(27),MYSPACES   REST OF KEY MUST BE SPACES!              
         JNE   NO                                                               
         CLC   =C'SJ',TRNKUNT      IF SJ                                        
         JNE   FLOD02                                                           
         CLI   TRNKACT,C' '        MAKE SURE CLIENT IS THERE                    
         JNH   NO                                                               
*        CLI   TRNKACT+6,C' '      BUT NOTHING AFTER PRODUCT                    
*        JH    NO                                                               
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         USING ACTRECD,R2                                                       
FLOD02   CLC   ACTKACT,MYSPACES    THIS MUST NOT BE EMPTY                       
         JE    NO                                                               
*                                                                               
         TM    ACTKSTA,ACTSABLP    LOW LEVEL ACCT NEEDS BAL ELEM                
         JZ    NO                                                               
*                                                                               
         TM    ACTKSTA,ACTSDRFT    EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
FLOD10   CLI   RECTYPE,ACRTTRN     TEST TRANSACTION (TYPE=10)                   
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*====================================================================           
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                            
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                                       
* P1 = A(EXTRACT ROUTINE)                                                       
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                                 
* P3 = NOT PASSED - DETERMINED BY RECTYPE                                       
*====================================================================           
                                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         LTR   RF,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (RF)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   GOTOR GETIT               GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA03   LR    RF,R4               INITIALISE EXTRACT BUFFER                    
         GOTO1 (RF)                                                             
*                                  CALL RECORD EXTRACT ROUTINE                  
         L     RF,=V(AXACCT)       MASTER DATA ROUTINE                          
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION (TYPE=10)                   
         JNE   *+8                                                              
         L     RF,=V(AXTRNS)                                                    
         GOTO1 (RF),DMCB,(RC),(C'L',DXAXREC),(R2),(R6)                          
*                                                                               
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
         L     R0,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ALOA04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VAXCNV,DMCB,(RC),(C'L',DXAXREC)                                  
         L     R0,DXASQLB                                                       
*                                                                               
ALOA04   GOTO1 DXPUT,DMCB,(R0),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
ALOA06   GOTOR DECIOC              DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
                                                                                
*                                                                               
         MVC   IOKEY(L'ACTKEY),0(R2) READ NEXT RECORD - SEQUENTIAL              
         GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                                 
*==================================================================             
         SPACE 1                                                                
CHKSEQIO NTR1  BASE=*,LABEL=*                                                   
         L     R2,DTFADDR                                                       
         USING ISDTF,R2                                                         
         L     RF,ISPDKEY                                                       
         LH    RE,ISKEYLN1                                                      
         EX    RE,SEQCLC                                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
SEQCLC   CLC   IOKEY(0),0(RF)                                                   
         DROP  R2                                                               
         EJECT                                                                  
*==================================================================             
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                                   
*==================================================================             
         SPACE 1                                                                
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP04                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP02                                                           
*                                                                               
GTYP04   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RF                                                               
         EJECT                                                                  
*==============================================================                 
* DECREMENT MAXIMUM IO COUNT                                                    
*==============================================================                 
         SPACE 1                                                                
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
         CLI   DXUSER+12,C'T'      TESTING?                                     
         JE    NO                                                               
*                                                                               
         LA    R3,DECMSG           OUTPUT IO COUNT EXCEEDED MESSAGE             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
         EJECT                                                                  
*===============================================================                
* CALL DMGR TO GET A RECORD                                                     
* NTRY: R2           = A(RECORD BUFFER)                                         
*       ACCADDR      = DISK ADDRESS TO READ                                     
* EXIT: CC EQUAL     = RECORD READ OK                                           
*       CC NOT EQUAL = ERROR ON READ                                            
*===============================================================                
         SPACE 1                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
         LA    R0,ACCMST                                                        
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),(R0),ACCADDR,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
         GOTO1 VHEXOUT,PARM,ACCADDR,GETDA,L'ACCADDR                             
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSG           OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ LEDGER RECORD TO FIND LENGTHS OF SUB RECORD TYPES              *         
*                                                                     *         
* THIS ROUTINE EXPECTS COMPANY,UNIT,LEDGER TO BE IN THEIR NAMESAKES   *         
* IT RETURNS THE 4 LENGTHS (OR 0) IN L1 TO L4                         *         
***********************************************************************         
         SPACE 1                                                                
RDLEDG   NTR1  BASE=*,LABEL=*                                                   
         STC   R1,BYTE                                                          
*                                                                               
RLED20   LA    R2,IOKEY                                                         
         USING LDGRECD,R2                                                       
         MVI   LDGKEY,C' '                                                      
         MVC   LDGKEY+1(L'LDGKEY-1),LDGKEY                                      
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT,UNIT                                                     
         MVC   LDGKLDG,LEDGER                                                   
*                                                                               
         LA    R2,IO                                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JE    RLED25                                                           
         CLI   BYTE,1              ARE WE DOING A SETUP CALL?                   
         JE    RLEDX                                                            
         DC    H'0'                                                             
*                                                                               
RLED25   MVC   ACCADDR,LDGKDA                                                   
         GOTOR GETIT                                                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,LDGRFST                                                       
         XR    RF,RF                                                            
         DROP  R2                                                               
*                                                                               
RLED30   CLI   0(R1),0             END OF RECORD?                               
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),ACLELQ                                                     
         JE    RLED40                                                           
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         J     RLED30                                                           
*                                                                               
         USING ACLELD,R1                                                        
RLED40   XC    DISPS,DISPS                                                      
         XC    LEVELS,LEVELS                                                    
         LA    RE,ACLVALS                                                       
         IC    RF,ACLLN                                                         
         DROP  R1                                                               
*                                                                               
         SRL   RF,4               WILL GIVE NUMBER OF LEVELS                    
         XR    R2,R2                                                            
         LA    R1,DISPS                                                         
*                                                                               
RLED50   MVC   0(1,R1),0(RE)                                                    
         LA    RE,16(RE)                                                        
         LA    R1,1(R1)                                                         
         BCT   RF,RLED50                                                        
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    R2,DISPS                                                         
         IC    RE,0(R2)                                                         
         LA    R1,LEVELS                                                        
         STC   RE,0(R1)                                                         
         LA    R0,3                                                             
*                                                                               
RLED60   LA    R1,1(R1)                                                         
         IC    RE,0(R2)                                                         
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         JZ    RLED70                                                           
         SR    RF,RE                                                            
         STC   RF,0(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,RLED60                                                        
*                                                                               
RLED70   MVI   LDGCLT,0                                                         
         MVI   LDGOFC,0                                                         
         MVI   LDGCAN,0                                                         
*                                                                               
         LA    R2,IO                                                            
         USING LDGRECD,R2                                                       
         LA    R1,LDGRFST                                                       
         XR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
RLED72   CLI   0(R1),0             END OF RECORD?                               
         JE    RLEDX                                                            
         CLI   0(R1),LDGELQ                                                     
         JE    RLED74                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     RLED72                                                           
*                                                                               
         USING LDGELD,R1                                                        
RLED74   MVC   LDGOFC,LDGOPOS      SAVE OFFICE POSITION                         
         MVC   LDGCLT,LDGCPOS                                                   
         TM    LDGSTAT,LDGSCANL                                                 
         JZ    *+8                                                              
         MVI   LDGCAN,C'Y'                                                      
         DROP  R1                                                               
*                                                                               
RLEDX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* CALL DMGR TO PERFORM A READHI                                                 
* NTRY: R2           = A(RECORD BUFFER)                                         
*       IOKEY        = KEY TO READ HIGH FOR                                     
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                                
*       CC NOT EQUAL = ERROR ON READ                                            
*===============================================================                
                                                                                
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,(R2),DMWORK                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 VHEXOUT,PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV                          
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI ERROR KEY HEXOUT FOLLOWS'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT                  
*                                                                               
* NTRY:                                                                         
* EXIT: CC EQ    RECORD TO BE PROCESSED                                         
*     : CC NE    RECORD NOT TO BE PROCESSED                                     
*===============================================================                
         SPACE 1                                                                
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         MVC   RCVACTN,=CL6'COPY'                                               
         CLI   RRECTY,RRECTCPY                                                  
         JE    RECC001                                                          
         MVC   RCVACTN,=CL6'CHANGE'                                             
         CLI   RRECTY,RRECTCHG                                                  
         JE    RECC001                                                          
         MVC   RCVACTN,=CL6'ADD'                                                
         CLI   RRECTY,RRECTADD                                                  
         JE    RECC001                                                          
RECC001  LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
CHG      USING ACCRECD,R2                                                       
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R4,RECVHDR+L'RECVHDR                                             
*                                                                               
CPY      USING ACCRECD,R4                                                       
*                                                                               
         LTR   R1,R1               ARE WE TESTING THE ENTIRE RECORD             
         JZ    RECC010             ENTIRE RECORD                                
         LR    RE,R2                                                            
         LR    RF,R4                                                            
         LA    RE,ACTRFST-ACTRECD(RE)    CHECK FOR NAME CHANGE.                 
         LA    RF,ACTRFST-ACTRECD(RF)                                           
*                                                                               
         SR    R1,R1                                                            
RECC002  CLI   0(RE),0             EOR?                                         
         JE    NO                                                               
         CLI   0(RE),NAMELQ        X'20' - NAME ELEMENT                         
         JE    RECC004                                                          
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         J     RECC002                                                          
*                                                                               
RECC004  CLI   0(RF),0             EOR?                                         
         JE    NO                                                               
         CLI   0(RF),NAMELQ        X'20' - NAME ELEMENT                         
         JE    RECC006                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         J     RECC004                                                          
*                                                                               
RECC006  CLC   1(1,RE),1(RF)       SAME LENGTH?                                 
         JNE   YES                 IF NOT-SOMETHING CHANGED                     
         IC    R1,1(RE)            TAKE A LENGTH IF NOT SET                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         JE    NO                  NO NAME CHANGE                               
         J     YES                 NAME HAS CHANGED.  PROCESS IT.               
*                                                                               
RECC010  CLC   CHG.ACCRLEN,CPY.ACCRLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,CPY.ACCRLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
STATAFT  DS    XL1                                                              
*                                                                               
         LTORG                                                                  
         DROP  CHG,CPY                                                          
         EJECT                                                                  
*===============================================================                
* PROCESS END OF UPDATE MODE                                                    
*===============================================================                
         SPACE 1                                                                
PROCUEND NTR1                                                                   
         J     YES                                                              
         EJECT                                                                  
*==================================================================             
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES                  
* REMEMBER BRANCHING TO YES MIGHT JUST MEAN DON'T PROCESS                       
* BRANCHING TO NO WILL CAUSE A DUMP!                                            
*==================================================================             
                                                                                
PROCUPDT NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
*                                                                               
         CLI   RFILTY,ACCMSTQ      TEST ACCMST FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
*                                                                               
         CLI   UPDINIT,C'Y'        TEST UPDTAB VALUES SET                       
         JE    UPDT2                                                            
*                                                                               
         BRAS  RE,SETUPTAB         SET LEDGER VALUES IN UPDTAB                  
         MVI   UPDINIT,C'Y'                                                     
*                                                                               
UPDT2    L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         CLC   SXDTAGB,0(R2)       TEST RIGHT COMPANY                           
         JNE   YES                                                              
*                                                                               
         MVI   CLXFLAG,0           CLEAR FLAG                                   
*                                                                               
         GOTO1 VRECTYP,DMCB,(C'D',(R2))                                         
         MVC   RECTYPE,0(R1)       SAVE RECORD TYPE                             
*                                                                               
* TEST IF UNIT LEDGER IN UPDATE LIST                                            
*                                                                               
         USING UPDTABD,R3                                                       
         LA    R3,UPDTAB                                                        
UPDT10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   1(2,R2),UPDUL       MATCH UNIT/LEDGER                            
         JE    UPDT20                                                           
         LA    R3,UPDTABLQ(R3)                                                  
         CLI   0(R3),0                                                          
         JNE   UPDT10                                                           
         J     YES                                                              
*                                                                               
UPDT20   CLI   RECTYPE,ACRTTRN     TEST TRANSACTION                             
         JE    UPDT30                   NO TRANSACTIONS FOR MASTER RUN          
         TM    UPDSTAT,UPDAINC     INCL THIS U/L IN ACCOUNT CHANGES?            
         JNO   YES                                                              
UPDT30   GOTOR RECCMP,0            TEST IF ADDED OR CHANGED                     
         JNE   YES                 NO - NOTHING TO PROCESS                      
*                                                                               
         MVC   LEVELS,UPDLEVS      SET LEDGER VALUES                            
         MVC   DISPS,UPDDISP                                                    
         MVC   LDGOFC,UPDLOFC                                                   
         MVC   LDGCLT,UPDCLT                                                    
         MVC   LDGCAN,UPDCAN                                                    
         DROP  R3                                                               
*                                                                               
         BRAS  RE,UPDTALL          CALL UPDATE PROCESS ROUTINE                  
         JE    YES                 EXIT OK                                      
         J     NO                  EXIT ERROR                                   
         DROP  R5                                                               
*                                                                               
UPDTAB   DS    0X                UNIT/LDGR/STAT/LEVELS/DISPS/CL/OF/CAN          
         DC    C'SB',AL1(0),XL13'00'                                            
*        DC    C'SJ',AL1(UPDAINC),XL13'00'                                      
         DC    C'SP',AL1(UPDAINC),XL13'00'                                      
         DC    C'SQ',AL1(UPDAINC),XL13'00'                                      
         DC    C'SR',AL1(UPDAINC),XL13'00'                                      
         DC    C'SS',AL1(UPDAINC),XL13'00'                                      
         DC    C'ST',AL1(UPDAINC),XL13'00'                                      
         DC    C'SU',AL1(UPDAINC),XL13'00'                                      
*        DC    C'SV',AL1(UPDAINC),XL13'00'                                      
*        DC    C'SW',AL1(0),XL13'00'                                            
*        DC    C'SY',AL1(UPDAINC),XL13'00'                                      
*        DC    C'SZ',AL1(0),XL13'00'                                            
         DC    X'00'                                                            
UPDINIT  DC    C'N'                                                             
*                                                                               
SETUPTAB NTR1                                                                   
*                                                                               
         USING UPDTABD,R3                                                       
         LA    R3,UPDTAB                                                        
         MVC   COMPANY,SXDTAGB     SET CPY CODE FROM SYSTEM TABLE               
*                                                                               
SETUPTB2 MVC   UNIT(2),UPDUL       SET UNIT/LDGR                                
         GOTOR RDLEDG,1                                                         
         MVC   UPDLEVS,LEVELS                                                   
         MVC   UPDDISP,DISPS                                                    
         MVC   UPDLOFC,LDGOFC                                                   
         MVC   UPDCLT,LDGCLT                                                    
         MVC   UPDCAN,LDGCAN                                                    
*                                                                               
         LA    R3,UPDTABLQ(R3)                                                  
         CLI   0(R3),0                                                          
         JNE   SETUPTB2                                                         
         J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* DXMODE = DXCLOXDQ - PUT OUT CONSOLE MESSAGE                                   
*==================================================================             
                                                                                
PROCCLOX NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         LARL  R1,AXFLAG                                                        
         TM    0(R1),AXFSR9        DID WE FIND A TYPE 9?                        
         JNO   YES                 NO - SKIP SENDING MESSAGE                    
         TM    MSGFLAG,X'80'       DID WE TRANSMIT ALREADY                      
         JNO   YES                 YES - DO NOT SEND AGAIN                      
*                                                                               
         TM    SXDTFLG2,SXDTUATQ          IS THIS A UAT FILE?                   
         JNO   *+10                                                             
         MVC   MSGDSCP1(6),=C'<0335>'     USE FOR UAT FILES                     
         MVC   MSGAGY(L'SXDTAGY),SXDTAGY                                        
         EDIT  SXDTPIN,(5,MSGORIG),ZERO=NOBLANK,FILL=0                          
         MVC   MSGDSN(L'SXDTXDSN),SXDTXDSN                                      
         LA    R1,MSGDSN           REPLACE AS WITH CM IN DSN                    
         LA    R0,L'MSGDSN                                                      
PROCC005 CLC   0(7,R1),=C'DDS.XTR'     ONLY CHECK DDS.XTR DATASETS              
         JNE   YES                     NOT THE RIGHT DSN-SKIP                   
PROCC010 CLC   0(3,R1),=C'XTR'         FIND XTR QUALIFIER                       
         JE    PROCC012                                                         
         LA    R1,1(R1)                                                         
         JCT   R0,PROCC010                                                      
         J     YES                     NOT THE RIGHT DSN-SKIP                   
*                                                                               
PROCC012 LA    R1,4(R1)            BUMP PASSED XTR. LEVEL                       
         AHI   R0,-4                                                            
         LA    RE,WORK                                                          
         MVC   WORK,MYSPACES                                                    
PROCC014 CLI   0(R1),C'.'          DUMP IF WE GET TO THE NEXT QUALIFIER         
         JE    YES                 NOT THE RIGHT DSN-SKIP                       
         CLC   0(2,R1),=C'AS'      FIND AS                                      
         JNE   *+14                                                             
         MVC   0(2,R1),=C'CM'      REPLACE WITH CM                              
         J     PROCC016                                                         
         MVC   0(1,RE),0(R1)       MOVE IN CODE                                 
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         JCT   R0,PROCC014                                                      
         J     YES                 NOT THE RIGHT DSN-SKIP                       
*                                                                               
PROCC016 MVC   MSGALPHA,WORK       PUT ALPHA CODE IN MESSAGE                    
         LA    RE,SXDTSLNM                                                      
         LA    R1,L'SXDTSLNM                                                    
PROCC018 CLI   0(RE),X'40'                                                      
         JNH   PROCC020                                                         
         CLC   0(3,RE),=C'ACC'     SKIP ACC                                     
         JNE   PROCC019                                                         
         LA    RE,3(RE)            BUMP PASSED ACC                              
         MVC   MSGSYS,0(RE)                                                     
         J     PROCC020                                                         
PROCC019 LA    RE,1(RE)                                                         
         JCT   R1,PROCC018                                                      
*                                                                               
PROCC020 WTO   TEXT=MSGFLEN                                                     
         NI    MSGFLAG,X'FF'-X'80' ONLY SEND ONCE                               
         LARL  R1,AXFLAG                                                        
         MVI   0(R1),0             CLEAR FLAG FOR NEXT RUN                      
         J     YES                                                              
*                                                                               
MSGFLEN  DC    AL2(MSGFLNQ)                                                     
MSGDSCP1 DC    C'<0334> FILE AGENCY='                                           
MSGAGY   DS    CL(L'SXDTAGY)                                                    
MSGDSCP2 DC    C' ALPHA='                                                       
MSGALPHA DS    CL5                                                              
MSGDSCP3 DC    C' ORIGIN='                                                      
MSGORIG  DS    CL5                                                              
MSGDSCP4 DC    C' SYS='                                                         
MSGSYS   DS    CL2                                                              
MSGDSCP5 DC    C' DSN='                                                         
MSGDSN   DS    CL(L'SXDTXDSN)                                                   
MSGFLNQ  EQU   *-MSGDSCP1                                                       
*                                                                               
MSGFLAG  DC    X'80'               MARK FLAG AS SET                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* PROCESS KEY VALUES IN RECOVERY RECORD                                         
* NTRY: R5 = A(RECOVERY RECORD)                                                 
*==========================================================                     
                                                                                
         USING RECDS,R5                                                         
         USING ACTRECD,RECVHDR+L'RECVHDR                                        
                                                                                
PROCKEY  NTR1  BASE=*,LABEL=*                                                   
         GOTOR RECCMP,0            COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         MVC   RECTYPE,0(R1)       SAVE RECORD TYPE                             
*                                                                               
         TM    ACTRSTAT,ACTSDELT   IS THIS RECORD DELETED?                      
         BZ    PKEY10              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY02              AND THEREFORE NEVER ON TEMPO DB              
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   PKEY04                                                           
*                                                                               
PKEY02   TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JNZ   NO                  IGNORE THESE                                 
*                                                                               
PKEY04   MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY10   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   PKEY16                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JZ    PKEY20                                                           
         CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY12                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   PKEY14                                                           
*                                                                               
PKEY12   TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   NO                  IGNORE THESE                                 
*                                                                               
PKEY14   MVI   DXACTION,C'A'                                                    
         J     YES                                                              
                                                                                
PKEY16   CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY18                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   YES                                                              
*                                                                               
PKEY18   TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   NO                  IGNORE THESE                                 
         J     YES                                                              
                                                                                
PKEY20   CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY22                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   YES                                                              
*                                                                               
PKEY22   TM    ACTRSTAT,ACTSDRFT                                                
         JZ    PKEY24                                                           
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JNZ   NO                                                               
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
                                                                                
PKEY24   TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN UPDATE MODE                          
* R2 = A(ACCOUNT RECORD BUFFER)                                                 
*-------->                                                                      
* NOTE TO ME - REMEMBER THAT EXITING TO YES SKIPS PROCESSING!                   
*-------->                                                                      
*===================================================================            
                                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 AFILTUPD                                                         
         JNE   YES                                                              
*                                                                               
         CLI   DXUSER,C'M'         TEST MASTER REC EXTRACT                      
         JNE   UPD01                                                            
         CLI   RECTYPE,ACRTTRN     MASTER EXTRACT - TEST TRANSACTION            
         JE    YES                 YES - SKIP                                   
         J     UPD02               ELSE PROCESS                                 
*                                                                               
UPD01    CLI   RECTYPE,ACRTTRN     ONLY PROCESS TRANSACTIONS                    
         JNE   YES                 IF NOT TRANSACTION, SKIP                     
*                                                                               
UPD02    BRAS  RE,INITALL                                                       
*                                                                               
         L     R4,DXAXREC                                                       
         BRAS  RE,GETCURR                                                       
*                                                                               
         L     RF,=V(AXACCT)                                                    
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION (TYPE=10)                   
         BNE   *+8                                                              
         L     RF,=V(AXTRNS)                                                    
*                                                                               
         GOTO1 (RF),DMCB,(RC),(C'U',DXAXREC),DXARECB,(R6)                       
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    YES                                                              
         TM    DMCB+8,X'80'                                                     
         JO    YES                                                              
*                                                                               
         USING AXG$TRN,R4                                                       
         L     R4,DXAXREC                                                       
**********************************************************************          
* TEST TEST TEST                                                                
**********************************************************************          
*        CLC   =C'PAY',AXG$RTYP    ONLY LOOKING FOR PAY                         
*        JNE   UPD05                                                            
         CLC   DXCLI,MYSPACES      ANY CLI= FILTER IN JCL                       
         JNH   UPD03                                                            
         CLC   DXCLI,AXG$CLT       DOES CLIENT MATCH?                           
         JE    UPD03                                                            
         J     YES                                                              
*                                                                               
UPD03    CLI   AXG$SYS,C'P'        PRINT HAS DIFFERENT INSERTION ORDER          
         JE    UPD05                                                            
         LA    R8,AXG$INSO                                                      
*                                                                               
         CLI   AXG$SYS,C'N'          AS PER SONNY, OCT16,2020                   
         JNE   UPD04                                                            
         GOTOR BLDINSO,DMCB,=C'N',1                                             
         J     UPD04X                                                           
*                                                                               
UPD04    GOTOR BLDINSO,DMCB,AXG$MED,L'AXG$MED                                   
UPD04X   GOTOR BLDINSO,DMCB,AXG$CLT,L'AXG$CLT                                   
         GOTOR BLDINSO,DMCB,AXG$PRD+3,L'AXG$PRD-3                               
         GOTOR BLDINSO,DMCB,AXG$EST,L'AXG$EST                                   
         GOTOR BLDINSO,DMCB,AXG$NETW,L'AXG$NETW                                 
         GOTOR BLDINSO,DMCB,(X'80',AXG$MOS),L'AXG$MOS                           
*                                                                               
UPD05    LARL  R1,AXFLAG                                                        
         TM    CLXFLAG,CLXFTY9     DID WE FIND A TYPE 9?                        
         JNO   *+8                 NO - DO NOT SET LOCAL BIT                    
         OI    0(R1),AXFSR9        YES-SET LOCAL BIT FOR PROCCLOX MODE          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     R0,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPD06               DO NOT CONVERT RECORD                        
*                                                                               
         L     R0,DXASQLB                                                       
UPD06    CLC   =C'CASHR',DXUSER    CASH RECEIPTS BEHAVES DIFFERENTLY            
         JE    UPD300                                                           
         GOTOR VENDSR              VENDOR DETAIL FOR SR TRANSACTIONS            
         J     YES                                                              
*                                                                               
UPD300   GOTOR CASHRCV             USE CHECKS FROM TABLE (VVNDBUFF)             
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
BLDINSO  NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)            STUFF TO MOVE OVER                           
         L     RE,4(R1)                                                         
BINSO100 CLI   0(RF),C' '                                                       
         BE    BINSO500                                                         
         MVC   0(1,R8),0(RF)                                                    
         AHI   R8,1                                                             
         AHI   RF,1                                                             
         BCT   RE,BINSO100                                                      
*                                                                               
BINSO500 CLI   0(R1),X'80'                                                      
         BE    BINSO600                                                         
         C     RE,4(R1)                                                         
         BE    BINSO600                                                         
         MVI   0(R8),C'-'                                                       
         AHI   R8,1                                                             
*                                                                               
BINSO600 XIT1  REGS=(R8)                                                        
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* GET CHECK DETAILS FOR SR TRANSACTIONS                                         
*===========================================================                    
CASHRCV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CKBUFFD,R3                                                       
         USING AXG$TRN,R4                                                       
         L     R4,DXAXREC                                                       
         XC    KEY,KEY             BUILD CHECK KEY TO LOOK IN TABLE             
         LA    R3,KEY                                                           
         MVC   CKBBNKAC,COMPANY    BANK ACCOUNT                                 
         MVC   CKBBNKA,AXG$BKCD                                                 
         MVC   CKBOFFC,AXG$OFFC    OFFICE                                       
         MVC   CKBCULCC,COMPANY    ACCOUNT                                      
         MVC   CKBCULC,AXG$ACCD                                                 
         MVC   WORK(6),AXG$CKDT    CHECK DATE                                   
         GOTO1 VDATCON,DMCB,(0,WORK),(20,AXG$CKDT)                              
         GOTO1 ENGDATE,DMCB,AXG$CKDT                                            
         MVC   WORK(6),AXG$DPDT    DEPOSIT DATE                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(1,CKBDATE)                                
         MVC   CKBNUMBR,AXG$CHEK   CHECK NUMBER                                 
         GOTO1 USADATE,DMCB,AXG$ACDT                ACTIVITY DATE               
         GOTO1 VDATCON,DMCB,(9,WORK),(2,CURRACDT)                               
*                                                                               
CASHR050 DS    0H                                                               
         SAM31                                                                  
         L     RF,VVNDBUFF                                                      
TBL      USING CKBUFFD,RF                                                       
CASHR100 OC    TBL.CKBKEY(CKBKEYLN),TBL.CKBKEY    EOT?                          
         JNZ   CASHR130                                                         
         ST    RF,CURRTBLL              STORE CURRENT LOCATION                  
         L     RE,VVNDBUFF                                                      
         SR    RF,RE                                                            
         C     RF,=AL4(VBUFFSZQ)        MAKE SURE WE DIDN'T OVERFLOW            
         JNH   *+6                      MEMORY STORAGE                          
         DC    H'0'                     OVERFLOW                                
         J     CASHR200                                                         
*                                                                               
CASHR130 CLC   TBL.CKBKEY,CKBKEY        MATCH ON CHECK KEY                      
         JNE   CASHR180                                                         
         CLC   TBL.CKBBREF,AXG$BREF     MATCH ON BATCH REFERENCE                
         JNE   CASHR180                                                         
         CLC   TBL.CKBACTDT,CURRACDT    MATCH ON ACTIVITY DATE                  
         JE    CASHR500                                                         
*                                                                               
CASHR180 AHI   RF,CKBUFFLN         NO, CHECK ON NEXT TABLE ENTRY                
         J     CASHR100                                                         
*                                                                               
CASHR200 SAM24                     READ CHECK RECORD                            
         LA    R2,CHEKREC                                                       
         MVC   IOKEY,CKBKEY                                                     
         GOTO1 VDATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,(R2),DMWORK                    
         CLI   DMCB+8,0                                                         
         BE    CASHR305                                                         
         DC    H'0'                                                             
*                                                                               
CASHR300 GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,(R2),DMWORK                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CASHR305 CLC   0(TRNKSBR-TRNRECD,R2),IOKEY       MATCH ON KEY                   
         JNH   CASHR308                                                         
*                                                                               
         SAM31                                                                  
         L     RF,CURRTBLL                CAN'T FIND CHECK                      
         MVC   TBL.CKBKEY,CKBKEY                                                
         MVC   TBL.CKBBREF,AXG$BREF                                             
         MVC   TBL.CKBACTDT,CURRACDT                                            
         ZAP   TBL.CKBAMT,=P'0'           ZERO OUT THE CHECK AMOUNT             
         J     CASHR500                                                         
*                                                                               
CASHR308 MVC   ACCADDR,(TRNKDA-TRNRECD)(R2)                                     
         GOTOR GETIT                                                            
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,CHEKREC                                                       
         LA    RE,TRNRFST                                                       
         USING TRNELD,RE                                                        
         CLC   TRNBTCH,AXG$BREF    MATCH ON BATCH REFERENCE                     
         JNE   CASHR300                                                         
*                                                                               
CASHR310 XR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
*                                                                               
         CLI   0(RE),0             EOR                                          
         JE    CASHR300                                                         
         USING TRSELD,RE                                                        
         CLI   TRSEL,TRSELQ                                                     
         JNE   CASHR300                                                         
         MVI   AXG$VOID,C'N'                                                    
         TM    TRSSTAT,TRSSVOID                                                 
         JZ    *+8                                                              
         MVI   AXG$VOID,C'Y'                                                    
         CLC   TRSDATE,CURRACDT    MATCH ON ACTIVITY DATE                       
         JNE   CASHR300                                                         
*                                                                               
CASHR400 SAM31                                                                  
         LA    RE,TRNRFST                                                       
         USING TRNELD,RE                                                        
         L     RF,CURRTBLL         RESTORE CURRENT LOCATION                     
         MVC   TBL.CKBKEY,CKBKEY                                                
         MVC   TBL.CKBBREF,AXG$BREF                                             
         MVC   TBL.CKBACTDT,CURRACDT                                            
         ZAP   TBL.CKBAMT,TRNAMNT                                               
         DROP  R2,RE                                                            
*                                                                               
CASHR500 SAM31                                                                  
         EDIT  (P6,TBL.CKBAMT),(16,AXG$CKAM),2,FLOAT=-,COMMAS=NO,      X        
               ZERO=NOBLANK,ALIGN=LEFT                                          
         SAM24                                                                  
*                                                                               
         BRAS  RE,FILLSUBM                                                      
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     R0,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    CASHR800            DO NOT CONVERT RECORD                        
         GOTO1 VAXCNV,DMCB,(RC),(C'U',DXAXREC)                                  
         L     R0,DXASQLB                                                       
*                                                                               
CASHR800 GOTO1 DXPUT,DMCB,(R0),(R7)                                             
         GOTOR DECIOC                                                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
*----------------------------------------------------------------------         
* CONVERT DDMMYYYY TO YYYYMMDD INTO WORK                                        
*----------------------------------------------------------------------         
USADATE  NTR1                                                                   
         L     RF,0(R1)            ADDRESS OF DATE TO CHANGE                    
         MVC   WORK(4),4(RF)                                                    
         MVC   WORK+4(2),2(RF)                                                  
         MVC   WORK+6(2),0(RF)                                                  
         J     EXIT                                                             
*----------------------------------------------------------------------         
* CONVERT YYYYMMDD TO DDMMYYYY                                                  
*----------------------------------------------------------------------         
ENGDATE  NTR1                                                                   
         L     RF,0(R1)            ADDRESS OF DATE TO CHANGE                    
         MVC   WORK(8),0(RF)                                                    
         MVC   0(2,RF),WORK+6                                                   
         MVC   2(2,RF),WORK+4                                                   
         MVC   4(4,RF),WORK                                                     
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
CURRTBLL DS    A                                                                
CURRACDT DS    XL2                                                              
CHEKREC  DS    2000C                                                            
         EJECT                                                                  
*===========================================================                    
* GET VENDOR DETAILS FOR SR TRANSACTIONS                                        
*===========================================================                    
VENDSR   NTR1  BASE=*,LABEL=*                                                   
         MVI   VNDFIRST,C'Y'                                                    
         ZAP   CHKAMT,=P'0'                                                     
*                                                                               
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION                             
         JNE   VENDSR50                                                         
*                                                                               
         USING AXG$TRN,R4                                                       
         L     R4,DXAXREC                                                       
         MVC   RTYP,AXG$RTYP       RCV/PAY                                      
*                                                                               
         USING TRNRECD,R4                                                       
         L     R4,DXARECB          CHANGE RECORD                                
         LA    R4,L'RECVHDR+4(R4)  BUMP PAST RCV LENGTH & HEADER                
         ST    R4,SRTRNSIO                                                      
*                                                                               
         CLI   RTYP,C'P'           PAY                                          
         BNE   VENDSR00            NO, RCV                                      
         MVI   VOIDCHEK,C'N'                                                    
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         MVC   TRANTYPE,TRNTYPE                                                 
*                                                                               
         USING PIDELD,RF                                                        
VSR0_10  CLI   0(RF),0             EOR?                                         
         JE    VENDSR08                                                         
         CLI   PIDEL,PIDELQ                                                     
         JNE   VSR0_20                                                          
         MVC   SVAGY,PIDAGY                                                     
         J     VSR0_90                                                          
*                                                                               
         USING TRSELD,RF                                                        
VSR0_20  CLI   TRSEL,TRSELQ                                                     
         JNE   VSR0_30                                                          
*        GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TMPUDATE)                            
         MVC   TMPUDATE,TRSUMOS                                                 
         MVI   TMPUDATE+2,X'01'                                                 
         TM    TRSSTAT,TRSSVOID                                                 
         JZ    *+8                                                              
         MVI   VOIDCHEK,C'Y'                                                    
         J     VSR0_90                                                          
*                                                                               
         USING MPYELD,RF                                                        
VSR0_30  CLI   MPYEL,MPYELQ                                                     
         JNE   VSR0_90                                                          
         ZAP   CHKAMT,MPYAMNT      CHECK AMOUNT                                 
*                                                                               
VSR0_90  XR    R1,R1                                                            
         IC    R1,MPYLN                                                         
         AR    RF,R1                                                            
         J     VSR0_10                                                          
*                                                                               
VENDSR00 LA    RF,TRNRFST                                                       
         USING FFTELD,RF                                                        
VENDSR01 CLI   0(RF),0             EOR?                                         
*        JE    VENDSR50                                                         
         JE    VENDSR08                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   VENDSR03                                                         
         CLI   FFTTYPE,FFTTMXTY    FIND MX ELEMENT                              
         JNE   VENDSR03                                                         
         CLI   FFTDLEN,FFTMLN2Q    HAS ORIGINAL MEDIA AGENCY                    
         JE    VENDSR05                                                         
VENDSR03 XR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         AR    RF,R1                                                            
         J     VENDSR01                                                         
                                                                                
VENDSR05 MVC   SVAGY,FFTMXTAG      ORIGINAL MEDIA AGENCY                        
*                                                                               
* CLEAR VENDOR BUFFER                                                           
VENDSR08 CLC   SVAGY,MYSPACES      IF NO MEDIA AGENCY                           
         BH    VSR8_03                                                          
         MVC   SVAGY,AGENCY        USE WHAT WE HAVE                             
VSR8_03  L     RE,VVNDBUFF                                                      
         ICM   RF,15,=AL4(VBUFFSZQ) VENDOR BUFFER SIZE                          
         SAM31                                                                  
         XCEFL                                                                  
         SAM24                                                                  
*                                                                               
         L     R4,SRTRNSIO                                                      
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
                                                                                
         MVC   ENDATE,TRNDATE                                                   
                                                                                
         GOTO1 VDATCON,DMCB,(1,ENDATE),(0,WORK+6)                               
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),WORK,-1                                
         GOTO1 VDATCON,DMCB,(0,WORK),(1,STDATE)                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(1,STDATE),(3,WORK)                                 
         GOTO1 VDATCON,DMCB,(X'31',ENDATE),(3,WORK+3),(1,0)                     
*                                                                               
* BUILD VENDOR BUFFER                                                           
*                                                                               
         USING AXG$TRN,R6                                                       
         L     R6,DXAXREC                                                       
***********************************************************************         
* TEST TEST TEST TEST TEST                                                      
***********************************************************************         
*        CLI   AXG$SYS,C'P'        INCLUDE PRINT ONLY FOR NOW                   
*        BE    VSR8_05             TO GO TO AXVNDR                              
*        BNE   VENDSR50                                                         
*        CLI   AXG$SYS,C'S'        SPOT ONLY FOR NOW                            
*        JNE   YES                                                              
*        CLI   AXG$SYS,C'P'        JUST PRINT FOR NOW                           
*        JNE   YES                                                              
*        JE    YES                                                              
*        CLI   AXG$SYS,C'N'        JUST NET FOR NOW                             
*        JNE   YES                                                              
*        JE    YES                                                              
***********************************************************************         
* TEST TEST TEST TEST TEST                                                      
***********************************************************************         
*                                                                               
VSR8_05  CLI   RTYP,C'P'           PAY                                          
         BNE   VENDSR09                                                         
         MVC   AXG$VOID,VOIDCHEK                                                
*                                                                               
* BUILD MBIEL AND GDAEL FOR AXTRBVND                                            
*                                                                               
VSR8_08  CLI   AXG$SYS,C'N'        EXCLUDE NET FOR NOW FOR PAY                  
*        JNE   VENDSR50                                                         
*        BE    VENDSR09                                                         
*                                                                               
         USING MBIELD,R5                                                        
         XC    VELEM,VELEM                                                      
         LA    R5,VELEM                                                         
         MVI   MBIEL,MBIELQ                                                     
         MVI   MBILN,MBILNQ                                                     
         MVC   MBISYS,AXG$SYS                                                   
         MVC   MBIMED,AXG$MED                                                   
*                                                                               
         CLI   AXG$SYS,C'N'                                                     
         JNE   VSR8_09                                                          
         MVI   MBISYS,C'S'                                                      
*                                                                               
VSR8_09  MVC   MBICLI(6),AXG$PRD                                                
         MVC   MBIEST,MYSPACES                                                  
         LHI   R1,2                                                             
         LA    RE,MBIEST                                                        
         LA    RF,AXG$EST+2                                                     
*                                                                               
VSR8_10  CLI   0(RF),C'0'                                                       
         BNL   VSR8_50                                                          
         AHI   RE,1                                                             
         AHI   RF,-1                                                            
         BCT   R1,VSR8_10                                                       
VSR8_50  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),AXG$EST                                                  
         OC    MBIEST,=C'000'                                                   
*                                                                               
         MVC   TMPDATE(4),AXG$MOS                                               
         MVC   TMPDATE+4(2),=C'01'                                              
         GOTO1 VDATCON,DMCB,(0,TMPDATE),(1,MBIMOS)                              
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),SRTRNSIO,VELEM,(RF)              
*                                                                               
VSR8_60  CLI   AXG$SYS,C'N'        SKIP THE REST FOR NET PAY                    
         JE    VENDSR09                                                         
         USING GDAELD,R5                                                        
         XC    VELEM,VELEM                                                      
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATMBD                                                  
         MVC   TMPDATE(4),AXG$TRDT+4                                            
         MVC   TMPDATE+4(2),AXG$TRDT+2                                          
         MVC   TMPDATE+6(2),AXG$TRDT                                            
         GOTO1 VDATCON,DMCB,(9,TMPDATE),(1,GDADATE)                             
*                                                                               
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         CLI   TRNTYPE,TRNTCHQS         CHECKS? (TYPE 129)                      
         JNE   VSR8_80                                                          
         MVC   GDADATE,TMPUDATE                                                 
*                                                                               
VSR8_80  LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),SRTRNSIO,VELEM,(RF)              
         DROP  R5,R6                                                            
*                                                                               
VENDSR09 GOTOR =V(AXTRBVND),DMCB,WORKD,SRTRNSIO,VVNDBUFF,WORK,SVAGY             
         GOTOR GTREPNM,DMCB,DXAXREC                                             
*                                                                               
         USING BUFFERD,R3                                                       
         L     R3,VVNDBUFF                                                      
         SAM31                                                                  
         MVC   SVCLTNM,BUFFCLNM                                                 
         MVC   SVPRDNM,BUFFPRNM                                                 
         MVC   SVESTNM,BUFFESNM                                                 
         MVC   SVPRDU1,BUFFPUD1                                                 
         MVC   SVPRDU2,BUFFPUD2                                                 
         MVC   SVESTU1,BUFFEUD1                                                 
         MVC   SVESTU2,BUFFEUD2                                                 
         MVC   SVVOUC,BUFFVOUC                                                  
         MVC   SVEST,BUFFEST                                                    
         LA    R3,BUFFDATA                                                      
         SAM24                                                                  
*                                                                               
         LHI   RF,1                                                             
         STH   RF,ITEMNUM                                                       
*                                                                               
         USING BUFFDATA,R3                                                      
VENDSR10 DS    0H                                                               
         SAM31                                                                  
         OC    0(BUFFERL1,R3),0(R3)    NOTHING IN BUFFER                        
         JNZ   VENDSR30            NO, WE HAVE SOMETHING, PROCESS IT            
         OC    BUFFERL1(BUFFERL2,R3),BUFFERL1(R3)                               
         JNZ   VENDSR30            NO, WE HAVE SOMETHING, PROCESS IT            
*                                                                               
         USING AXG$TRN,R4                                                       
VENDSR20 DS    0H                                                               
         SAM24                                                                  
         L     R4,DXAXREC                                                       
         MVC   AXG$AXN,RCVACTN    RECOVERY ACTION                               
*                                                                               
         CLC   SVVOUC,MYSPACES                                                  
         JH    VENDSR22                                                         
         MVC   SVVOUC(2),AXG$AGY                                                
         MVC   SVVOUC+2(1),AXG$MED                                              
         MVC   SVVOUC+3(3),AXG$CLT                                              
         MVC   SVVOUC+6(20),AXG$INV                                             
*                                                                               
VENDSR22 MVI   AXG$ITMN,C'1'       MUST HAVE ITEM NUMBER                        
         CLC   =C'PAY',AXG$RTYP                                                 
         JE    VENDSR23                                                         
*        MVC   AXG$NET,MYSPACES                                                 
*        MVC   AXG$NET(3),=C'.00'  AND NET AMOUNT                               
*                                                                               
         CLC   AXG$NET(3),=C'.00'  DON'T NEGATE IF ZERO                         
         JE    VNDSR22M                                                         
         CLI   AXG$NET,C'-'                                                     
         JE    VNDSR22C                                                         
*                                                                               
         MVI   WORK,C'-'           NEGATE NET                                   
         MVC   WORK+1(L'AXG$NET-1),AXG$NET                                      
         J     VNDSR22J                                                         
*                                                                               
VNDSR22C MVC   WORK(L'AXG$NET-1),AXG$NET                                        
         MVI   WORK+L'AXG$NET-1,C' '                                            
*                                                                               
VNDSR22J MVC   AXG$NET,WORK                                                     
*                                                                               
VNDSR22M MVC   AXG$GRS,AXG$DR                                                   
*                                                                               
VENDSR23 CLC   SVCLTNM,MYSPACES    USE SJ NAME IF NO NAME                       
         JNH   *+10                                                             
         MVC   AXG$CLNM,SVCLTNM                                                 
         CLC   SVPRDNM,MYSPACES    USE SJ NAME IF NO NAME                       
         JNH   *+10                                                             
         MVC   AXG$PRNM,SVPRDNM                                                 
         MVC   AXG$ESNM,SVESTNM                                                 
         MVC   AXG$PUD1,SVPRDU1                                                 
         MVC   AXG$PUD2,SVPRDU2                                                 
         MVC   AXG$EUD1,SVESTU1                                                 
         MVC   AXG$EUD2,SVESTU2                                                 
         MVC   AXG$VOUC,SVVOUC                                                  
         MVC   AXG$LINI+9(5),AXG$ITMN                                           
*                                                                               
         BAS   RE,SPCLPONM         SPECIAL PONUM                                
*                                                                               
         CLI   RTYP,C'P'           PAY                                          
         BNE   VENDSR25                                                         
         MVC   AXG$VNDC,AXG$NETW                                                
VENDSR25 BRAS  RE,FILLSUBM                                                      
         BRAS  RE,GETPID                                                        
         J     VENDSR50                                                         
*                                                                               
* PUT BUFFER ENTRY TO AREA                                                      
VENDSR30 DS    0H                                                               
         SAM24                                                                  
         MVI   VNDFIRST,C'N'                                                    
         GOTOR BUFF2REC                                                         
         CLC   SVVENDNM,MYSPACES                                                
         BNH   VENDSR40                                                         
         L     R4,DXAXREC                                                       
         MVC   AXG$VNDN,SVVENDNM                                                
*                                                                               
VENDSR40 GOTOR VAXCNV,DMCB,(RC),(C'U',DXAXREC)                                  
*                                                                               
         GOTOR DXPUT,DMCB,(R0),(R7)                                             
         GOTOR DECIOC                                                           
         JNE   NO                                                               
*                                                                               
         SAM31                                                                  
         AHI   R3,BUFFERLN                                                      
         J     VENDSR10                                                         
*                                                                               
VENDSR50 CLI   VNDFIRST,C'Y'                                                    
         JNE   YES                                                              
         CLC   SVVENDNM,MYSPACES                                                
         BNH   VENDSR60                                                         
         L     R4,DXAXREC                                                       
         MVC   AXG$VNDN,SVVENDNM                                                
*                                                                               
VENDSR60 GOTOR VAXCNV,DMCB,(RC),(C'U',DXAXREC)                                  
*                                                                               
         GOTOR DXPUT,DMCB,(R0),(R7)                                             
         GOTOR DECIOC                                                           
         J     YES                                                              
*                                                                               
VOIDCHEK DS    CL1                                                              
CHKAMT   DS    PL8                                                              
GRSAMT   DS    PL8                                                              
TRANTYPE DS    X                                                                
TMPUDATE DS    PL3                                                              
TMPDATE  DS    CL8                                                              
TMPFLDH  DS    XL8                                                              
TMPFLD   DS    CL16                                                             
VELEM    DS    CL255                                                            
*                                                                               
         EJECT                                                                  
*===========================================================                    
* BUFFER TO RECORD                                                              
*===========================================================                    
BUFF2REC NTR1                                                                   
                                                                                
         L     RF,0(R1)                                                         
         USING AXG$TRN,R4                                                       
         L     R4,DXAXREC                                                       
         MVC   AXG$AXN,RCVACTN    RECOVERY ACTION                               
         ZAP   GRSAMT,=P'0'                                                     
                                                                                
         CLC   ITEMNUM,=X'0002'                                                 
         BL    B2REC03                                                          
         MVC   AXG$TAXA,=CL16'.00'                                              
         MVC   AXG$BINC,=CL16'.00'                                              
         J     B2REC05                                                          
*                                                                               
B2REC03  LA    RF,16                                                            
         LA    R1,AXG$TAXA+L'AXG$TAXA-1                                         
B2REC03C CLI   0(R1),X'F0'                                                      
         JNL   B2REC03F                                                         
         AHI   R1,-1                                                            
         AHI   RF,-1                                                            
         J     B2REC03C                                                         
B2REC03F GOTO1 =V(CASHVAL),DMCB,(X'80',AXG$TAXA),(RF),0                         
         AP    GRSAMT,DMCB+4(8)                                                 
*                                                                               
         LA    RF,16                                                            
         LA    R1,AXG$BINC+L'AXG$BINC-1                                         
B2REC03J CLI   0(R1),X'F0'                                                      
         JNL   B2REC03M                                                         
         AHI   R1,-1                                                            
         AHI   RF,-1                                                            
         J     B2REC03J                                                         
B2REC03M GOTO1 =V(CASHVAL),DMCB,(X'80',AXG$BINC),(RF),0                         
         AP    GRSAMT,DMCB+4(8)                                                 
*                                                                               
B2REC05  SAM31                                                                  
         EDIT  (B2,ITEMNUM),(5,AXG$ITMN),0,ALIGN=LEFT                           
         OC    BUFFVNDR,MYSPACES                                                
         MVC   AXG$PID,BUFFPID                                                  
         MVC   AXG$VNDC,BUFFVNDR                                                
         MVC   AXG$VNDN,BUFFNAME                                                
         MVC   AXG$INSD,BUFFINSD                                                
         MVC   AXG$SPAC,BUFFSPAC                                                
         CLC   SVCLTNM,MYSPACES                                                 
         JNH   *+10                                                             
         MVC   AXG$CLNM,SVCLTNM                                                 
         CLC   SVPRDNM,MYSPACES                                                 
         JNH   *+10                                                             
         MVC   AXG$PRNM,SVPRDNM                                                 
         MVC   AXG$ESNM,SVESTNM                                                 
         MVC   AXG$PUD1,SVPRDU1                                                 
         MVC   AXG$PUD2,SVPRDU2                                                 
         MVC   AXG$EUD1,SVESTU1                                                 
         MVC   AXG$EUD2,SVESTU2                                                 
         MVC   AXG$VOUC,SVVOUC                                                  
*                                                                               
         BAS   RE,SPCLPONM         SPECIAL PONUM                                
*                                                                               
         BAS   RE,FILLSUBM                                                      
*                                                                               
B2REC06  ZAP   B2RECPK,BUFFGRS                                                  
         CLI   RTYP,C'P'           PAY                                          
         JE    *+10                                                             
         MP    B2RECPK,=P'-1'                                                   
         EDIT  (P8,B2RECPK),(16,AXG$GRS),2,FLOAT=-,COMMAS=NO,          X        
               ZERO=NOBLANK,ALIGN=LEFT                                          
*                                                                               
         CLI   TRANTYPE,TRNTCHQS                                                
         JNE   B2REC07                                                          
         EDIT  (P8,CHKAMT),(16,AXG$GRS),2,FLOAT=-,COMMAS=NO,           X        
               ZERO=NOBLANK,ALIGN=LEFT                                          
*                                                                               
B2REC07  ZAP   B2RECPK,BUFFNET                                                  
         CLI   RTYP,C'P'           PAY                                          
         JE    *+10                                                             
         MP    B2RECPK,=P'-1'                                                   
         AP    GRSAMT,B2RECPK                                                   
         EDIT  (P8,B2RECPK),(16,AXG$NET),2,FLOAT=-,COMMAS=NO,          X        
               ZERO=NOBLANK,ALIGN=LEFT                                          
         CLI   TRANTYPE,TRNTCHQS                                                
         JE    B2REC09                                                          
         CLI   RTYP,C'P'           PAY                                          
         JE    *+10                                                             
         MP    GRSAMT,=P'-1'                                                    
         EDIT  (P8,GRSAMT),(16,AXG$GRS),2,FLOAT=-,COMMAS=NO,           X        
               ZERO=NOBLANK,ALIGN=LEFT                                          
*                                                                               
B2REC09  MVC   AXG$INSO,BUFFINSO                                                
         MVC   AXG$CMPG,BUFFCMPG                                                
         MVC   AXG$MEDA,BUFFMEDA                                                
         EDIT  (B1,BUFFESTU),(3,AXG$BUYU),0,ZERO=NOBLANK,ALIGN=LEFT             
         MVC   AXG$BUYU,BUFFBUYU                                                
*                                                                               
         CLI   AXG$SYS,C'P'                                                     
         JNE   B2REC09J                                                         
         CLC   AXG$INSO,MYSPACES                                                
         JNE   B2REC09J                                                         
*                                                                               
         LA    R8,AXG$INSO                                                      
         MVC   0(2,R8),AXG$AGY                                                  
         MVC   2(1,R8),AXG$MED                                                  
         MVC   3(3,R8),AXG$CLT                                                  
         AHI   R8,6                                                             
         MVC   0(9,R8),=C'000000000'                                            
*                                                                               
         LHI   R1,9                                                             
         LA    RF,BUFFBUYU+8                                                    
         LA    RE,BUFFBUYU                                                      
B2REC09C CLI   0(RF),C' '                                                       
         JH    B2REC09E                                                         
         J     B2REC09H                                                         
*                                                                               
B2REC09E MVC   0(1,R8),0(RE)                                                    
         AHI   RE,1                                                             
B2REC09H AHI   R8,1                                                             
         SHI   RF,1                                                             
         BCT   R1,B2REC09C                                                      
*                                                                               
B2REC09J EDIT  (B2,BUFFSEQ),(5,AXG$ITMN),0,ALIGN=LEFT                           
         MVC   AXG$LINI+9(5),AXG$ITMN                                           
*                                                                               
         XC    SECAGY,SECAGY                                                    
         XC    SECAGYA,SECAGYA                                                  
         OC    AXG$PID(2),AXG$PID                                               
         BNZ   B2REC10                                                          
         MVC   AXG$PID(2),BUFFPID                                               
         MVC   SECAGY(2),BUFFSECA                                               
         SAM24                                                                  
*                                                                               
B2REC10  BAS   RE,GETPID                                                        
*                                                                               
         LH    RF,ITEMNUM                                                       
         AHI   RF,1                                                             
         STH   RF,ITEMNUM                                                       
*                                                                               
         J     EXIT                                                             
*                                                                               
B2RECPK  DC    PL8'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPECIAL PO NUM FOR SJR                                              *         
***********************************************************************         
SPCLPONM NTR1                                                                   
         CLI   AXG$SYS,C'P'        PRINT                                        
         JNE   SPCLPON3                                                         
         OC    SVEST,SVEST                                                      
         JZ    SPCLPON3                                                         
         EDIT  SVEST,(6,AXG$EST),0,ALIGN=LEFT                                   
SPCLPON3 CLC   AXG$AGY,=C'SJ'      AS PER SONNY, APR 20, 2020                   
         JNE   EXIT                                                             
         CLC   AXG$CLT,=C'NCP'                                                  
         JNE   EXIT                                                             
         MVC   AXG$PONM,AXG$EUD1                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET REP NAME                                             *         
***********************************************************************         
GTREPNM  NTR1                                                                   
         MVC   SVVENDNM,MYSPACES                                                
         CLI   RTYP,C'P'           PAY ONLY                                     
         JNE   EXIT                                                             
*                                                                               
         USING AXG$TRN,R4                                                       
         L     R4,0(R1)                                                         
*        CLC   AXG$CONT(8),=C'00000000'   IF THERE'S A ACCT IN CONTRA           
*        JL    EXIT                       NO, THERE'S NO REP, LEAVE             
*                                                                               
         MVC   IOKEY,MYSPACES             YES, GET REP NAME                     
         MVC   IOKEY(1),COMPANY                                                 
         MVC   IOKEY+1(L'AXG$ACCD),AXG$ACCD                                     
         LA    R2,PIDREC                                                        
         GOTOR READHI              INITIAL READ HIGH                            
         JNE   NO                  CC EQ IF NOT FOUND                           
*                                                                               
         MVC   ACCADDR,(TRNKDA-TRNRECD)(R2)                                     
         GOTOR GETIT                                                            
*                                                                               
         USING NAMELD,R3                                                        
         LA    R3,(ACCRFST-ACCRECD)(R2)                                         
*                                                                               
GTREP10  CLI   0(R3),0                                                          
         JE    EXIT                                                             
         CLI   0(R3),NAMELQ                                                     
         JE    GTREP20                                                          
         LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         J     GTREP10                                                          
*                                                                               
GTREP20  LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q                                                       
         CHI   RF,L'SVVENDNM                                                    
         BNH   GTREP30                                                          
         LHI   RF,L'SVVENDNM                                                    
GTREP30  AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVVENDNM(0),NAMEREC                                              
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* FILL SUBMEDIA                                                                 
*===========================================================                    
FILLSUBM NTR1  BASE=*,LABEL=*                                                   
         CLI   AXG$SYS,C'P'                                                     
         JNE   FSUBM05                                                          
         CLI   AXG$MED,C'M'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'MAGAZINE'                                     
         CLI   AXG$MED,C'N'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'NEWSPAPER'                                    
         CLI   AXG$MED,C'I'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(11),=CL11'INTERACTIVE'                                  
         CLI   AXG$MED,C'O'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'OUTDOOR'                                      
         CLI   AXG$MED,C'T'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'TRADE'                                        
         CLI   AXG$MED,C'S'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'SUPPLEMENT'                                   
         CLI   AXG$MED,C'L'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'SOCIAL'                                       
         CLI   AXG$MED,C'*'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'PRODUCTION'                                   
         J     EXIT                                                             
*                                                                               
FSUBM05  CLI   AXG$MED,C'T'                                                     
         JNE   *+14                                                             
         MVC   AXG$SBMT(10),=CL10'TELEVISION'                                   
         J     FSUBM10                                                          
         CLI   AXG$MED,C'R'                                                     
         JNE   *+14                                                             
         MVC   AXG$SBMT(10),=CL10'RADIO'                                        
         J     FSUBM10                                                          
         CLI   AXG$MED,C'C'                                                     
         JNE   *+14                                                             
         MVC   AXG$SBMT(10),=CL10'CABLE'                                        
         J     FSUBM10                                                          
         CLI   AXG$MED,C'N'                                                     
         JNE   *+14                                                             
         MVC   AXG$SBMT(10),=CL10'NETWORK'                                      
         J     FSUBM10                                                          
         CLI   AXG$MED,C'X'                                                     
         JNE   *+10                                                             
         MVC   AXG$SBMT(10),=CL10'NET RADIO'                                    
*                                                                               
FSUBM10  MVC   AXG$SBMN(9),=CL9'BROADCAST'                                      
         CLI   AXG$SYS,C'N'          AS PER SONNY, OCT16,2020                   
         JNE   *+8                                                              
         MVI   AXG$MED,C'N'                                                     
                                                                                
         CLI   AXG$VNDC,C'0'                                                    
         JL    *+14                                                             
         MVC   AXG$SBMN(9),=CL9'CABLE'                                          
         J     EXIT                                                             
                                                                                
         CLI   AXG$VNDC+4,C'D'                                                  
         JE    *+8                                                              
         CLI   AXG$VNDC+4,C'S'                                                  
         JNE   *+10                                                             
         MVC   AXG$SBMN(9),=CL9'STREAMING'                                      
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PID FROM BINARY PID                                  *         
*                                                                     *         
* NTRY: P1=A(INPUT)                                                   *         
*       P2=A(OUTPUT)                                                  *         
***********************************************************************         
*                                                                               
GETPID   NTR1  BASE=*,LABEL=*                                                   
         L     RE,=V(UTL)                                                       
         MVI   4(RE),SYSCONQ       SET TO CONTROL SYSTEM                        
*                                                                               
         MVC   WORK(2),AXG$PID                                                  
         MVC   AXG$PID,MYSPACES                                                 
         OC    WORK(2),WORK        NO PID?                                      
         BZ    GETPIDX                                                          
*                                                                               
*-----------------------------------------------                                
* READ THE AGY ALPHA FROM ID RECORD USING IDNUM                                 
*-----------------------------------------------                                
         OC    SECAGY,SECAGY       NOT SET? PID CAME FROM PIDEL                 
         JNZ   GETPID10            SKIP TO READING SECURITY AGY                 
         MVC   SECAGYA,AXG$AGY                                                  
         J     GETPID50                                                         
*                                                                               
         USING CTIREC,R2                                                        
GETPID10 LA    R2,PIDREC                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,SECAGY                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                        
         BE    GETPID13                                                         
         MVC   SECAGYA,AXG$AGY                                                  
         B     GETPID50                                                         
*                                                                               
         USING CTAGYD,R1                                                        
GETPID13 LA    R1,CTIDATA          EXTRACT ALPHA FROM SEC ALPHA EL.             
         XR    R0,R0                                                            
*                                                                               
GETPID15 CLI   CTAGYEL,0                                                        
         BE    GETPID50                                                         
         CLI   CTAGYEL,CTAGYELQ                                                 
         BE    GETPID19                                                         
         IC    R0,CTAGYLEN                                                      
         BXH   R1,R0,GETPID15                                                   
*                                                                               
GETPID19 MVC   SECAGYA,CTAGYID                                                  
*-----------------------------------------------                                
* READ THE AGY ALPHA FROM ID RECORD USING IDNUM                                 
*-----------------------------------------------                                
         USING CT5REC,R2                                                        
GETPID50 LA    R2,PIDREC                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SECAGYA                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                        
         BNE   GETPID80                                                         
*                                                                               
         LA    R1,CT5DATA          EXTRACT ALPHA FROM SEC ALPHA EL.             
         USING CTSEAD,R1                                                        
         XR    R0,R0                                                            
*                                                                               
GETPID55 CLI   CTSEAEL,0                                                        
         BE    GETPID80                                                         
         CLI   CTSEAEL,CTSEAELQ                                                 
         BE    GETPID59                                                         
         IC    R0,CTSEALEN                                                      
         BXH   R1,R0,GETPID55                                                   
*                                                                               
GETPID59 MVC   SECAGYA,CTSEAAID                                                 
*                                                                               
*-------------------------                                                      
* READ THE PID RECORD NOW                                                       
*-------------------------                                                      
         USING SA0REC,R2           READ PID RECORD                              
GETPID80 LA    R2,PIDREC                                                        
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGYA     ELSE NATIVE AGENCY                           
         DROP  RF                                                               
         MVC   SA0KNUM,WORK                                                     
                                                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SA0REC,SA0REC                        
         BNE   GETPIDX                                                          
*                                                                               
         LA    R1,SA0DATA          EXTRACT NAME FROM DESCRIPTION EL.            
         USING SAPALD,R1                                                        
         XR    R0,R0                                                            
*                                                                               
GETPID90 CLI   SAPALEL,0                                                        
         BE    GETPIDX                                                          
         CLI   SAPALEL,SAPALELQ                                                 
         BE    GETPID95                                                         
         IC    R0,SAPALLN                                                       
         BXH   R1,R0,GETPID90                                                   
*                                                                               
GETPID95 MVC   AXG$PID,SAPALPID                                                 
         CLC   =C'**',AXG$PID                                                   
         JNE   EXIT                                                             
         MVC   AXG$PID,MYSPACES                                                 
*                                                                               
GETPIDX  J     EXIT                                                             
*                                                                               
CNTRL2   DC    CL8'CONTROL '                                                    
CTFILES2 DC    CL8'NCTFILE '                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         LTORG                                                                  
       ++INCLUDE AXTRBRID                                                       
         EJECT                                                                  
*===========================================================                    
* GET AGENCY DEFAULT CURRENCY                                                   
*===========================================================                    
GETCURR  NTR1  BASE=*,LABEL=*                                                   
         L     RE,=V(UTL)                                                       
         LLC   R5,4(RE)            SAVE CURRENT SYSTEM                          
         MVI   4(RE),SYSCONQ       SET TO CONTROL SYSTEM                        
*                                                                               
         USING CT5REC,R2                                                        
         LA    R2,ACCREC           ACCESS RECORD HAS CURRENCY CODE              
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                        
         JNE   GETCURR9                                                         
*                                                                               
         USING CTAGDD,R1                                                        
         LA    R1,CT5DATA          EXTRACT ALPHA FROM SEC ALPHA EL.             
         XR    R0,R0                                                            
*                                                                               
GETCURR5 CLI   CTAGDEL,0                                                        
         BE    GETCURR9                                                         
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    GETCURR7                                                         
         CLI   CTAGDEL,CTDSCELQ                                                 
         JE    GETCURR8                                                         
GETCURR6 IC    R0,CTAGDLEN                                                      
         BXH   R1,R0,GETCURR5                                                   
*                                                                               
GETCURR7 MVC   AXG$CURR,CTAGDCUR                                                
         J     GETCURR6                                                         
*                                                                               
         USING CTDSCEL,R1                                                       
GETCURR8 MVC   SECCPY,CTDSC                                                     
         J     GETCURR6                                                         
*                                                                               
         USING CTIREC,R2                                                        
GETCURR9 LA    R2,ACCREC                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,SECCPY                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                        
         JNE   GTCURR90                                                         
*                                                                               
         USING CTDSCD,R1                                                        
         LA    R1,CTIDATA          EXTRACT ALPHA FROM SEC ALPHA EL.             
         XR    R0,R0                                                            
*                                                                               
GTCURR30 CLI   CTDSCEL,0                                                        
         BE    GTCURR90                                                         
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    GTCURR50                                                         
         IC    R0,CTDSCLEN                                                      
         BXH   R1,R0,GTCURR30                                                   
*                                                                               
GTCURR50 SR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         SHI   RF,CTDSC-CTDSCD+1                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SECCPYID(0),CTDSC                                                
         OC    SECCPYID,MYSPACES                                                
         MVC   AXG$CPYI,SECCPYID                                                
*                                                                               
GTCURR90 DS    0H                                                               
         L     RE,=V(UTL)          RESTORE SYSTEM                               
         STC   R5,4(RE)                                                         
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
SECCPY   DS    XL2                                                              
SECCPYID DS    CL10                                                             
ACCREC   DS    2000X                                                            
         EJECT                                                                  
*                                                                               
*===========================================================                    
* FILTER UPDATE RECORD                                                          
*===========================================================                    
                                                                                
         USING TRNRECD,R2                                                       
         USING RECDS,R5                                                         
FILTUPD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
*                                                                               
         CLI   RPRG,X'01'          WAS THIS A PFM ACTION?                       
         JE    NO                                                               
*                                                                               
         CLC   =C'CASHR',DXUSER    CASH RECEIPTS                                
         JNE   FUPD0060                                                         
*                                                                               
         USING TRNELD,R3                                                        
         LA    R3,(TRNRFST-TRNRECD)(R2)                                         
         CLI   TRNTYPE,TRNTCALC    ONLY WANT TYPE 30'S                          
         JNE   NO                                                               
         TM    TRNSTAT,TRNSDR      ONLY WANT CREDITS                            
         JO    NO                                                               
*                                                                               
         USING RALELD,R3                                                        
         SR    R0,R0                                                            
FUPD0020 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
FUPD0030 CLI   0(R3),X'00'         EOR?                                         
         JE    NO                                                               
         CLI   RALEL,RALELQ        X'D9' RECEIVABLE ALLOCATION ELEMENT          
         JNE   FUPD0020                                                         
         CLI   RALTYPE,RALTALC     X'01' REGULAR ALLOCATION                     
         JNE   FUPD0020                                                         
*                                                                               
         LA    R3,(TRNRFST-TRNRECD)(R2)                                         
         USING SPAELD,R3                                                        
         SR    R0,R0                                                            
FUPD0040 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
FUPD0050 CLI   0(R3),X'00'         EOR?                                         
         JE    NO                                                               
         CLI   SPAEL,SPAELQ        X'2C' SPECIAL POSTING A/C ELEMENT            
         JNE   FUPD0040                                                         
         CLI   SPATYPE,SPATBANK    X'1D' BANK ACCOUNT                           
         JNE   FUPD0040                                                         
*                                                                               
         J     FUPD0100                                                         
*----------------------------------------------------------------------         
FUPD0060 CLC   =C'CASHP',DXUSER    CASH PAID                                    
         JNE   FUPD0100                                                         
*                                                                               
         USING TRNELD,R3                                                        
         LA    R3,(TRNRFST-TRNRECD)(R2)                                         
         CLI   TRNTYPE,TRNTCHQS    ONLY WANT TYPE 129'S                         
         JNE   NO                                                               
         TM    TRNSTAT,TRNSDR      ONLY WANT DEBITS                             
         JZ    NO                                                               
*                                                                               
         J     FUPD0100                                                         
*----------------------------------------------------------------------         
FUPD0100 CLI   DXUSER,C'M'         TEST MASTER REC EXTRACT                      
         JNE   YES                                                              
         CLC   =C'SJ',TRNKUNT      IF SJ                                        
         JNE   FUPD0200                                                         
         CLI   TRNKACT,C' '        MAKE SURE CLIENT IS THERE                    
         JNH   NO                                                               
         CLI   RPRG,X'0B'          WAS THIS AN PROD CHANGE?                     
         JE    YES                                                              
         CLI   RPRG,X'1F'          WAS THIS AN LINK CHANGE?                     
         JE    YES                                                              
         CLI   RPRG,X'37'          WAS THIS AN ORGANIZER CHANGE?                
         JE    YES                                                              
         J     NO                                                               
*                                                                               
FUPD0200 CLI   RECTYPE,ACRTTRN     TEST TRANSACTION                             
         JE    YES                 OK                                           
         CLI   RECTYPE,ACRTACTL    ONLY WANT ACCOUNT RECORDS                    
         JE    *+12                                                             
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   NO                                                               
         CLI   DXACTION,C'A'       WAS THIS AN ADD?                             
         JE    YES                                                              
         CLI   RPRG,X'23'          WAS THIS AN AFM CHANGE?                      
         JE    YES                                                              
         GOTOR RECCMP,1            COMPARE FOR NAME CHANGE ONLY                 
         JE    YES                 CHANGES TO REC - CONTINUE                    
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*===============================================================                
* INITIALISE ALL EXTRACT RECORDS                                                
*===============================================================                
                                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R1,AXGMACCX-AXGMACC  LENGTH OF HDR REC                           
         CLI   RECTYPE,ACRTTRN      TEST TRANSACTION (TYPE=10)                  
         BNE   *+8                                                              
         LHI   R1,AXG$TRNX-AXG$TRN  LENGTH OF TRANSACTION                       
*                                                                               
*        CLI   RECTYPE,X'FE'        TEST TOTAL RECORD                           
*        BNE   *+8                                                              
*        LHI   R1,AXGTTOTX-AXGTTOT                                              
*                                                                               
*        CLI   RECTYPE,X'FD'        TEST RECON HEADER                           
*        BNE   *+8                                                              
*        LHI   R1,AXGMFRHX-AXGMFRH                                              
*                                                                               
         CLI   HDRONLY,C'Y'        TEST ONLY INTIALIZE HEADER                   
         BNE   *+8                                                              
         LHI   R1,DXHDRLEV-DXHDRD                                               
*                                                                               
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
*                                                                               
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
         CLI   RECTYPE,X'FE'       TOTAL RECORD                                 
         JE    IALL02              YES - NO RECOVERY DATA                       
         CLI   RECTYPE,X'FD'       FILE RECON HDR                               
         JE    IALL02              YES - NO RECOVERY DATA                       
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(X'20',DXHDRCDT+2)                        
         MVC   DXHDRCDT(2),DXCENT                                               
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   YES                                                              
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),=C'  '                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
ACCMSTQ  EQU   X'6A'                                                            
MXTRTQ   EQU   C'|'                PIPE SEPARATOR CHR                           
FF       EQU   X'FF'                                                            
                                                                                
*===================================================================            
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT                 
* ADDRESS IS AT AADDRESS IN W/S                                                 
*===================================================================            
         SPACE 1                                                                
         DS    0L                                                               
ADDRESS  DC    CL8'EXTRNS'                                                      
         DC    A(COMFACS)                                                       
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(ACRECTYP)                                                      
         DC    V(AXTRBCNV)                                                      
         DC    A(0)                 A(DXBLOCK)                                  
         DC    A(0)                 A(DXTAB)                                    
         DC    V(DATVAL)                                                        
         DC    V(HEXOUT)                                                        
         DC    V(GETDAY)                                                        
         DC    V(ADDAY)                                                         
         DC    V(AXTRBVND)                                                      
         DC    V(LOADER)                                                        
         DC    A(0)                 A(VENDBUFF)                                 
         DS    V(HELLO)                                                         
         DS    2A                                                               
*                                                                               
         DS    F                                                                
         DS    2H                   MYSDATE2/MYEDATE2                           
         DC    CL8'FILTERS'                                                     
         DC    A(FILTLOD)                                                       
         DC    A(FILTUPD)                                                       
*                                                                               
         DC    CL8'INITS'                                                       
         DC    A(INITALL)                                                       
*                                                                               
         DC    C'OPEN   '                                                       
         DC    C'DMREAD '                                                       
         DC    C'DMRSEQ '                                                       
         DC    C'DMRDHI '                                                       
         DC    C'DMCLSE '                                                       
         DC    C'DMFAST '                                                       
         DC    C'GETREC '                                                       
         DC    C'CONTROL'                                                       
         DC    C'CTFILE '                                                       
         DC    C'ACCDIR '                                                       
         DC    C'ACCMST '                                                       
         DC    F'0'                DMDA                                         
*                                                                               
         DS    PL8                 SEQNUM                                       
         DS    PL8                 TOTALDR                                      
         DS    PL8                 TOTALCR                                      
         DS    176C                SPARE                                        
         DC    CL80' '                                                          
*                                                                               
         DS    F                                                                
         DS    F                   IOLEN                                        
         DS    6144C               IO1                                          
         DS    6144C               IO2                                          
*                                                                               
AXFLAG   DC    X'00'               STATUS FLAG                                  
AXFSR9   EQU   X'80'               TYPE 9 FOUND                                 
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DS    0D                                                               
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
         EJECT                                                                  
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
         EJECT                                                                  
*===================================================================            
* DSECT TO COVER TYPTAB TABLE                                                   
*===================================================================            
                                                                                
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
         DS    XL3                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
*                                                                               
                                                                                
UPDTABD  DSECT                                                                  
UPDUL    DS    CL2                 UNIT/LEDGER                                  
UPDSTAT  DS    XL1                 STATUS BYTE                                  
UPDAINC  EQU   X'80'               INCLUDE IN UPDATE                            
UPDLEVS  DS    XL4                 LEDGER LEVELS                                
UPDDISP  DS    XL4                 LEDGER DISPLACEMENTS                         
UPDLOFC  DS    CL1                 LEDGER OFFICE POSITION                       
UPDCLT   DS    CL1                 LEDGER CLIENT POSITION                       
UPDCAN   DS    CL1                 CANADIAN LEDGER                              
         DS    CL2                 SPARE                                        
UPDTABLQ EQU   *-UPDTABD                                                        
*                                                                               
       ++INCLUDE AXTRBBFD                                                       
VBUFFSZQ EQU   BUFFERLN*1500+1     FOR 1500 VENDOR ENTRIES + 1 BYTE EOT         
         EJECT                                                                  
       ++INCLUDE AXTRBCKD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE AXTRBWRK                                                       
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FASYSEQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109AXTRBLK   10/20/20'                                      
         END                                                                    
