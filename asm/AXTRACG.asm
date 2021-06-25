*          DATA SET AXTRACG    AT LEVEL 007 AS OF 08/12/15                      
*PHASE AXTRACGC                                                                 
*INCLUDE AXROUTG                                                                
*INCLUDE AXCNVG                                                                 
*INCLUDE AXHDRG                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE DDWTO                                                                  
         TITLE 'AXTRACG - EXTRACT GROUP M ACCOUNTING DATA'                      
***********************************************************************         
* GROUPM SQL SUB SYSTEM EXTRACT CONTROL MODULE                        *         
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
         EJECT                                                                  
AXTRACG  CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,AXTRACG*                                                   
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     RA,=A(ADDRESS)                                                   
         USING ADDRESSD,RA                                                      
         ST    RA,AADDRESS                                                      
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
         BNE   MUPDTEND                                                         
         BRAS  RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
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
         CLI   DXMODE,DXOPENQ                                                   
         BNE   GENINX                                                           
         ZAP   SEQNUM,=P'0'                                                     
         ZAP   TOTALDR,=P'0'                                                    
         ZAP   TOTALCR,=P'0'                                                    
GENINX   B     MXIT                                                             
         EJECT                                                                  
*==================================================================             
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                                  
*==================================================================             
                                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
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
         DC    CL3'SCX',X'01',AL4(LOADSC)                                       
         DC    CL3'SEX',X'01',AL4(LOADSE)                                       
         DC    CL3'SGX',X'01',AL4(LOADSG)                                       
         DC    CL3'SIX',X'01',AL4(LOADSI)                                       
         DC    CL3'SJX',X'00',AL4(LOADSJ)                                       
         DC    CL3'SPX',X'00',AL4(LOADSP)                                       
         DC    CL3'SQX',X'00',AL4(LOADSQ)                                       
         DC    CL3'SRX',X'01',AL4(LOADSR)                                       
         DC    CL3'SSX',X'00',AL4(LOADSS)                                       
         DC    CL3'STX',X'00',AL4(LOADST)                                       
         DC    CL3'SUX',X'00',AL4(LOADSU)                                       
         DC    CL3'SVX',X'00',AL4(LOADSV)                                       
         DC    CL3'SWX',X'00',AL4(LOADSW)                                       
         DC    CL3'SXX',X'00',AL4(LOADSX)                                       
         DC    CL3'SYX',X'00',AL4(LOADSY)                                       
         DC    CL3'SZX',X'01',AL4(LOADSZ)                                       
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
         GOTO1 (RF),DMCB,(RC),(C'L',DXAXREC),(R2)                               
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
         LA    R2,RECVHDR+L'RECVHDR                                             
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
         CLC   CHG.ACCRLEN,CPY.ACCRLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,CPY.ACCRLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
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
UPDT30   GOTOR RECCMP              TEST IF ADDED OR CHANGED                     
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
         DC    C'SC',AL1(0),XL13'00'                                            
         DC    C'SE',AL1(0),XL13'00'                                            
         DC    C'SG',AL1(UPDAINC),XL13'00'                                      
         DC    C'SI',AL1(0),XL13'00'                                            
         DC    C'SJ',AL1(UPDAINC),XL13'00'                                      
         DC    C'SP',AL1(UPDAINC),XL13'00'                                      
         DC    C'SQ',AL1(UPDAINC),XL13'00'                                      
         DC    C'SR',AL1(0),XL13'00'                                            
         DC    C'SS',AL1(UPDAINC),XL13'00'                                      
         DC    C'ST',AL1(UPDAINC),XL13'00'                                      
         DC    C'SU',AL1(UPDAINC),XL13'00'                                      
         DC    C'SV',AL1(UPDAINC),XL13'00'                                      
         DC    C'SW',AL1(0),XL13'00'                                            
         DC    C'SX',AL1(UPDAINC),XL13'00'                                      
         DC    C'SY',AL1(UPDAINC),XL13'00'                                      
         DC    C'SZ',AL1(0),XL13'00'                                            
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
*==========================================================                     
* PROCESS KEY VALUES IN RECOVERY RECORD                                         
* NTRY: R5 = A(RECOVERY RECORD)                                                 
*==========================================================                     
                                                                                
         USING RECDS,R5                                                         
         USING ACTRECD,RECVHDR+L'RECVHDR                                        
                                                                                
PROCKEY  NTR1  BASE=*,LABEL=*                                                   
         GOTOR RECCMP              COMPARE COPY/CHANGE RECORDS                  
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
         L     RF,=V(AXACCT)                                                    
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION (TYPE=10)                   
         BNE   *+8                                                              
         L     RF,=V(AXTRNS)                                                    
*                                                                               
         GOTO1 (RF),DMCB,(RC),(C'U',DXAXREC),DXARECB                            
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    YES                                                              
         TM    DMCB+8,X'80'                                                     
         JO    YES                                                              
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     R0,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPD06               DO NOT CONVERT RECORD                        
*                                                                               
UPD04    GOTO1 VAXCNV,DMCB,(RC),(C'U',DXAXREC)                                  
*                                                                               
         L     R0,DXASQLB                                                       
UPD06    GOTO1 DXPUT,DMCB,(R0),(R7)                                             
         GOTOR DECIOC                                                           
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
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
         CLI   DXUSER,C'M'         TEST MASTER REC EXTRACT                      
         JNE   YES                                                              
         CLC   =C'SJ',TRNKUNT      IF SJ                                        
         JNE   FUPD02                                                           
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
FUPD02   CLI   RECTYPE,ACRTTRN     TEST TRANSACTION                             
         JE    YES                 OK                                           
         CLI   RPRG,X'23'          WAS THIS AN AFM CHANGE?                      
         JE    YES                                                              
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
         CLI   RECTYPE,X'FE'        TEST TOTAL RECORD                           
         BNE   *+8                                                              
         LHI   R1,AXGTTOTX-AXGTTOT                                              
*                                                                               
         CLI   RECTYPE,X'FD'        TEST RECON HEADER                           
         BNE   *+8                                                              
         LHI   R1,AXGMFRHX-AXGMFRH                                              
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
         DC    V(AXCNVG)                                                        
         DC    A(0)                 A(DXBLOCK)                                  
         DC    A(0)                 A(DXTAB)                                    
         DC    V(DATVAL)                                                        
         DC    V(HEXOUT)                                                        
         DC    V(GETDAY)                                                        
         DC    V(ADDAY)                                                         
         DS    6A                                                               
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
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE AXWORKG                                                        
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007AXTRACG   08/12/15'                                      
         END                                                                    
