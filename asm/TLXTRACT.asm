*          DATA SET TLXTRACT   AT LEVEL 010 AS OF 03/15/18                      
*PHASE LXTRACTA                                                                 
*INCLUDE LXROUTS                   XTRACT RECORD CREATION MODULE                
*INCLUDE LXCNVX                    CONVERSION ROUTINES FOR ALL ABOVE            
*INCLUDE BUFFERIN                                                               
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE GETOPT                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CUREDIT                                                                
*INCLUDE PRORATA                                                                
*INCLUDE TINVCON                                                                
*INCLUDE CRYPT                                                                  
         TITLE 'TLXTRACT-EXTRACT SYSTEM FILE SYB DATA'                          
***********************************************************************         
* TALENT SYB SUB SYSTEM EXTRACT CONTROL MODULE                        *         
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
***********************************************************************         
         EJECT                                                                  
TLXTRACT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*TLXTR**,R8,CLEAR=YES                                      
         USING WORKD,RC            RC=A(LOCAL/GLOBAL W/S)                       
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         USING DXBLOCKD,R7                                                      
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING SXDTABD,R6                                                       
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
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
MAIN     DS    0H                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
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
         BNE   MERR                                                             
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
*         INITIALIZE CONSWITCH                                        *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
         CLI   DXMODE,DXOPENQ                                                   
         BNE   GENIN10                                                          
         GOTOR VBUFFRIN,DMCB,('BUFFAINI',AHIDBUF),HIDREC,ACOMFACS               
*                                                                               
GENIN10  XC    AENCKEY,AENCKEY                                                  
         CLC   SXDTEKEY,SPACES                                                  
         BNH   GENIN20                                                          
         LA    RF,SXDTEKEY                                                      
         STCM  RF,15,AENCKEY                                                    
*                                  TEST FOR PASSWORD ENCRYPTION                 
GENIN20  LA    RE,PKCNTS           CLEAR RECORD COUNTS                          
         LA    R0,PKCNTQ                                                        
         ZAP   0(PKCNTLNQ,RE),=P'0'                                             
         AHI   RE,PKCNTLNQ                                                      
         BCT   R0,*-10                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN TALENT SYSTEM FILES                         *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         XC    IOKEY,IOKEY         GET DTF ADDRESS                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,TALDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,TALENT,TALFILES,IO                          
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE TALENT SYSTEM FILES                       *         
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,TALENT,0,IO                                 
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
TALENT   DC    CL8'TALENT'                                                      
TALFILES DC    C'NTALDIR NTALFIL NCHKDIR NCHKFIL '                              
         DC    C'NGENDIR NGENFIL NCTFILE X'                                     
*                                                                               
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS TALENT FILE DATA IN LOAD MODE                               *         
***********************************************************************         
         SPACE 1                                                                
PROCLOAD NTR1  ,                                                                
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         L     RF,=A(LOADCNT)      RUN COUNT RECORD                             
         BASR  RE,RF                                                            
         J     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS TALENT FILE DATA IN UPDATE MODE READ RECOVERY FILES         *         
***********************************************************************         
         SPACE 1                                                                
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   PRIALPHA,SXDTAGY    SET PRIMARY ALPHA FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
         CLI   RFILTY,TALFILQ      TEST TALFIL FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         ICM   RF,15,TYPEAUPD      ELSE CALL UPDATE PROCESS ROUTINE             
         JZ    YES                 NO UPDATE ROUTINE SO SKIP                    
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R5                                                         
         USING TLRCD,RECVHDR+L'RECVHDR                                          
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         TM    TLRCSTAT,TALSDELT   IS THIS RECORD DELETED?                      
         BZ    PROCK10             NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+TLRCSTAT-TLRCD+4(R4),TALSDELT                          
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
* TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD                         
*                                                                               
PROCK10  CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+TLRCSTAT-TLRCD+4(R4),TALSDELT                          
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUFFERIN FILE                                                       *         
***********************************************************************         
         SPACE 1                                                                
HIDBUF   BUFFD TYPE=D,                                                 X        
               KEYLEN=L'HIDREC,                                        X        
               COMLEN=L'HIDREC,                                        X        
               BUFFERS=20                                                       
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(BUFFERIN)                                                      
         DC    V(TLXCNVX)                                                       
*                                                                               
* TLXROUTS VCONS                                                                
         DC    V(TLXAGYC)          AGENCY RECORD                                
         DC    V(TLXAGGC)          AGENCY GROUP                                 
         DC    V(TLXW4RC)          W-4 RECORD                                   
         DC    V(TLXAGTC)          AGENT RECORD                                 
         DC    V(TLXCLIC)          CLIENT RECORD                                
         DC    V(TLXPRDC)          PRODUCT RECORD                               
         DC    V(TLXCSTC)          CAST RECORD                                  
         DC    V(TLXCOMC)          COMMERCIAL RECORD                            
         DC    V(TLXSTFC)          STAFF RECORD                                 
         DC    V(TLXGRTC)          GUARANTEE RECORD                             
* END OF TLXROUTS VCONS                                                         
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(TALLOAD)          TALENT RECORD LOAD                           
         DC    A(CXLOAD)           CONTROL FILE LOAD                            
         DC    A(TALUPDT)          TALENT RECORD UPDATE                         
         DC    A(DECIOC)                                                        
         DC    A(CHKSEQIO)                                                      
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(READCHI)                                                       
         DC    A(RECCMP)                                                        
         DC    A(HIDBUF)           HOLDING FEE ID TABLE FOR BUFFERING           
         DC    A(COMFACS)          COMFACS                                      
*                                                                               
* LOAD RECORD ROUTINE ADCONS                                                    
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADALL)          ALL                                          
         DC    A(LOADAGY)          AGENCY (AND SUB-RECORDS)                     
         DC    A(LOADAGG)          AGENCY GROUP                                 
         DC    A(LOADW4R)          W-4 (AND SUB-RECORDS)                        
         DC    A(LOADAGT)          AGENT                                        
         DC    A(LOADCLI)          CLIENT                                       
         DC    A(LOADPRD)          PRODUCT                                      
         DC    A(LOADCST)          CAST (AND SUB-RECORDS)                       
         DC    A(LOADCOM)          COMMERCIAL (AND SUB-RECORDS)                 
         DC    A(LOADSTF)          STAFF (AND SUB-RECORDS)                      
         DC    A(LOADGRT)          GUARANTEE                                    
         DC    A(LOADCNT)          RECORD COUNT RECORD                          
* END OF LOAD ROUTINE ADCONS                                                    
*                                                                               
* UPDATE RECORD ROUTINE ADCONS                                                  
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTALL)          ALL                                          
         DC    A(UPDTAGY)          AGENCY                                       
         DC    A(UPDTAGG)          AGENCY GROUP                                 
         DC    A(UPDTW4R)          W-4                                          
         DC    A(UPDTAGT)          AGENT                                        
         DC    A(UPDTCLI)          CLIENT                                       
         DC    A(UPDTPRD)          PRODUCT                                      
         DC    A(UPDTCST)          CAST                                         
         DC    A(UPDTCOM)          COMMERCIAL                                   
         DC    A(UPDTSTF)          STAFF                                        
         DC    A(UPDTGRT)          GUARANTEE RECORDS                            
         DC    A(0)                RECORD COUNT RECORD                          
* END OF UPDATE ROUTINE ADCONS                                                  
*                                                                               
* FILTER ROUTINE ADCONS                                                         
         DC    CL8'FILTERS'                                                     
         DC    A(FILTAGY)          AGENCY                                       
         DC    A(FILTAGG)          AGENCY GROUP                                 
         DC    A(FILTW4R)          W-4                                          
         DC    A(FILTAGT)          AGENT                                        
         DC    A(FILTCLI)          CLIENT                                       
         DC    A(FILTPRD)          PRODUCT                                      
         DC    A(FILTCST)          CAST                                         
         DC    A(FILTCOM)          COMMERCIAL                                   
         DC    A(FILTSTF)          STAFF                                        
         DC    A(FILTGRT)          GUARANTEE                                    
         DC    A(0)                RECORD COUNT RECORD                          
* END OF FILTER ROUTINE ADCONS                                                  
*                                                                               
* INITIALIZATION ROUTINE ADCONS                                                 
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          ALL                                          
         DC    A(INITAGY)          AGENCY                                       
         DC    A(INITAGG)          AGENCY GROUP                                 
         DC    A(INITW4R)          W-4                                          
         DC    A(INITAGT)          AGENT                                        
         DC    A(INITCLI)          CLIENT                                       
         DC    A(INITPRD)          PRODUCT                                      
         DC    A(INITCST)          CAST                                         
         DC    A(INITCOM)          COMMERCIAL                                   
         DC    A(INITSTF)          STAFF                                        
         DC    A(INITGRT)          GUARANTEE                                    
         DC    A(INITCNT)          RECORD COUNT RECORD                          
* END OF INITIALIZATION ADCONS                                                  
*                                                                               
         DC    C'OPEN   '                                                       
         DC    C'DMREAD '                                                       
         DC    C'DMRSEQ '                                                       
         DC    C'DMRDHI '                                                       
         DC    C'DMCLSE '                                                       
         DC    C'DMFAST '                                                       
         DC    C'GETREC '                                                       
         DC    C'RECOVER'                                                       
         DC    C'CONTROL'                                                       
         DC    C'CTFILE '                                                       
         DC    C'TALDIR '                                                       
         DC    C'TALFIL '                                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    80C' '                                                           
*                                                                               
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
TALDIRQ  EQU   X'71'                                                            
TALFILQ  EQU   X'72'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
FF       EQU   X'FF'                                                            
TALSDELT EQU   X'80'               RECORD IS DELETED                            
*                                                                               
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    DEPTH TO READ TO FOR INCREMENT ON RDHI FOR LEDGER LOAD       *         
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
         SPACE 1                                                                
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'NET',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'AGG',AL1(00,00,00,00,00),AL4(LOADAGG,UPDTAGG)                
         DC    CL3'W4R',AL1(00,00,00,00,00),AL4(LOADW4R,UPDTW4R)                
         DC    CL3'PID',AL1(00,00,00,00,00),AL4(LOADW4R,UPDTW4R)                
         DC    CL3'AGT',AL1(00,00,00,00,00),AL4(LOADAGT,UPDTAGT)                
         DC    CL3'CLI',AL1(00,00,00,00,00),AL4(LOADCLI,UPDTCLI)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CST',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'OVA',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'OVP',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'CER',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'EPS',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'HFI',AL1(00,00,00,00,00),AL4(LOADCST,UPDTCST)                
         DC    CL3'COM',AL1(00,00,00,00,00),AL4(LOADCOM,UPDTCOM)                
         DC    CL3'AFM',AL1(00,00,00,00,00),AL4(LOADCOM,UPDTCOM)                
         DC    CL3'CMV',AL1(00,00,00,00,00),AL4(LOADCOM,UPDTCOM)                
         DC    CL3'MCD',AL1(00,00,00,00,00),AL4(LOADCOM,UPDTCOM)                
         DC    CL3'CMP',AL1(00,00,00,00,00),AL4(LOADCOM,UPDTCOM)                
******** DC    CL3'STF',AL1(00,00,00,00,00),AL4(LOADSTF,UPDTSTF)                
******** DC    CL3'LAG',AL1(00,00,00,00,00),AL4(LOADSTF,UPDTSTF)                
******** DC    CL3'LCL',AL1(00,00,00,00,00),AL4(LOADSTF,UPDTSTF)                
         DC    CL3'GRT',AL1(00,00,00,00,00),AL4(LOADGRT,UPDTGRT)                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
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
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
         SPACE 1                                                                
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP10   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP20                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP10                                                           
*                                                                               
GTYP20   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
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
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       TALADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),TALFIL,TALADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,TALADDR,GETDA,L'TALADDR                          
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
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READCHI                                      *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
READCHI  NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,(R2)                           
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,TALDIR,IOKEY,(R2),DMWORK                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV                       
*                                                                               
         SR    R0,R0                                                            
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
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
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
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL10              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
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
IALL10   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
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
         DROP  R3,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA FOR TALENT                                    *          
***********************************************************************         
         SPACE 1                                                                
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD20                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD20   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD10                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY RECORDS                      
         DC    CL3'AGG',AL1(0),AL4(LOADAGG) AGENCY GROUP RECORDS                
         DC    CL3'W4R',AL1(0),AL4(LOADW4R) W-4 RECORDS                         
         DC    CL3'AGT',AL1(0),AL4(LOADAGT) AGENT RECORDS                       
         DC    CL3'CLI',AL1(0),AL4(LOADCLI) CLIENT RECORDS                      
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT RECORDS                     
         DC    CL3'CST',AL1(0),AL4(LOADCST) CAST RECORDS                        
         DC    CL3'COM',AL1(0),AL4(LOADCOM) COMMERCIAL RECORDS                  
******** DC    CL3'STF',AL1(0),AL4(LOADSTF) STAFF RECORDS                       
         DC    CL3'GRT',AL1(0),AL4(LOADGRT) GUARANTEE RECORDS                   
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
*                                                                               
UPDT10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT20                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT20   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT10                                                           
*                                                                               
UPDTTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY RECORDS                      
         DC    CL3'AGG',AL1(0),AL4(UPDTAGG) AGENCY GROUP RECORDS                
         DC    CL3'W4R',AL1(0),AL4(UPDTW4R) W-4 RECORDS                         
         DC    CL3'AGT',AL1(0),AL4(UPDTAGT) AGENT RECORDS                       
         DC    CL3'CLI',AL1(0),AL4(UPDTCLI) CLIENT RECORDS                      
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT RECORDS                     
         DC    CL3'CST',AL1(0),AL4(UPDTCST) CAST RECORDS                        
         DC    CL3'COM',AL1(0),AL4(UPDTCOM) COMMERCIAL RECORDS                  
******** DC    CL3'STF',AL1(0),AL4(UPDTSTF) STAFF RECORDS                       
         DC    CL3'GRT',AL1(0),AL4(UPDTGRT) GUARANTEE RECORDS                   
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORD AND SUBSETS                                      *         
*      AGENCY RECORD (05300)                                          *         
*      AGENCY RECORD FOR NETWORK IDS (05301)                          *         
***********************************************************************         
         SPACE 1                                                                
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLAYD,R2                                                         
         XC    TLAYKEY,TLAYKEY     CLEAR FILL THE KEY                           
         MVI   TLAYCD,TLAYCDQ      RECORD 10 FOR AGENCY                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LAGYX                                                            
         CLC   TLAYCD(TLAYCD+L'TLAYCD-TLAYD),IOKEY                              
         JNE   LAGYX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTAGY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LAGY40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITAGY            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXAGYC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LAGY20   GOTO1 VTLXAGYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LAGY40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LAGY40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LAGY40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LAGY30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
LAGY30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LAGY20              TOO MANY IOS                                 
         J     LAGYX                                                            
*                                                                               
LAGY40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LAGY50                                                           
         GOTO1 AREADHI                                                          
         JE    LAGY50                                                           
         DC    H'0'                                                             
*                                                                               
LAGY50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY10                                                           
*                                                                               
LAGYX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY RECORD AND SUBSETS                                    *         
*        AGENCY RECORD (05300)                                        *         
*        AGENCY RECORD FOR NETWORK IDS (05301)                        *         
***********************************************************************         
         SPACE 1                                                                
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAYD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGY                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXAGYC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UAGY10   GOTO1 VTLXAGYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UAGYX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UAGYX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UAGY20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UAGY20   CLI   TLXREACT-TLXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UAGY40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TLXREACT-TLXRECD(RF),C'D'                                        
         J     UAGY40                                                           
*                                                                               
UAGY30   CLI   COPYFLAG,X'01'                                                   
         JNE   UAGY10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UAGY10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TLXREACT-TLXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UAGY40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UAGY30                                                           
*                                                                               
UAGYX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENCY RECORD AT R2                                          *         
***********************************************************************         
         SPACE 1                                                                
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING TLAYD,R2                                                         
         CLI   TLAYCD,TLAYCDQ      AGENCY RECORD?                               
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX00LEN          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY GROUP RECORD (05302)                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADAGG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGR RECORD             
         USING TLAGD,R2                                                         
         XC    TLAGKEY,TLAGKEY                                                  
         MVI   TLAGCD,TLAGCDQ      18 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGG10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLAGKEY(TLAGCD+L'TLAGCD-TLAGD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTLXAGGC,AINITAGG,AFILTAGG,VTLXCNVX                
         JNE   NO                                                               
         AP    PK02CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGG10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY GROUP RECORD DATA (05302)                             *         
***********************************************************************         
         SPACE 1                                                                
UPDTAGG  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAGD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGG                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGG                                                         
         GOTO1 ATALUPDT,DMCB,VTLXAGGC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENCY GROUP RECORD (05302)                                  *         
***********************************************************************         
         SPACE 1                                                                
FILTAGG  NTR1  BASE=*,LABEL=*                                                   
         USING TLAGD,R2                                                         
         CLI   TLAGCD,TLAGCDQ      X'18'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY GROUP RECORD (05302)                              *         
***********************************************************************         
         SPACE 1                                                                
INITAGG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX02LEN          R1=L'AGG RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD W-4 RECORD AND SUBSETS                                         *         
*      W-4 RECORD (05303)                                             *         
*      PID RECORD (05321)                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADW4R  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST W4 RECORD              
         USING TLW4D,R2                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ      60 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LW4R10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LW4RX                                                            
         CLC   TLW4KEY(TLW4CD+L'TLW4CD-TLW4D),IOKEY                             
         JNE   LW4RX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTW4R            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LW4R40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITW4R            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXW4RC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LW4R20   GOTO1 VTLXW4RC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LW4R40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LW4R40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LW4R40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LW4R30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
LW4R30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LW4R20              TOO MANY IOS                                 
         J     LW4RX                                                            
*                                                                               
LW4R40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LW4R50                                                           
         GOTO1 AREADHI                                                          
         JE    LW4R50                                                           
         DC    H'0'                                                             
*                                                                               
LW4R50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LW4R10                                                           
*                                                                               
LW4RX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE W-4 RECORD AND SUBSETS                                       *         
*        W-4 RECORD (05303)                                           *         
*        PID RECORD (05321)                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTW4R  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLW4D,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTW4R                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITW4R                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXW4RC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UW4R10   GOTO1 VTLXW4RC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UW4RX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UW4RX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UW4R20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UW4R20   CLI   TLXREACT-TLXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UW4R40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TLXREACT-TLXRECD(RF),C'D'                                        
         J     UW4R40                                                           
*                                                                               
UW4R30   CLI   COPYFLAG,X'01'                                                   
         JNE   UW4R10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UW4R10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TLXREACT-TLXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UW4R40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UW4R30                                                           
*                                                                               
UW4RX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER W-4 GROUP RECORD (05303)                                     *         
***********************************************************************         
         SPACE 1                                                                
FILTW4R  NTR1  BASE=*,LABEL=*                                                   
         USING TLW4D,R2                                                         
         CLI   TLW4CD,TLW4CDQ      X'60'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE W-4 GROUP RECORD (05303)                                 *         
***********************************************************************         
         SPACE 1                                                                
INITW4R  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX03LEN          R1=L'W4R RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENT RECORD (05304)                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADAGT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGT RECORD             
         USING TLAND,R2                                                         
         XC    TLANKEY,TLANKEY                                                  
         MVI   TLANCD,TLANCDQ      50 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLANKEY(TLANCD+L'TLANCD-TLAND),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTLXAGTC,AINITAGT,AFILTAGT,VTLXCNVX                
         JNE   NO                                                               
         AP    PK04CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGT10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENT GROUP RECORD DATA (05304)                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTAGT  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAND,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGT                                                         
         GOTO1 ATALUPDT,DMCB,VTLXAGTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENT GROUP RECORD (05304)                                   *         
***********************************************************************         
         SPACE 1                                                                
FILTAGT  NTR1  BASE=*,LABEL=*                                                   
         USING TLAND,R2                                                         
         CLI   TLANCD,TLANCDQ      X'50'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENT GROUP RECORD (05304)                               *         
***********************************************************************         
         SPACE 1                                                                
INITAGT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX04LEN          R1=L'AGT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORD (05305)                                          *         
***********************************************************************         
         SPACE 1                                                                
LOADCLI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RECORD             
         USING TLCLD,R2                                                         
         XC    TLCLKEY,TLCLKEY                                                  
         MVI   TLCLCD,TLCLCDQ      20 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLI10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLCLKEY(TLCLCD+L'TLCLCD-TLCLD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTLXCLIC,AINITCLI,AFILTCLI,VTLXCNVX                
         JNE   NO                                                               
         AP    PK05CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLI10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT RECORD DATA (05305)                                   *         
***********************************************************************         
         SPACE 1                                                                
UPDTCLI  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCLD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCLI                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCLI                                                         
         GOTO1 ATALUPDT,DMCB,VTLXCLIC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CLIENT RECORD (05305)                                        *         
***********************************************************************         
         SPACE 1                                                                
FILTCLI  NTR1  BASE=*,LABEL=*                                                   
         USING TLCLD,R2                                                         
         CLI   TLCLCD,TLCLCDQ      X'20'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CLIENT RECORD (05305)                                    *         
***********************************************************************         
         SPACE 1                                                                
INITCLI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX05LEN          R1=L'CLI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORD (05306)                                         *         
***********************************************************************         
         SPACE 1                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PRD RECORD             
         USING TLPRD,R2                                                         
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ      30 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLPRKEY(TLPRCD+L'TLPRCD-TLPRD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTLXPRDC,AINITPRD,AFILTPRD,VTLXCNVX                
         JNE   NO                                                               
         AP    PK06CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT RECORD DATA (05306)                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLPRD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPRD                                                         
         GOTO1 ATALUPDT,DMCB,VTLXPRDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT RECORD (05306)                                       *         
***********************************************************************         
         SPACE 1                                                                
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING TLPRD,R2                                                         
         CLI   TLPRCD,TLPRCDQ      X'30'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT RECORD (05306)                                   *         
***********************************************************************         
         SPACE 1                                                                
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX06LEN          R1=L'PRD RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CAST RECORD AND SUBSETS                                        *         
*      CAST RECORD (05307)                                            *         
*      OVERSCALE AMOUNT RECORD (05308)                                *         
*      OVERSCALE PERCENT RECORD (05309)                               *         
*      CAST ERROR RECORD (05310)                                      *         
*      EPISODE RECORD (05317)                                         *         
*      HOLDING FEE DOCUMENT ID (05322)                                *         
***********************************************************************         
         SPACE 1                                                                
LOADCST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST COMM RECORD            
         USING TLCAD,R2                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ      80 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCST10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LCSTX                                                            
         CLC   TLCAKEY(TLCACD+L'TLCACD-TLCAD),IOKEY                             
         JNE   LCSTX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTCST            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LCST40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITCST            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXCSTC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS,WORK                
*                                  GET NEXT UNCOMMITTED RECORD                  
LCST20   GOTO1 VTLXCSTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LCST40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LCST40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LCST40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LCST30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
LCST30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LCST20              TOO MANY IOS                                 
         J     LCSTX                                                            
*                                                                               
LCST40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LCST50                                                           
         GOTO1 AREADHI                                                          
         JE    LCST50                                                           
         DC    H'0'                                                             
*                                                                               
LCST50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCST10                                                           
*                                                                               
LCSTX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CAST RECORD DATA AND SUBSETS                                 *         
*        CAST RECORD (05307)                                          *         
*        OVERSCALE AMOUNT RECORD (05308)                              *         
*        OVERSCALE PERCENT RECORD (05309)                             *         
*        CAST ERROR RECORD (05310)                                    *         
*        EPISODE RECORD (05317)                                       *         
*        HOLDING FEE DOCUMENT ID (05322)                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTCST  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCAD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCST                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCST                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXCSTC,DMCB,DXAXREC,(R2),(1,0),(R6),0,WORK                     
*                                  GET NEXT UNCOMMITTED RECORD                  
UCST10   GOTO1 VTLXCSTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UCSTX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCSTX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UCST20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UCST20   CLI   TLXREACT-TLXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JNE   UCST25                                                           
         CLC   =C'05307',TLXRETYP-TLXRECD(RF)                                   
         JE    UCST22                                                           
         CLC   =C'05308',TLXRETYP-TLXRECD(RF)                                   
         JNE   UCST40                                                           
*                                                                               
* ON CAST RECORD ADD, ALWAYS ISSUE A KILL BEFORE THE ADD                        
* THIS PREVENTS THE DUPLICATE ADD WHEN CHANGES ARE MADE BY SELECTING            
* WITH A "C" FROM CAST/LIST AND UPDATING THE KEY - 12/6/11 SCHT                 
*                                                                               
UCST22   MVI   TLXREACT-TLXRECD(RF),C'D'   EXECUTE KILL FIRST                   
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         L     RF,DXASQLB                                                       
         MVI   TLXREACT-TLXRECD(RF),C'A'   THEN ADD                             
         JE    UCST40                                                           
*                                                                               
UCST25   MVI   COPYFLAG,X'01'                                                   
         MVI   TLXREACT-TLXRECD(RF),C'D'                                        
         J     UCST40                                                           
*                                                                               
UCST30   CLI   COPYFLAG,X'01'                                                   
         JNE   UCST10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UCST10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TLXREACT-TLXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UCST40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UCST30                                                           
*                                                                               
UCSTX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CAST RECORD (05307)                                          *         
***********************************************************************         
         SPACE 1                                                                
FILTCST  NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R2                                                         
         CLI   TLCACD,TLCACDQ      X'80'                                        
         JNE   FCSTNO                                                           
*                                                                               
         MVC   SVKEY,0(R2)         SAVE OFF KEY FOR LATER READ                  
         MVC   ICOM#,TLCACOM       SAVE OFF INTERNAL COMMERCIAL #               
         DROP  R2                                                               
*                                                                               
         USING TLCOPD,R2                                                        
         LA    R2,IOKEY                                                         
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ    COMMERCIAL RECORD PASSIVE                    
         MVC   TLCOCCOM,ICOM#      INTERNAL COMMERCIAL NUMBER                   
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
*                                                                               
         CLC   TLCOPKEY(TLCOCCOM+L'TLCOCCOM-TLCOPD),IOKEY                       
         JNE   FCSTNO                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   FCSTNO              PROBLEM WITH GETREC                          
*                                                                               
         LA    R3,TLCOELEM-TLCOD(R2)                                            
FCST10   CLI   0(R3),0                                                          
         JE    FCSTNO                                                           
         CLI   0(R3),TAACELQ       X'F4' - ACTIVITY ELEMENT                     
         JE    FCST20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         J     FCST10                                                           
*                                                                               
         USING TAACD,R3                                                         
FCST20   CLC   DXFDATEP,SPACES     TEST FROM DATE                               
         JNH   FCSTYES                                                          
         CLC   TAACCDTE,DXFDATEP                                                
         JL    FCSTNO                                                           
*                                                                               
FCSTYES  MVI   BYTE,1                                                           
         J     *+8                                                              
FCSTNO   MVI   BYTE,0                                                           
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JNE   FCSTX                                                            
         MVC   IOKEY,SVKEY         FOR LOADS ONLY RESET READ SEQ                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
*                                                                               
FCSTX    CLI   BYTE,1                                                           
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CAST RECORD (05307)                                      *         
***********************************************************************         
         SPACE 1                                                                
INITCST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX07LEN          R1=L'CST RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
*                                                                               
         USING TLXBADD,RE                                                       
         LA    RE,WORK                                                          
         MVC   TLXABUF,AHIDBUF     HOLDING FEE ID BUFFER                        
         MVC   TLXBUFFN,VBUFFRIN   BUFFERIN ADDRESS                             
         MVC   TLXCFAC,ACOMFACS    ADDRESS OF COMFACS                           
         DROP  RE                                                               
*                                                                               
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD COMMERCIAL RECORD AND SUBSETS                                  *         
*      COMMERCIAL RECORD DATA (05311)                                 *         
*      AFM RECORD DATA (05312)                                        *         
*      COMMERCIAL VERSION RECORD (05318)                              *         
*      MUSIC CODE RECORD (05319)                                      *         
*      COMMERCIAL PRODUCT RECORD (05320)                              *         
***********************************************************************         
         SPACE 1                                                                
LOADCOM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST COMM RECORD            
         USING TLCOD,R2                                                         
         XC    TLCOKEY,TLCOKEY                                                  
         MVI   TLCOCD,TLCOCDQ      40 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCOM10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LCOMX                                                            
         CLC   TLCOKEY(TLCOCD+L'TLCOCD-TLCOD),IOKEY                             
         JNE   LCOMX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AFILTCOM            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LCOM40                                                           
*                                                                               
         GOTO1 AINITCOM            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXCOMC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LCOM20   GOTO1 VTLXCOMC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LCOM40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LCOM40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LCOM40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LCOM30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
LCOM30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LCOM20              TOO MANY IOS                                 
         J     LCOMX                                                            
*                                                                               
LCOM40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LCOM50                                                           
         GOTO1 AREADHI                                                          
         JE    LCOM50                                                           
         DC    H'0'                                                             
*                                                                               
LCOM50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCOM10                                                           
*                                                                               
LCOMX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMMERCIAL RECORD AND SUBSETS                                *         
*        COMMERCIAL RECORD DATA (05311)                               *         
*        AFM RECORD DATA (05312)                                      *         
*        COMMERCIAL VERSION RECORD (05318)                            *         
*        MUSIC CODE RECORD (05319)                                    *         
*        COMMERCIAL PRODUCT RECORD (05320)                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTCOM  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCOD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCOM            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   UCOMX                                                            
*                                                                               
         GOTO1 AINITCOM            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXCOMC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UCOM10   GOTO1 VTLXCOMC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UCOMX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCOMX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UCOM20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UCOM20   CLI   TLXREACT-TLXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UCOM40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TLXREACT-TLXRECD(RF),C'D'                                        
         J     UCOM40                                                           
*                                                                               
UCOM30   CLI   COPYFLAG,X'01'                                                   
         JNE   UCOM10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UCOM10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TLXREACT-TLXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UCOM40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UCOM30                                                           
*                                                                               
UCOMX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER COMMERCIAL RECORD (05311)                                    *         
***********************************************************************         
         SPACE 1                                                                
FILTCOM  NTR1  BASE=*,LABEL=*                                                   
         USING TLCOD,R2                                                         
         CLI   TLCOCD,TLCOCDQ      X'40'                                        
         JNE   NO                                                               
*                                                                               
         LA    R3,TLCOELEM                                                      
FCOM10   CLI   0(R3),0                                                          
         JE    NO                                                               
         CLI   0(R3),TAACELQ       X'F4' - ACTIVITY ELEMENT                     
         JE    FCOM20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         J     FCOM10                                                           
*                                                                               
         USING TAACD,R3                                                         
FCOM20   CLC   DXFDATEP,SPACES     TEST FROM DATE                               
         JNH   YES                                                              
         CLC   TAACCDTE,DXFDATEP                                                
         JL    NO                                                               
*                                                                               
         J     YES                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE COMMERCIAL RECORD (05311)                                *         
***********************************************************************         
         SPACE 1                                                                
INITCOM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX11LEN          R1=L'COM RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD STAFF RECORD AND SUBSETS                                       *         
*      STAFF RECORD (05313)                                           *         
*      STAFF RECORD FOR LIMITED AGENCY RECORD (05314)                 *         
*      STAFF RECORD FOR LIMITED CLIENT RECORD (05315)                 *         
***********************************************************************         
         SPACE 1                                                                
LOADSTF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST STAFF RECORD           
         USING TLSTD,R2                                                         
         XC    TLSTKEY,TLSTKEY                                                  
         MVI   TLSTCD,TLSTCDQ      08 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTF10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LSTFX                                                            
         CLC   TLSTCD(TLSTCD+L'TLSTCD-TLSTD),IOKEY                              
         JNE   LSTFX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTSTF            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSTF40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITSTF            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXSTFC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS,AENCKEY             
*                                  GET NEXT UNCOMMITTED RECORD                  
LSTF20   GOTO1 VTLXSTFC,DMCB,DXAXREC,(R2),(2,0),(R6),0,AENCKEY                  
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LSTF40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LSTF40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSTF40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LSTF30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
LSTF30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LSTF20              TOO MANY IOS                                 
         J     LSTFX                                                            
*                                                                               
LSTF40   MVC   IOKEY(L'TLSTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LSTF50                                                           
         GOTO1 AREADHI                                                          
         JE    LSTF50                                                           
         DC    H'0'                                                             
*                                                                               
LSTF50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTF10                                                           
*                                                                               
LSTFX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE STAFF RECORD AND SUBSETS                                     *         
*        STAFF RECORD (05313)                                         *         
*        LIMITED AGENCY RECORD (05314)                                *         
*        LIMITED CLIENT RECORD (05315)                                *         
***********************************************************************         
         SPACE 1                                                                
UPDTSTF  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLSTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSTF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSTF                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTLXSTFC,DMCB,DXAXREC,(R2),(1,0),(R6),0,AENCKEY                  
*                                  GET NEXT UNCOMMITTED RECORD                  
USTF10   GOTO1 VTLXSTFC,DMCB,DXAXREC,(R2),(2,0),(R6),0,AENCKEY                  
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    USTFX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   USTFX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    USTF20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
USTF20   CLI   TLXREACT-TLXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    USTF40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TLXREACT-TLXRECD(RF),C'D'                                        
         J     USTF40                                                           
*                                                                               
USTF30   CLI   COPYFLAG,X'01'                                                   
         JNE   USTF10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    USTF10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TLXREACT-TLXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
USTF40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     USTF30                                                           
*                                                                               
USTFX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER STAFF RECORD (05313)                                         *         
***********************************************************************         
         SPACE 1                                                                
FILTSTF  NTR1  BASE=*,LABEL=*                                                   
         USING TLSTD,R2                                                         
         CLI   TLSTCD,TLSTCDQ      X'08'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE STAFF RECORD (05313)                                     *         
***********************************************************************         
         SPACE 1                                                                
INITSTF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX13LEN          R1=L'STF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GUARANTEE RECORD (05316)                                       *         
***********************************************************************         
         SPACE 1                                                                
LOADGRT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST GRT RECORD             
         USING TLGUD,R2                                                         
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      6C RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGRT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLGUKEY(TLGUCD+L'TLGUCD-TLGUD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTLXGRTC,AINITGRT,AFILTGRT,VTLXCNVX                
         JNE   NO                                                               
         AP    PK16CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGRT10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GUARANTEE RECORD DATA (05316)                                *         
***********************************************************************         
         SPACE 1                                                                
UPDTGRT  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLGUD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGRT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGRT                                                         
         GOTO1 ATALUPDT,DMCB,VTLXGRTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER GUARANTEE RECORD (05316)                                     *         
***********************************************************************         
         SPACE 1                                                                
FILTGRT  NTR1  BASE=*,LABEL=*                                                   
         USING TLGUD,R2                                                         
         CLI   TLGUCD,TLGUCDQ      X'6C'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GUARANTEE RECORD (05316)                                 *         
***********************************************************************         
         SPACE 1                                                                
INITGRT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX16LEN          R1=L'GRT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COUNT RECORD (05399)                                                *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
         SPACE 1                                                                
LOADCNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING RECTBD,R2                                                        
         LA    R2,RECTAB           SET KEY TO READ FIRST GRT RECORD             
         USING TLXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LCNT10   CLI   0(R2),EOF                                                        
         BE    LCNTX                                                            
*                                                                               
         GOTO1 AINITCNT            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TLXRELEN,TLXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TLXRELEN(2),=Y(TX99LEN)                                          
         MVC   TLXRETYP,=C'05399'                                               
*                                                                               
         MVI   TX99REC-1,MXTRTQ                                                 
         MVI   TX99CNT-1,MXTRTQ                                                 
*                                                                               
         MVC   TX99REC,RECRECNO                                                 
         SR    RE,RE                                                            
         ICM   RE,3,RECCNTDS                                                    
         LA    RE,WORKD(RE)                                                     
         ZAP   DUB,0(8,RE)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,TX99CNT                                                    
*                                                                               
         GOTO1 VTLXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         AHI   R2,RECTLNQ                                                       
         B     LCNT10              TOO MANY IOS                                 
*                                                                               
LCNTX    J     YES                                                              
         DROP  R2,R3                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD COUNT RECORD (05399)                              *         
***********************************************************************         
         SPACE 1                                                                
INITCNT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX99LEN          R1=L'CNT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
RECTAB   DS    0C                                                               
         DC    C'05300',AL2(PK00CNT-WORKD)                                      
         DC    C'05301',AL2(PK01CNT-WORKD)                                      
         DC    C'05302',AL2(PK02CNT-WORKD)                                      
         DC    C'05303',AL2(PK03CNT-WORKD)                                      
         DC    C'05304',AL2(PK04CNT-WORKD)                                      
         DC    C'05305',AL2(PK05CNT-WORKD)                                      
         DC    C'05306',AL2(PK06CNT-WORKD)                                      
         DC    C'05307',AL2(PK07CNT-WORKD)                                      
         DC    C'05308',AL2(PK08CNT-WORKD)                                      
         DC    C'05309',AL2(PK09CNT-WORKD)                                      
         DC    C'05310',AL2(PK10CNT-WORKD)                                      
         DC    C'05311',AL2(PK11CNT-WORKD)                                      
         DC    C'05312',AL2(PK12CNT-WORKD)                                      
         DC    C'05313',AL2(PK13CNT-WORKD)                                      
         DC    C'05314',AL2(PK14CNT-WORKD)                                      
         DC    C'05315',AL2(PK15CNT-WORKD)                                      
         DC    C'05316',AL2(PK16CNT-WORKD)                                      
         DC    C'05317',AL2(PK17CNT-WORKD)                                      
         DC    C'05318',AL2(PK18CNT-WORKD)                                      
         DC    C'05319',AL2(PK19CNT-WORKD)                                      
         DC    C'05320',AL2(PK20CNT-WORKD)                                      
         DC    C'05321',AL2(PK21CNT-WORKD)                                      
         DC    C'05322',AL2(PK22CNT-WORKD)                                      
         DC    AL1(EOF)                                                         
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
         SPACE 1                                                                
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
CHG      USING TLRCD,R2                                                         
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R4,RECVHDR+L'RECVHDR                                             
CPY      USING TLRCD,R4                                                         
*                                                                               
         CLC   CHG.TLRCLEN,CPY.TLRCLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         XR    R3,R3                                                            
         ICM   R3,3,CPY.TLRCLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
         DROP  CHG,CPY                                                          
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT TALENT RECORDS IN LOAD MODE                   *         
* R2 = A(TALENT DIRECTORY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
TALLOAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   ALPARMS,0(R1)                                                    
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    TLOA10                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   TLOA40              NOT VALID - GET NEXT                         
*                                                                               
TLOA10   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
TLOA20   GOTO1 (R4)                INITIALIZE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    TLOA40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    TLOA40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   TLOA40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    TLOA30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 ALPACNVX,DMCB,(R7)                                               
*                                                                               
TLOA30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
TLOA40   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         CLI   TYPEDEEP,0                                                       
         JNE   TLOA50              YES, NEED TO READ HIGH                       
*                                                                               
         GOTO1 ACHKSEQ             SEE IF READ SEQUENCE BROKEN                  
         JE    TLOA60              NO                                           
         GOTO1 AREADHI                                                          
         JE    TLOA60                                                           
         DC    H'0'                                                             
*                                                                               
TLOA50   LA    RF,IOKEY            READ NEXT RECORD - HIGH                      
         USING TLDRD,RF                                                         
         SR    RE,RE                                                            
         IC    RE,TYPEDEEP         LEVEL TO FILTER AT                           
         BCTR  RE,0                                                             
         LA    R1,TLDRCD                                                        
         AR    R1,RE                                                            
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)            INCREMENT IT                                 
         STC   RE,0(R1)                                                         
         DROP  RF                                                               
*                                                                               
         GOTO1 AREADHI                                                          
         JE    YES                                                              
         J     NO                                                               
*                                                                               
TLOA60   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORDS IN LOAD MODE            *         
* R2 = A(TALENT DIRECTORY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
CXLOAD   NTR1  BASE=*,LABEL=*                                                   
         MVC   ALPARMS,0(R1)                                                    
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               TEST IF FILTER ROUTINE PASSED                
         JZ    CXLOAD10                                                         
         GOTO1 (R5)                FILTER RECORD                                
         JNE   CXLOAD40            GET NEXT - NOT VALID                         
*                                                                               
CXLOAD10 GOTO1 (R4)                INITIALIZE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
         MVC   BYTE,DMCB+8         SAVE DMCB+8                                  
         NI    DMCB+8,X'FF'-X'01'                                               
         CLI   DMCB+8,X'88'                                                     
         BE    CXLOAD30            TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    CXLOAD40            TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   CXLOAD40                                                         
         CLI   SXDTPLFM,0                                                       
         BE    CXLOAD20                                                         
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 ALPACNVX,DMCB,(R7)                                               
*                                                                               
CXLOAD20 GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         TM    BYTE,X'40'          TEST IF CALL BACK REQUIRED                   
         BO    CXLOAD10                                                         
*                                                                               
CXLOAD30 GOTO1 ADECIOC             DECREMENT IO COUNTER                         
         JNE   NO                                                               
*                                                                               
CXLOAD40 MVC   IOKEY(L'IOKEY),0(R2)   READ NEXT RECORD                          
         TM    BYTE,X'01'                                                       
         BZ    CXLOAD50            TEST READ SEQUENCE BROKEN                    
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CXLOAD50 GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT TALENT RECORDS IN UPDATE MODE                 *         
* R2 = A(TALENT RECORD BUFFER)                                        *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
         SPACE 1                                                                
TALUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    TALU20              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   TALU10                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(COPYBUFL)                                                  
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6)         BUILD COPY REC            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
*                                                                               
         LH    R1,=AL2(TX00AGY-TLXRECD) DISP TO COMPANY                         
         AR    R0,R1               BUMP TO COMPANY CODE IN BOTH RECS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
         SH    RF,=AL2(L'TX00X)    DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
TALU10   GOTO1 VTLXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
TALU20   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     YES                                                              
         DROP  R5                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED      *         
***********************************************************************         
         SPACE 1                                                                
COPYBUFF DS    8096C                                                            
COPYBUFL EQU   *-COPYBUFF                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
AENCKEY  DS    A                   A(PASSWORD ENCRYPTION KEY OR 0)              
TALADDR  DS    F                   DISK ADDRESS                                 
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
COPYFLAG DS    XL1                 UPDATE FLAG FOR COPY                         
SVKEY    DS    CL(L'IOKEY)                                                      
ICOM#    DS    CL(L'TLCACOM)                                                    
*                                                                               
PRIALPHA DS    CL2                 PRIMARY ALPHA                                
COMPANY  DS    XL1                                                              
VERSION  DS    XL1                                                              
*                                                                               
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
WORK     DS    XL64                                                             
*                                                                               
ALPARMS  DS    0XL16                                                            
ALPAEXTR DS    A                   EXTRACT ROUTINE                              
ALPAINIT DS    A                   INITIALISATION ROUTINE                       
ALPAFILT DS    A                   FILTER ROUTINE                               
ALPACNVX DS    A                   CONVERT ROUTINE                              
*                                                                               
HIDREC   DS    CL(L'TX22HFID)                                                   
*                                                                               
PKCNTS   DS    0C                                                               
PK00CNT  DS    PL8                 05300 REC COUNT (AGENCY)                     
PKCNTLNQ EQU   *-PKCNTS                                                         
PK01CNT  DS    PL8                 05301 REC COUNT (LIMITED NETWORK)            
PK02CNT  DS    PL8                 05302 REC COUNT (AGENCY GROUP)               
PK03CNT  DS    PL8                 05303 REC COUNT (W-4)                        
PK04CNT  DS    PL8                 05304 REC COUNT (AGENT)                      
PK05CNT  DS    PL8                 05305 REC COUNT (CLIENT)                     
PK06CNT  DS    PL8                 05306 REC COUNT (PRODUCT)                    
PK07CNT  DS    PL8                 05307 REC COUNT (CAST)                       
PK08CNT  DS    PL8                 05308 REC COUNT (OVERSCALE AMOUNT)           
PK09CNT  DS    PL8                 05309 REC COUNT (OVERSCALE PERCENT)          
PK10CNT  DS    PL8                 05310 REC COUNT (CAST ERRORD)                
PK11CNT  DS    PL8                 05311 REC COUNT (COMMERCIAL)                 
PK12CNT  DS    PL8                 05312 REC COUNT (AFM)                        
PK13CNT  DS    PL8                 05313 REC COUNT (STAFF)                      
PK14CNT  DS    PL8                 05314 REC COUNT (LIMITED AGENCY)             
PK15CNT  DS    PL8                 05315 REC COUNT (LIMITED CLIENT)             
PK16CNT  DS    PL8                 05316 REC COUNT (GUARANTEE)                  
PK17CNT  DS    PL8                 05317 REC COUNT (EPISODE)                    
PK18CNT  DS    PL8                 05318 REC COUNT (COMMERCIAL VERSION)         
PK19CNT  DS    PL8                 05319 REC COUNT (MUSIC CODE)                 
PK20CNT  DS    PL8                 05320 REC COUNT (COMMERCIAL PRODUCT)         
PK21CNT  DS    PL8                 05321 REC COUNT (PID)                        
PK22CNT  DS    PL8                 05322 REC COUNT (HOLDING FEES ID)            
PKCNTQ   EQU   (*-PKCNTS)/PKCNTLNQ                                              
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
*                                                                               
EOF      EQU   X'FF'                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
         DS    XL3                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER COMMON ADDRESSES                                     *         
***********************************************************************         
         SPACE 1                                                                
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VBUFFRIN DS    V                                                                
VTLXCNVX DS    V                                                                
*                                                                               
* TLXROUTS ADDRESSES                                                            
VTLXAGYC DS    V                   AGENCY RECORDS                               
VTLXAGGC DS    V                   AGENCY GROUP RECORDS                         
VTLXW4RC DS    V                   W-4 RECORD                                   
VTLXAGTC DS    V                   AGENT RECORD                                 
VTLXCLIC DS    V                   CLIENT RECORD                                
VTLXPRDC DS    V                   PRODUCT RECORD                               
VTLXCSTC DS    V                   CAST RECORD                                  
VTLXCOMC DS    V                   COMMERCIAL RECORD                            
VTLXSTFC DS    V                   STAFF RECORD                                 
VTLXGRTC DS    V                   GUARANTEE RECORD                             
* END OF TLXROUTS                                                               
*                                                                               
         DS    CL8                 COMMON INTERNAL ROUTINES                     
ATALLOAD DS    A                                                                
ACXLOAD  DS    A                                                                
ATALUPDT DS    A                                                                
ADECIOC  DS    A                                                                
ACHKSEQ  DS    A                                                                
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                   READ HIGH ON THE TALENT FILE                 
AREADCHI DS    A                   READ HIGH ON THE CONTROL FILE                
ARECCMP  DS    A                   COMPARE CHANGE/COPY RECORD                   
AHIDBUF  DS    A                   HOLDING FEE ID TABLE FOR BUFFERIN            
ACOMFACS DS    A                   ADDRESS OF COMFACS FOR BUFFERIN              
*                                                                               
* LOAD ROUTINE ADDRESSES                                                        
         DS    CL8                 LOAD ROUTINES                                
ALOADALL DS    A                   ALL                                          
ALOADAGY DS    A                   AGENCY                                       
ALOADAGG DS    A                   AGENCY GROUP                                 
ALOADW4R DS    A                   W-4                                          
ALOADAGT DS    A                   AGENT                                        
ALOADCLI DS    A                   CLIENT                                       
ALOADPRD DS    A                   PRODUCT                                      
ALOADCST DS    A                   CAST                                         
ALOADCOM DS    A                   COMMERCIAL                                   
ALOADSTF DS    A                   STAFF                                        
ALOADGRT DS    A                   GUARANTEE                                    
ALOADCNT DS    A                   RECORD COUNT RECORD                          
* END OF LOAD ROUTINE ADDRESSES                                                 
*                                                                               
* UPDATE ROUTINE ADDRESSES                                                      
         DS    CL8                 UPDATE ROUTINES                              
AUPDTALL DS    A                   ALL RECORDS                                  
AUPDTAGY DS    A                   AGENCY                                       
AUPDTAGG DS    A                   AGENCY GROUP                                 
AUPDTW4R DS    A                   W-4                                          
AUPDTAGT DS    A                   AGENT                                        
AUPDTCLI DS    A                   CLIENT                                       
AUPDTPRD DS    A                   PRODUCT                                      
AUPDTCST DS    A                   CAST                                         
AUPDTCOM DS    A                   COMMERCIAL                                   
AUPDTSTF DS    A                   STAFF                                        
AUPDTGRT DS    A                   GUARANTEE                                    
         DS    A                   RECORD COUNT RECORD                          
* END OF UPDATE ROUTINE ADDRESSES                                               
*                                                                               
* FILTER ROUTINE ADDRESSES                                                      
         DS    CL8                 FILTER ROUTINES                              
AFILTAGY DS    A                   AGENCY                                       
AFILTAGG DS    A                   AGENCY GROUP                                 
AFILTW4R DS    A                   W-4                                          
AFILTAGT DS    A                   AGENT                                        
AFILTCLI DS    A                   CLIENT                                       
AFILTPRD DS    A                   PRODUCT                                      
AFILTCST DS    A                   CAST                                         
AFILTCOM DS    A                   COMMERCIAL                                   
AFILTSTF DS    A                   STAFF                                        
AFILTGRT DS    A                   GUARANTEE                                    
         DS    A                   RECORD COUNT RECORD                          
* END OF FILTER ROUTINE ADDRESSES                                               
*                                                                               
* INITIALIZATION ROUTINE ADDRESSES                                              
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   ADD RECORDS                                  
AINITAGY DS    A                   AGENCY                                       
AINITAGG DS    A                   AGENCY GROUP                                 
AINITW4R DS    A                   W-4                                          
AINITAGT DS    A                   AGENT                                        
AINITCLI DS    A                   CLIENT                                       
AINITPRD DS    A                   PRODUCT                                      
AINITCST DS    A                   CAST                                         
AINITCOM DS    A                   COMMERCIAL                                   
AINITSTF DS    A                   STAFF                                        
AINITGRT DS    A                   GUARANTEE                                    
AINITCNT DS    A                   RECORD COUNT RECORD                          
* END OF INITIALIZATION ADDRESSES                                               
*                                                                               
DMOPEN   DS    CL7                                                              
DMREAD   DS    CL7                                                              
DMRSEQ   DS    CL7                                                              
DMRDHI   DS    CL7                                                              
DMCLSE   DS    CL7                                                              
DMFAST   DS    CL7                                                              
GETREC   DS    CL7                                                              
DMRFIL   DS    CL7                                                              
CONTROL  DS    CL7                                                              
CTFILE   DS    CL7                                                              
TALDIR   DS    CL7                                                              
TALFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
SPACES   DS    CL80                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECORD COUNT TABLE                                   *         
***********************************************************************         
         SPACE 1                                                                
RECTBD   DSECT                                                                  
RECRECNO DS    CL5                 RECORD NUMBER                                
RECCNTDS DS    XL2                 DISPLACMENET TO RECORD COUNT FIELD           
RECTLNQ  EQU   *-RECTBD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
         SPACE 1                                                                
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
         SPACE 1                                                                
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
*                                                                               
* TLXRECD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TLXRECD                                                        
         PRINT ON                                                               
*                                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*                                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*                                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
*                                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*                                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
*                                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TLXTRACT  03/15/18'                                      
         END                                                                    
