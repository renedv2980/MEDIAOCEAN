*          DATA SET RXTRACT    AT LEVEL 068 AS OF 05/26/05                      
*PHASE RXTRACTB                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE RXROUTS                  XTRACT RECORD CREATION MODULE                 
*INCLUDE RXCNVX                   CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE REPVALM3                 CONTRACT BUCKET MODULE                        
         SPACE 1                                                                
         TITLE 'REPXTRACT - EXTRACT REP SYSTEM FILE SQL DATA'                   
***********************************************************************         
*  REP  SQL SUB SYSTEM EXTRACT CONTROL MODULE                         *         
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
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM, SEE RXUSERD              *         
*---------------------------------------------------------------------*         
*                                                                     *         
* 09/05/2001  JRD   HANDLE REPCODE OVERRIDE FROM RXUSERD              *         
*                   INCLUDE XRER FLAG ON CONTRACT                     *         
*                                                                     *         
* 09/18/2001  JRD   REMOVE GROUP FROM XSP EXTRACT                     *         
*                                                                     *         
* 09/19/2001  JRD   INCLUDE XRER FLAG ON DOLLAR/BUCKET                *         
*                                                                     *         
* 10/30/2001  JRD   MAKE CONTRACT TYPE A SUBSIDIARY RECORD            *         
*                   CLEAN UP TABLE DRIVEN RECORD TYPES TO             *         
*                   USE A SINGLE ENTRY POINT                          *         
*                                                                     *         
* 10/31/2001  JRD   MAKE CONTRACT TYPE A MASTER RECORD BUT            *         
*                   HANDLE IT SEPERATELY IN JCL FOR CLEAR CHANNEL     *         
*                   SINCE KATZ STILL HAS NOT LEARNED TO USE SQL       *         
*                                                                     *         
* 11/28/2001  JRD   MAKE CONTRACT TYPE A SUBSIDIARY RECORD            *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RXTRACT  CSECT                                                                  
         ENTRY COMFACS                                                          
         ENTRY MASTC                                                            
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,**RXTR**                                                   
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
DXU      USING RXUSERD,DXUSER                                                   
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         MVC   PLATFORM,SXDTPLFM                                                
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
LOW      SR    RC,RC                                                            
         CHI   RC,256                                                           
         J     EXIT                                                             
*                                                                               
HIGH     CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     DS   0H                                                                
         OC    DXDDSIO,DXDDSIO                                                  
         BZ    MINIT010                                                         
*                                                                               
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
MINIT010 DS    0H                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BRAS  RE,SETDATES                                                      
         BNE   MERR                                                             
*                                                                               
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
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
*                                                                               
         L     R4,=A(VDEMADDR)                                                  
         OC    0(4,R4),0(R4)       IS DEMADDR ALREADY LOADED?                   
         BNZ   GENINITX            YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADE'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINITX DS    0H                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,REP,REPFILES,IO                             
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,REP,0,IO                                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
REP      DC    CL8'REP    '                                                     
REPFILES DC    C'NREPDIR NREPFILEX'                                             
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                              *         
***********************************************************************         
         SPACE 1                                                                
PROCLOAD NTR1  ,                                                                
         MVC   REPALPHA,SXDTAGY    SET REPALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         BRAS  RE,REPINIT                                                       
*                                                                               
         TM    TYPEFLAG,TYPFLTAB   LOAD FROM TABLE?                             
         JZ    LOAD10              NO                                           
*                                                                               
         ICM   R3,15,TYPEALOD                                                   
         USING TYPTABD,R3                                                       
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,TYPVER                                                   
         JL    LOAD04                                                           
         MVC   TYPECODE,TYPNAME                                                 
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,TYPLOAD                                                    
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,TYPTABLQ(R3)                                                  
         J     LOAD02                                                           
         DROP  R3                                                               
*                                                                               
LOAD10   DS    0H                                                               
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
         SPACE 1                                                                
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   REPALPHA,SXDTAGY    SET REPALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
         CLI   RFILTY,REPFILQ      TEST REPFIL FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
*                                                                               
         BRAS  RE,REPINIT                                                       
*                                                                               
         TM    TYPEFLAG,TYPFLTAB   UPDATE FROM TABLE?                           
         JZ    UPDT10              NO                                           
*                                                                               
         ICM   R3,15,TYPEAUPD                                                   
         USING TYPTABD,R3                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,TYPVER                                                   
         JL    UPDT04                                                           
         MVC   TYPECODE,TYPNAME                                                 
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,TYPUPDT                                                    
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,TYPTABLQ(R3)                                                  
         J     UPDT02                                                           
         DROP  R3                                                               
*                                                                               
UPDT10   DS    0H                                                               
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
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
RX       USING RREPREC,RECVHDR+L'RECVHDR                                        
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         TM    RX.RREPCNTL,X'80'      IS THIS RECORD DELETED?                   
         BZ    PKEY02              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+RREPCNTL-RREPREC+4(R4),X'80'                           
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY02   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+RREPCNTL-RREPREC+4(R4),X'80'                           
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  RX,R5                                                            
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
         DC    V(RXCNVX)                                                        
**       DC    V(AXDACC)                                                        
**       DC    V(AXDTRC)                                                        
*                                                                               
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    V(REPDOLC)          CONTRACT DOLLARS                             
         DC    V(REPAGYC)          AGENCY                                       
         DC    V(REPADVC)          ADVERTISER                                   
         DC    V(REPSALC)          SALESPERSON                                  
         DC    V(REPCTYC)          CONTRACT TYPE                                
         DC    V(REPDCTC)          DEV CONTRACT TYPE                            
         DC    V(REPSTAC)          STATION                                      
         DC    V(REPDSPC)          DEV SALESPERSON                              
         DC    V(REPTEMC)          TEAM                                         
         DC    V(REPPRDC)          PRODUCT CODE          T                      
         DC    V(REPCLSC)          CLASS                                        
         DC    V(REPCATC)          CATEGORY                                     
         DC    V(REPOFFC)          OFFICE                                       
         DC    V(REPOWNC)          OWNER                                        
         DC    V(REPMKTC)          MARKET                                       
         DC    V(REPGRPC)          GROUP/SUBGROUP                               
         DC    V(REPMSTC)          SUB/MASTER                                   
         DC    V(REPREPC)          REP INFO                                     
*                                                                               
         SPACE 1                                                                
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
         SPACE 1                                                                
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(LOADDOL)          CONTRACT DOLLARS                             
         DC    A(LOADAGY)          AGENCY                                       
         DC    A(LOADADV)          ADVERTISER                                   
         DC    A(LOADSAL)          SALESPERSON                                  
         DC    A(LOADCTY)          CONTRACT TYPE                                
         DC    A(LOADDCT)          DEV CONTRACT TYPE                            
         DC    A(LOADSTA)          STATION                                      
         DC    A(LOADDSP)          DEV SALESPERSON                              
         DC    A(LOADTEM)          TEAM                                         
         DC    A(LOADPRD)          PRODUCT CODE          T                      
         DC    A(LOADCLS)          CLASS                                        
         DC    A(LOADCAT)          CATEGORY                                     
         DC    A(LOADOFF)          OFFICE                                       
         DC    A(LOADOWN)          OWNER                                        
         DC    A(LOADMKT)          MARKET                                       
         DC    A(LOADGRP)          GROUP/SUBGROUP                               
         DC    A(LOADMST)          SUB/MASTER                                   
         DC    A(LOADREP)          REP INFO                                     
         SPACE 1                                                                
         DC    CL8'UPDTING'                                                     
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(UPDTDOL)          CONTRACT DOLLARS                             
         DC    A(UPDTAGY)          AGENCY                                       
         DC    A(UPDTADV)          ADVERTISER                                   
         DC    A(UPDTSAL)          SALESPERSON                                  
         DC    A(UPDTCTY)          CONTRACT TYPE                                
         DC    A(UPDTDCT)          DEV CONTRACT TYPE                            
         DC    A(UPDTSTA)          STATION                                      
         DC    A(UPDTDSP)          DEV SALESPERSON                              
         DC    A(UPDTTEM)          TEAM                                         
         DC    A(UPDTPRD)          PRODUCT CODE          T                      
         DC    A(UPDTCLS)          CLASS                                        
         DC    A(UPDTCAT)          CATEGORY                                     
         DC    A(UPDTOFF)          OFFICE                                       
         DC    A(UPDTOWN)          OWNER                                        
         DC    A(UPDTMKT)          MARKET                                       
         DC    A(UPDTGRP)          GROUP/SUBGROUP                               
         DC    A(UPDTMST)          SUB/MASTER                                   
         DC    A(UPDTREP)          REP INFO                                     
         SPACE 1                                                                
         DC    CL8'FILTERS'                                                     
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(FILTDOL)          CONTRACT DOLLARS                             
         DC    A(FILTAGY)          AGENCY                                       
         DC    A(FILTADV)          ADVERTISER                                   
         DC    A(FILTSAL)          SALESPERSON                                  
         DC    A(FILTCTY)          CONTRACT TYPE                                
         DC    A(FILTDCT)          DEV CONTRACT TYPE                            
         DC    A(FILTSTA)          STATION                                      
         DC    A(FILTDSP)          DEV SALESPERSON                              
         DC    A(FILTTEM)          TEAM                                         
         DC    A(FILTPRD)          PRODUCT CODE                                 
         DC    A(FILTCLS)          CLASS                                        
         DC    A(FILTCAT)          CATEGORY                                     
         DC    A(FILTOFF)          OFFICE                                       
         DC    A(FILTOWN)          OWNER                                        
         DC    A(FILTMKT)          MARKET                                       
         DC    A(FILTGRP)          GROUP/SUBGROUP                               
         DC    A(FILTMST)          SUB/MASTER                                   
         DC    A(FILTREP)          REP INFO                                     
         SPACE 1                                                                
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(CONERR)           CONTRACT HEADER                              
         DC    A(INITDOL)          CONTRACT DOLLARS                             
         DC    A(INITAGY)          AGENCY                                       
         DC    A(INITADV)          ADVERTISER                                   
         DC    A(INITSAL)          SALESPERSON                                  
         DC    A(INITCTY)          CONTRACT TYPE                                
         DC    A(INITDCT)          DEV CONTRACT TYPE                            
         DC    A(INITSTA)          STATION                                      
         DC    A(INITDSP)          DEV SALESPERSON                              
         DC    A(INITTEM)          TEAM                                         
         DC    A(INITPRD)          PRODUCT CODE          T                      
         DC    A(INITCLS)          CLASS                                        
         DC    A(INITCAT)          CATEGORY                                     
         DC    A(INITOFF)          OFFICE                                       
         DC    A(INITOWN)          OWNER                                        
         DC    A(INITMKT)          MARKET                                       
         DC    A(INITGRP)          GROUP/SUBGROUP                               
         DC    A(INITMST)          SUB/MASTER                                   
         DC    A(INITREP)          REP INFO                                     
         SPACE 1                                                                
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
         DC    C'REPDIR '                                                       
         DC    C'REPFIL '                                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    CL1'Y'                                                           
         DC    80C' '                                                           
*                                                                               
*                                                                               
         LTORG                                                                  
REPFILQ  EQU   X'82'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPARATOR CHR                          
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
TYPTAB   DS    0L                                                               
*****                                                                           
***** RECORD GROUP TABLES                                                       
*****                                                                           
         DC    CL3'ALL',AL1(00,01,00,00,00),AL4(ALLTAB,ALLTAB)                  
         DC    CL3'MAS',AL1(00,01,00,00,00),AL4(MASTAB,MASTAB)                  
         DC    CL3'SUB',AL1(00,01,00,00,00),AL4(SUBTAB,SUBTAB)                  
         DC    CL3'SUP',AL1(00,01,00,00,00),AL4(SUPTAB,SUPTAB)                  
         DC    CL3'XSP',AL1(00,01,00,00,00),AL4(XSPTAB,XSPTAB)                  
*****                                                                           
***** INDIVIDUAL RECORD ROUNTINES                                               
*****                                                                           
*                                                                               
* HANDLED BY THE DOLLAR RECORD                                                  
*        DC    CL3'CON',AL1(00,00,00,00,00),AL4(LOADCON,UPDTCON)                
*                                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------------            
* ALL RECORDS                                                                   
*-------------------------------------------------------------------            
ALLTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* SUBSIDIARY LEVEL RECORDS                                                      
*-------------------------------------------------------------------            
SUBTAB   DS    0L                                                               
         DC    CL3'DOL',AL1(00,00,00,00,00),AL4(LOADDOL,UPDTDOL)                
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* SUPPORT RECORDS AT THE SUBSIDIARY LEVEL                                       
*-------------------------------------------------------------------            
SUPTAB   DS    0L                                                               
         DC    CL3'STA',AL1(00,00,00,00,00),AL4(LOADSTA,UPDTSTA)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* MASTER LEVEL RECORDS                                                          
*-------------------------------------------------------------------            
MASTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
**JRD    DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOADCTY,UPDTCTY)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'GRP',AL1(00,00,00,00,00),AL4(LOADGRP,UPDTGRP)                
         DC    CL3'MST',AL1(00,00,00,00,00),AL4(LOADMST,UPDTMST)                
         DC    CL3'REP',AL1(00,00,00,00,00),AL4(LOADREP,UPDTREP)                
         DC    X'00'                                                            
*-------------------------------------------------------------------            
* EXTRA SUPPORT RECORDS AT THE MASTER LEVEL FOR CLEAR CHANNEL                   
*  THESE RECORDS ARE EXTRACTED FROM K3 AND OVERRIDDEN TO 'NU'                   
*  AS WOULD BE DONE BY THE INTEREP TABLE IN DATAMANAGER                         
*                                                                               
*  GROUP, REP, MASTERSUBSIDIARY                                                 
*  ARE HANDLED IN THE JCL AND EXTRACTED FROM 'NU' AS 'NU'                       
*-------------------------------------------------------------------            
XSPTAB   DS    0L                                                               
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'ADV',AL1(00,00,00,00,00),AL4(LOADADV,UPDTADV)                
         DC    CL3'SAL',AL1(00,00,00,00,00),AL4(LOADSAL,UPDTSAL)                
         DC    CL3'DCT',AL1(00,00,00,00,00),AL4(LOADDCT,UPDTDCT)                
         DC    CL3'DSP',AL1(00,00,00,00,00),AL4(LOADDSP,UPDTDSP)                
         DC    CL3'TEM',AL1(00,00,00,00,00),AL4(LOADTEM,UPDTTEM)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'CLS',AL1(00,00,00,00,00),AL4(LOADCLS,UPDTCLS)                
         DC    CL3'CAT',AL1(00,00,00,00,00),AL4(LOADCAT,UPDTCAT)                
         DC    CL3'OWN',AL1(00,00,00,00,00),AL4(LOADOWN,UPDTOWN)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
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
*       REPADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),REPFIL,REPADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,REPADDR,GETDA,L'REPADDR,0                        
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HIGH                                                             
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
         SPACE 1                                                                
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
         GOTO1 VDATAMGR,DMCB,DMRDHI,REPDIR,IOKEY,(R2),DMWORK                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV,0                     
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
CHG      USING RREPREC,R2                                                       
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R4,RECVHDR+L'RECVHDR                                             
CPY      USING RREPREC,R4                                                       
*                                                                               
         CLC   CHG.RREPLEN,CPY.RREPLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         XR    R3,R3                                                            
         ICM   R3,3,CPY.RREPLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
         LTORG                                                                  
         DROP  CHG,CPY                                                          
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
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
         JE    IALL02              YES                                          
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
         MVC   DXHDRCDT+07(2),SPACES                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CONTRACT RECORD CALLS BRANCH HERE                                             
*---------------------------------------------------------------------*         
CONERR   DC    H'0'                                                             
         DC    C'CONTRACT ROUTINES SHOULD NOT BE CALLED, ALL DATA IS'           
         DC    C' HANDLED IN THE DOLLAR RECORD ROUTINES'                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CONTRACT DOLLAR RECORD                                         *         
*        ROUTINE WILL BUILD BOTH DOLLAR BUCKET RECORDS AND            *         
*        CONTRACT HEADER RECORDS AT THE SAME TIME                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDOL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
         USING RCONREC,R2                                                       
         XC    RCONKEY,RCONKEY                                                  
*                                                                               
         MVI   RCON8ETP,X'8E'                                                   
         MVC   RCON8ERP,REPALPHA                                                
*                                 CHECK STATION FILTER                          
         CLC   DXU.RXUSTA,SPACES                                                
         JNH   *+10                                                             
         MVC   RCON8EST,DXU.RXUSTA                                              
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LDOL0020 TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(RCON8EST-RCONKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         L     R5,=A(STAXREC)                                                   
*                                                                               
         CLC   4(5,R5),RCON8EST-RCONKEY(R2)                                     
         JE    LDOL0030            STATION DIDN'T CHANGE                        
*                                                                               
         OC    4(5,R5),4(R5)       FIRST TIME?                                  
         JZ    LDOL0022            YES                                          
*                                                                               
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JH    YES                 YES - WE MUST BE DONE                        
*                                                                               
LDOL0022 DS    0H                                                               
         MVC   WORK(L'IOKEY),0(R2)      SAVE THE CONTRACT KEY                   
K        USING RSTAKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,REPALPHA                                              
         MVC   K.RSTAKSTA,RCON8EST-RCONKEY(R2)                                  
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(L'RSTAKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
R        USING RSTAREC,R2                                                       
         MVC   00(2,R5),=X'0248'                                                
         MVC   02(2,R5),REPALPHA    INSERT REP CODE                             
         MVC   04(5,R5),R.RSTAKSTA                                              
         MVC   09(3,R5),R.RSTASTRT                                              
         MVC   12(3,R5),R.RSTAEND                                               
         MVC   15(2,R5),REPALPHA    INSERT REP CODE                             
         MVC   17(20,R5),R.RSTAMKT                                              
         OC    17(20,R5),SPACES  SET X'00' TO SPACES                            
         MVC   37(2,R5),R.RSTAGRUP                                              
         MVC   39(3,R5),R.RSTAAFFL STATION AFFILIATE                            
         MVC   42(2,R5),=C'T '   DEFAULT NY TEAM CODE                           
         MVC   44(2,R5),R.RSTATVB  TVB REGION                                   
         MVC   46(3,R5),R.RSTAOWN  STATION OWNER                                
         MVC   49(1,R5),R.RSTARANK STATION MARKET RANK                          
         MVC   50(2,R5),=C'T '   DEFAULT CH TEAM CODE                           
         XC    52(2,R5),WORK+52  DISP OF LAST STA IN MKT                        
         MVC   68(2,R5),R.RSTACLDT INSERT CLOSE DATE OF STATION                 
         DROP  R                                                                
*                                                                               
         MVC   IOKEY,WORK          RESTORE CONTRACT                             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LDOL0030 DS    0H                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AFILTDOL            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LDOL0080                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LDOL0080                                                         
*                                                                               
         TM    REPPFLG,X'40'       FILTER BACK BILLING?                         
         JZ    LDOL0032            NO                                           
*                                                                               
R        USING RCONREC,R2                                                       
         CLC   =C'ACC-BB',RCONBUYR                                              
         JE    LDOL0080                                                         
         DROP  R                                                                
*                                                                               
LDOL0032 DS    0H                                                               
*                                                                               
*   EXCLUDE ALL INTEREP/INFINITY ORDERS FOR GROUP/SUBGROUP 'Q'                  
*                                                                               
         CLC   =C'IF',RCONKREP     INFINITY?                                    
         BNE   LDOL0033            NO                                           
         CLI   RCONKGRP,C'Q'       YES - GROUP 'Q'?                             
         BE    LDOL0080            YES - TOSS THE RECORD                        
LDOL0033 DS    0H                                                               
*                                                                               
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,RCONLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RCONELEM-RCONREC(RE)                                          
*                                                                               
R        USING RCONELEM,RE                                                      
         TM    R.RCONMODR,X'10'                                                 
         JO    LDOL0036            BUY ADDED - KEEP RECORD                      
         DROP  R                                                                
*                                                                               
LDOL0034 CLI   0(RE),0             END OF RECORD - TOSS                         
         JE    LDOL0080                                                         
         CR    RE,R0               PAST EOR - BAIL OUT                          
         JNL   LDOL0080                                                         
         CLI   0(RE),X'03'         EST BUCKET - KEEP RECORD                     
         JE    LDOL0036                                                         
         CLI   0(RE),X'04'         INV BUCKET - KEEP RECORD                     
         JE    LDOL0036                                                         
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     LDOL0034                                                         
*                                                                               
LDOL0036 DS    0H                                                               
         GOTO1 AINITDOL            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LDOL0040 DS    0H                                                               
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF                                                         
         JE    LDOL0080            NO MORE RECORDS LEFT                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   LDOL0080            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    LDOL0060            DO NOT CONVERT RECORD                        
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   LDOL0042                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CON'),VERSION                                              
         J     LDOL0044                                                         
*                                                                               
LDOL0042 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
LDOL0044 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LDOL0060 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LDOL0040                                                         
*                                                                               
LDOL0080 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDOL0020                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CONTRACT DOLLAR RECORD                                       *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDOL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCONREC,R2                                                       
         CLI   0(R2),X'0C'         CONTRACT?                                    
         JNE   YES                 NO                                           
*                                                                               
         XC    IOKEY,IOKEY         BUILD FAKE 8E KEY FOR FILTER                 
K        USING RCONKEY,IOKEY                                                    
         MVI   K.RCON8ETP,X'8E'                                                 
         MVI   K.RCON8EID,X'01'                                                 
         MVC   K.RCON8EST,RCONKSTA                                              
         MVC   K.RCON8ERP,RCONKREP                                              
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(2,K.RCON8EFS)                         
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(2,K.RCON8EFE)                       
         DROP  K                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         GOTO1 AFILTDOL            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         TM    REPPFLG,X'40'       FILTER BACK BILLING?                         
         JZ    UDOL0002            NO                                           
*                                                                               
         CLC   RCONBUYR,=C'ACC-BB'                                              
         JE    YES                                                              
*                                                                               
UDOL0002 DS    0H                                                               
         LR    RE,R2               POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         ICM   R0,3,RCONLEN                                                     
         AR    R0,RE               A(EOR)                                       
         LA    RE,RCONELEM-RCONREC(RE)                                          
*                                                                               
R        USING RCONELEM,RE                                                      
         TM    R.RCONMODR,X'10'                                                 
         JO    UDOL0006            BUY ADDED - KEEP RECORD                      
         DROP  R                                                                
*                                                                               
UDOL0004 CLI   0(RE),0             END OF RECORD - TOSS                         
         JE    YES                                                              
         CR    RE,R0               PAST EOR - BAIL OUT                          
         JNL   YES                                                              
         CLI   0(RE),X'03'         EST BUCKET - KEEP RECORD                     
         JE    UDOL0006                                                         
         CLI   0(RE),X'04'         INV BUCKET - KEEP RECORD                     
         JE    UDOL0006                                                         
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     UDOL0004                                                         
*                                                                               
UDOL0006 DS    0H                                                               
         L     R4,=A(STAXREC)                                                   
*                                                                               
         CLC   4(5,R4),RCONKSTA                                                 
         JE    UDOL0010            STATION DIDN'T CHANGE                        
*                                                                               
K        USING RSTAKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,RCONKREP                                              
         MVC   K.RSTAKSTA,RCONKSTA                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(L'RSTAKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
R        USING RSTAREC,R2                                                       
         MVC   00(2,R4),=X'0248'                                                
         MVC   02(2,R4),REPALPHA    INSERT REP CODE                             
         MVC   04(5,R4),R.RSTAKSTA                                              
         MVC   09(3,R4),R.RSTASTRT                                              
         MVC   12(3,R4),R.RSTAEND                                               
         MVC   15(2,R4),REPALPHA    INSERT REP CODE                             
         MVC   17(20,R4),R.RSTAMKT                                              
         OC    17(20,R4),SPACES  SET X'00' TO SPACES                            
         MVC   37(2,R4),R.RSTAGRUP                                              
         MVC   39(3,R4),R.RSTAAFFL STATION AFFILIATE                            
         MVC   42(2,R4),=C'T '   DEFAULT NY TEAM CODE                           
         MVC   44(2,R4),R.RSTATVB  TVB REGION                                   
         MVC   46(3,R4),R.RSTAOWN  STATION OWNER                                
         MVC   49(1,R4),R.RSTARANK STATION MARKET RANK                          
         MVC   50(2,R4),=C'T '   DEFAULT CH TEAM CODE                           
         XC    52(2,R4),WORK+52  DISP OF LAST STA IN MKT                        
         MVC   68(2,R4),R.RSTACLDT INSERT CLOSE DATE OF STATION                 
         DROP  R                                                                
*                                                                               
UDOL0010 DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         L     R3,DXAXREC                                                       
         USING RECOND,R3                                                        
*                                                                               
         GOTO1 AINITDOL                                                         
*                                                                               
         CLI   RECONACT,C'C'       CHANGE?                                      
         JNE   UDOL0040            NO - NO TABLES TO CLEAR THEN                 
*                                                                               
*                                  BUILD KILL RECORDS                           
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (0,=A(STAXREC)),(R6)                                             
*                                                                               
UDOL0020 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    UDOL0040                                                         
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDOL0020            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECONACT,C'K'       NOW ADD COPY RECORD DETAILS                  
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UDOL0030            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   UDOL0022                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',=C'CON'),VERSION                                           
         J     UDOL0024                                                         
*                                                                               
UDOL0022 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (C'K',TYPENAME),VERSION                                          
UDOL0024 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UDOL0030 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UDOL0020                                                         
*                                                                               
UDOL0040 DS    0H                  BUILD RECORDS                                
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                                                               
UDOL0060 DS    0H                  GET NEXT RECORD                              
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   X.RECONREP,DXU.RXUOVREP                                          
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   *+10                                                             
         MVC   X.RECONMAS,MASALPHA     SET MASTER ON CONTRACT HDR               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDOL0060            DO NOT WRITE RECORDS                         
*                                                                               
         MVI   RECONACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UDOL0080            DO NOT CONVERT RECORDS                       
*                                                                               
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   UDOL0062                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,=C'CON'),VERSION                                              
         J     UDOL0064                                                         
*                                                                               
UDOL0062 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',(RF)),(PLATFORM,DXASQLB),           *        
               (0,TYPENAME),VERSION                                             
UDOL0064 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UDOL0080 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UDOL0060                                                         
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CONTRACT RECORD ACTIVE KEY                                             
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDOL  NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R2                                                       
*                                                                               
         CLC   RCON8ERP,REPALPHA   SAME REP?                                    
         JNE   NO                                                               
         CLI   RCON8ETP,X'8E'      SAME KEY?                                    
         JNE   NO                                                               
         CLI   RCON8EID,X'01'      ONLY CHECK THE 01 SUB KEY                    
         JNE   NO                                                               
         TM    RCONKEY+27,X'80'    DELETED KEY?                                 
         JO    NO                                                               
*                                                                               
         L     RE,=A(DATEBLK)                                                   
         USING DATEBLK,RE                                                       
**TEST                                                                          
**       J     FLTDOL2             CURRENT YEAR ONLY                            
**TEST                                                                          
*                                                                               
         CLC   RCON8EFE,PRIBST     CHECK FOR DATE OVERLAP W/PREVIOUS            
         JL    FLTDOL2                                                          
         CLC   RCON8EFS,PRIBND                                                  
         JH    FLTDOL2                                                          
         J     YES                                                              
*                                                                               
FLTDOL2  DS    0H                                                               
*                                                                               
**TEST                                                                          
**       J     NO                  PREVIOUS YEAR ONLY                           
**TEST                                                                          
*                                                                               
         CLC   RCON8EFE,CURBST     CHECK FOR DATE OVERLAP W/CURRENT             
         JL    NO                                                               
         CLC   RCON8EFS,CURBND                                                  
         JH    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,RE                                                            
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CONTRACT HEADER/DOLLAR RECORD                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDOL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECONDL          R1=L'CON RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         SPACE 2                                                                
***********************************************************************         
* LOAD AGENCY RECORDS                                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RAGYREC,R2                                                       
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'1A'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RAGYKAGY-RAGYKEY),0(R2)                                    
         JNE   YES                   ALL DONE IF TYPE CHANGES                   
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPAGYC,AINITAGY,AFILTAGY                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE AGENCY RECORD DATA                                                     
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RAGYREC,R2                                                       
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
         GOTO1 AINITAGY                                                         
         GOTO1 AACCUPDT,DMCB,VREPAGYC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER AGY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING RAGYREC,R2                                                       
         CLC   RAGYKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RAGYKTYP,X'1A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE AGENCY RECORD                                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REAGYDL          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ADV RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADADV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RADVREC,R2                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,X'08'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LADV02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RADVKADV-RADVKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPADVC,AINITADV,AFILTADV                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LADV02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE ADV RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTADV  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RADVREC,R2                                                       
*                                                                               
         GOTO1 AFILTADV                                                         
         JNE   YES                                                              
         GOTO1 AINITADV                                                         
         GOTO1 AACCUPDT,DMCB,VREPADVC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER ADV RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTADV  NTR1  BASE=*,LABEL=*                                                   
         USING RADVREC,R2                                                       
         CLC   RADVKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RADVKTYP,X'08'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE ADV RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITADV  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,READVDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD SAL RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSALREC,R2                                                       
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSAL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSALKSAL-RSALKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   WORK(L'IOKEY),0(R2)                                              
         MVC   IOKEY,0(R2)                                                      
         XC    IO(255),IO                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'46'         SAL2 FOR EMAIL                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   LSAL20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   LSAL20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   LSAL20                                                           
*                                                                               
LSAL20   DS    0H                                                               
         MVC   IOKEY,WORK          RESTORE KEY                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY,0(R2)                                                      
         JNE   NO                                                               
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPSALC,AINITSAL,AFILTSAL                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSAL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE SAL RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSAL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSALREC,R2                                                       
*                                                                               
         GOTO1 AFILTSAL                                                         
         JNE   YES                                                              
         GOTO1 AINITSAL                                                         
*                                                                               
         XC    IO(255),IO                                                       
*                                                                               
         MVC   IOKEY,0(R2)                                                      
         LA    R2,IOKEY                                                         
         MVI   IOKEY,X'46'         SAL2 FOR EMAIL                               
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 AREADHI                                                          
         JNE   USAL20                                                           
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   USAL20                                                           
*                                                                               
         LA    R2,IO                                                            
         MVC   REPADDR,IOKEY+28    SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   USAL20                                                           
*                                                                               
USAL20   DS    0H                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AACCUPDT,DMCB,VREPSALC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER SAL RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSAL  NTR1  BASE=*,LABEL=*                                                   
         USING RSALREC,R2                                                       
         CLC   RSALKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RSALKTYP,X'06'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE SAL RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESALDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CTY RECORDS (CONTRACT TYPE)                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCTY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTYREC,R2                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCTY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTYKCTY-RCTYKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCTYC,AINITCTY,AFILTCTY                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCTY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CTY RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCTY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCTYREC,R2                                                       
*                                                                               
         GOTO1 AFILTCTY                                                         
         JNE   YES                                                              
         GOTO1 AINITCTY                                                         
         GOTO1 AACCUPDT,DMCB,VREPCTYC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CTY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCTY  NTR1  BASE=*,LABEL=*                                                   
         USING RCTYREC,R2                                                       
         CLC   RCTYKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCTYKTYP,X'32'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CTY RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCTY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECTYDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DCT RECORDS (DEVELOPMENT CONTRACT TYPE)                                  
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDCT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDCTREC,R2                                                       
         XC    RDCTKEY,RDCTKEY                                                  
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDCT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDCTKCTY-RDCTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDCTC,AINITDCT,AFILTDCT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDCT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DCT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDCT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RDCTREC,R2                                                       
*                                                                               
         GOTO1 AFILTDCT                                                         
         JNE   YES                                                              
         GOTO1 AINITDCT                                                         
         GOTO1 AACCUPDT,DMCB,VREPDCTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DCT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDCT  NTR1  BASE=*,LABEL=*                                                   
         USING RDCTREC,R2                                                       
         CLC   RDCTKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RDCTKTYP,X'3B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE DCT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDCT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REDCTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD STA RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSTA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSTAREC,R2                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSTAKSTA-RSTAKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPSTAC,AINITSTA,AFILTSTA                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE STA RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTSTA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RSTAREC,R2                                                       
*                                                                               
         GOTO1 AFILTSTA                                                         
         JNE   YES                                                              
         GOTO1 AINITSTA                                                         
         GOTO1 AACCUPDT,DMCB,VREPSTAC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER STA RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTSTA  NTR1  BASE=*,LABEL=*                                                   
         USING RSTAREC,R2                                                       
         CLC   RSTAKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RSTAKTYP,X'02'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE STA RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RESTADL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GRP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADGRP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RGRPREC,R2                                                       
         XC    RGRPKEY,RGRPKEY                                                  
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGRP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RGRPKGRP-RGRPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPGRPC,AINITGRP,AFILTGRP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGRP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE GRP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTGRP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RGRPREC,R2                                                       
*                                                                               
         GOTO1 AFILTGRP                                                         
         JNE   YES                                                              
         GOTO1 AINITGRP                                                         
         GOTO1 AACCUPDT,DMCB,VREPGRPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER GRP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTGRP  NTR1  BASE=*,LABEL=*                                                   
         USING RGRPREC,R2                                                       
         CLC   RGRPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RGRPKTYP,X'07'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE GRP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITGRP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REGRPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DSP RECORDS (DEVELOPMENT SALESPERSON)                                    
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDSP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDSPREC,R2                                                       
         XC    RDSPKEY,RDSPKEY                                                  
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDSP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDSPKSAL-RDSPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDSPC,AINITDSP,AFILTDSP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDSP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE DSP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTDSP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RDSPREC,R2                                                       
*                                                                               
         GOTO1 AFILTDSP                                                         
         JNE   YES                                                              
         GOTO1 AINITDSP                                                         
         GOTO1 AACCUPDT,DMCB,VREPDSPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER DSP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTDSP  NTR1  BASE=*,LABEL=*                                                   
         USING RDSPREC,R2                                                       
         CLC   RDSPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RDSPKTYP,X'3A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE DSP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITDSP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REDSPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD TEM RECORDS (TEAM)                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADTEM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RTEMREC,R2                                                       
         XC    RTEMKEY,RTEMKEY                                                  
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTEM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RTEMKTEM-RTEMKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPTEMC,AINITTEM,AFILTTEM                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTEM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE TEM RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTTEM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RTEMREC,R2                                                       
*                                                                               
         GOTO1 AFILTTEM                                                         
         JNE   YES                                                              
         GOTO1 AINITTEM                                                         
         GOTO1 AACCUPDT,DMCB,VREPTEMC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER TEM RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTTEM  NTR1  BASE=*,LABEL=*                                                   
         USING RTEMREC,R2                                                       
         CLC   RTEMKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RTEMKTYP,X'05'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE TEM RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITTEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RETEMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRD RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RPRDREC,R2                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RPRDKADV-RPRDKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPPRDC,AINITPRD,AFILTPRD                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRD RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RPRDREC,R2                                                       
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
         GOTO1 AINITPRD                                                         
         GOTO1 AACCUPDT,DMCB,VREPPRDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER PRD RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING RPRDREC,R2                                                       
         CLC   RPRDKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RPRDKTYP,X'09'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE PRD RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REPRDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLS RECORDS (CLASS)                                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCLS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCLSREC,R2                                                       
         XC    RCLSKEY,RCLSKEY                                                  
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLS02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCLSKCLS-RCLSKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCLSC,AINITCLS,AFILTCLS                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLS RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCLS  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCLSREC,R2                                                       
*                                                                               
         GOTO1 AFILTCLS                                                         
         JNE   YES                                                              
         GOTO1 AINITCLS                                                         
         GOTO1 AACCUPDT,DMCB,VREPCLSC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CLS RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCLS  NTR1  BASE=*,LABEL=*                                                   
         USING RCLSREC,R2                                                       
         CLC   RCLSKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCLSKTYP,X'0D'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CLS RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCLS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECLSDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CAT RECORDS (PRODUCT CATEGORY)                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTGREC,R2                                                       
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCAT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTGKCTG-RCTGKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCATC,AINITCAT,AFILTCAT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE CAT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTCAT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RCTGREC,R2                                                       
*                                                                               
         GOTO1 AFILTCAT                                                         
         JNE   YES                                                              
         GOTO1 AINITCAT                                                         
         GOTO1 AACCUPDT,DMCB,VREPCATC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER CAT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTCAT  NTR1  BASE=*,LABEL=*                                                   
         USING RCTGREC,R2                                                       
         CLC   RCTGKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RCTGKTYP,X'0F'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE CAT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITCAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,RECATDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OFF RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROFFREC,R2                                                       
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROFFKOFF-ROFFKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOFFC,AINITOFF,AFILTOFF                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OFF RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROFFREC,R2                                                       
*                                                                               
         GOTO1 AFILTOFF                                                         
         JNE   YES                                                              
         GOTO1 AINITOFF                                                         
         GOTO1 AACCUPDT,DMCB,VREPOFFC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER OFF RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
         USING ROFFREC,R2                                                       
         CLC   ROFFKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   ROFFKTYP,X'04'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE OFF RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REOFFDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OWN RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOWN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROWNREC,R2                                                       
         XC    ROWNKEY,ROWNKEY                                                  
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOWN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROWNKOWN-ROWNKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOWNC,AINITOWN,AFILTOWN                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOWN02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OWN RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTOWN  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ROWNREC,R2                                                       
*                                                                               
         GOTO1 AFILTOWN                                                         
         JNE   YES                                                              
         GOTO1 AINITOWN                                                         
         GOTO1 AACCUPDT,DMCB,VREPOWNC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER OWN RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTOWN  NTR1  BASE=*,LABEL=*                                                   
         USING ROWNREC,R2                                                       
         CLC   ROWNKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   ROWNKTYP,X'2A'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE OWN RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REOWNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MKT RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RMKTREC,R2                                                       
         XC    RMKTKEY,RMKTKEY                                                  
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RMKTKMKT-RMKTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMKTC,AINITMKT,AFILTMKT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE MKT RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTMKT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RMKTREC,R2                                                       
*                                                                               
         GOTO1 AFILTMKT                                                         
         JNE   YES                                                              
         GOTO1 AINITMKT                                                         
         GOTO1 AACCUPDT,DMCB,VREPMKTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER MKT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTMKT  NTR1  BASE=*,LABEL=*                                                   
         USING RMKTREC,R2                                                       
         CLC   RMKTKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RMKTKTYP,X'2B'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE MKT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REMKTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MST RECORDS (SEE-ME/READ-ME MASTER/SUB RECORDS                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMSTC,AINITMST,AFILTMST                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMST02                                                           
         J     YES                                                              
*                                                                               
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE MST RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTMST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RREPREC,R2                                                       
*                                                                               
         GOTO1 AFILTMST                                                         
         JNE   YES                                                              
         GOTO1 AINITMST                                                         
         GOTO1 AACCUPDT,DMCB,VREPMSTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER MST RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTMST  NTR1  BASE=*,LABEL=*                                                   
         USING RREPREC,R2                                                       
         CLC   RREPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RREPKTYP,X'01'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE MKT RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITMST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REMSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD REP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPREPC,AINITREP,AFILTREP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE REP RECORD DATA                                                        
*---------------------------------------------------------------------*         
         SPACE 1                                                                
UPDTREP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING RREPREC,R2                                                       
*                                                                               
         GOTO1 AFILTREP                                                         
         JNE   YES                                                              
         GOTO1 AINITREP                                                         
         GOTO1 AACCUPDT,DMCB,VREPREPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FILTER REP RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
FILTREP  NTR1  BASE=*,LABEL=*                                                   
         USING RREPREC,R2                                                       
         CLC   RREPKREP,REPALPHA    REPALPHA OK?                                
         JNE   NO                                                               
         CLI   RREPKTYP,X'01'                                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* INITIALISE REP RECORD                                                         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
INITREP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,REREPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                  *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),IO                                 
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                DO NOT OVERRIDE REPCODE                      
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    ALOA04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',DXAXREC),(PLATFORM,DXASQLB),        *        
               (0,TYPENAME),VERSION                                             
*                                                                               
         L     RF,DXASQLB                                                       
ALOA04   GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
ALOA06   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'RREPKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,REPDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN UPDATE MODE                *         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
         SPACE 1                                                                
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),IO                                 
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+10                   DO NOT OVERRIDE REPCODE                   
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    UPDL04              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL02                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(L'COPYBUFF)                                                
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6)         BUILD COPY REC            
*                                                                               
         CLC   DXU.RXUOVREP,SPACES                                              
         JE    *+14                   DO NOT OVERRIDE REPCODE                   
*                                                                               
         L     RF,ACOPYBUF            RF=A(COPY RECORD)                         
         MVC   RECONREP-RECOND(2,RF),DXU.RXUOVREP                               
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(RECONREP-RECOND) DISP TO REPALPHA                        
         AR    R0,R1               BUMP TO REPALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         AHI   RF,-((RECONCON-1)-RECOND)                                        
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
UPDL02   DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'U',DXAXREC),(PLATFORM,DXASQLB),        *        
               (R4),VERSION                                                     
*                                                                               
         L     RF,DXASQLB                                                       
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
***********************************************************************         
* SETUP DATES REQUIRED BY THE PROGRAM                                           
***********************************************************************         
SETDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CURRENT YEAR BROADCAST START                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURST),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,CURBST)                              
*                                                                               
* PRIOR YEAR BROADCAST START                                                    
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,PRIBST)                              
*                                                                               
* CURRENT YEAR BROADCAST END                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURND),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,CURBND)                              
*                                                                               
* PRIOR YEAR BROADCAST END                                                      
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,PRIBND)                              
*                                                                               
         L     R2,=V(RXQWK)                                                     
         USING QWKD,R2                                                          
         GOTO1 VDATCON,DMCB,(2,CURBST),(3,QWCURST3)                             
         GOTO1 VDATCON,DMCB,(2,CURBND),(3,QWCURND3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBST),(3,QWPRIST3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBND),(3,QWPRIND3)                             
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(SETUPMON),DMCB,DXUSER                                         
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPFILE INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
REPINIT  NTR1  BASE=*,LABEL=*                                                   
K        USING RREPKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RREPKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING RREPREC,R2                                                       
         MVC   MASALPHA,R.RREPKREP   REP COMPANY                                
         CLC   R.RREPMAST,SPACES      NO MASTER CONTROL                         
         JNH   REPINI10                                                         
         CLC   R.RREPMAST,=X'FFFF'    THIS IS THE MASTER                        
         JE    REPINI10                                                         
*                                                                               
         MVC   MASALPHA,R.RREPMAST   SET THE MASTER                             
*                                                                               
REPINI10 DS    0H                                                               
         MVI   REPPFLG,0                                                        
         CLI   R.RREPPROF+10,C'Y'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'80'                                                    
         CLI   R.RREPPROF+15,C'B'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'40'                                                    
         DROP  R                                                                
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RXRECID                                                                       
***********************************************************************         
         PRINT OFF                                                              
RXRECIDT DS    0C                                                               
       ++INCLUDE RXRECID                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* BROADCAST DATE BLOCK                                                          
***********************************************************************         
DATEBLK  DS    0C                                                               
CURBST   DS    XL2                 COMPRESSED CURRENT BROADCAST START           
CURBND   DS    XL2                 COMPRESSED CURRENT BROADCAST END             
PRIBST   DS    XL2                 COMPRESSED PRIOR BROADCAST START             
PRIBND   DS    XL2                 COMPRESSED PRIOR BROADCAST END               
         SPACE 3                                                                
***********************************************************************         
* FAKE SPACEND TABLE FOR SETVAL IN RXROUTS                                      
***********************************************************************         
STAXREC DC     XL72'0000'                                                       
         SPACE 3                                                                
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
COPYBUFF DS    CL10000                                                          
         EJECT                                                                  
***********************************************************************         
* TF SSB                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      CSECT                                                                  
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
         EJECT                                                                  
***********************************************************************         
* COPY OF COMFACS FOR THOSE DAMN DEMOS                                *         
***********************************************************************         
         SPACE 1                                                                
COMFACS  CSECT                     COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                CALLOFF)                                     
         DC    A(0)                GETMSG)                                      
         DC    A(0)                GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    A(0)                HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    A(0)                HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    A(0)                GETPROF)                                     
         DC    V(PERVERT)                                                       
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    V(GENERAL)                                                       
         DC    18A(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    A(0)                DDISPSRT)                                    
VDEMADDR DC    A(0)                DEMADDR                                      
         DC    A(0)                DEMDISP)                                     
         DC    A(0)                DBOOK)                                       
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
         DC    A(0)                DEMOUT)                                      
         DC    A(0)                DEMEL)                                       
         DC    A(0)                DEMAINT)                                     
         DC    A(0)                DEMAND)                                      
         DC    A(0)                DEMOMATH)                                    
         DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    V(PERVAL)                                                        
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
*&&                                                                             
         DC    A(0)                GLOBBER)                                     
         DC    A(0)                MINIO)                                       
         DC    A(0)                PARSNIP)                                     
         DC    A(0)                DICTATE)                                     
         DC    A(0)                EDITOR)                                      
         DC    A(0)                GETHELP)                                     
         DC    A(0)                CUREDIT)                                     
         DC    A(0)                GETRET)                                      
         DC    A(0)                REPORT)                                      
         DC    A(0)                BLDCUR)                                      
         DC    A(0)                GETCUR)                                      
         DC    A(0)                GETNARR)                                     
         DC    A(0)                DEJAVU)                                      
         DC    A(0)                SECRET)                                      
         DC    A(0)                BILLIT)                                      
         DC    A(0)                                                             
         DC    A(0)                PQPROF)                                      
         DC    2A(0)                                                            
         DC    A(0)                BINSRCH)                                     
         DC    A(0)                PROTON)                                      
         DC    A(0)                PROTOFF)                                     
         DC    A(0)                HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    8A(0)               SPARE                                        
         EJECT                                                                  
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
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
REPADDR  DS    CL4                                                              
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
REPALPHA DS    XL2                                                              
MASALPHA DS    XL2                                                              
VERSION  DS    XL1                                                              
PLATFORM DS    XL1                                                              
*                                                                               
SALEMAIL DS    CL60                                                             
*                                                                               
REPPFLG DS     XL1                 REP PROFILE FLAGS                            
*                                   X'80' #11 ZERO BUCKETS = Y                  
*                                   X'40' #16 SURPRESS BACK BILLING = Y         
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
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    4096X                                                            
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
TYPFLTAB EQU   X'01'               LOAD/UPDATE ARE TABLE ADDRESSES              
TYPVER   DS    XL1                 VERSION                                      
         DS    XL2                 N/D                                          
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
VREPCNVX DS    V                                                                
***PDACC DS    V                                                                
***PDTRC DS    V                                                                
VREPCONC DS    V          CONTRACT HEADER                                       
VREPDOLC DS    V          CONTRACT DOLLARS                                      
VREPAGYC DS    V          AGENCY                                                
VREPADVC DS    V          ADVERTISER                                            
VREPSALC DS    V          SALESPERSON                                           
VREPCTYC DS    V          CONTRACT TYPE                                         
VREPDCTC DS    V          DEV CONTRACT TYPE                                     
VREPSTAC DS    V          STATION                                               
VREPDSPC DS    V          DEV SALESPERSON                                       
VREPTEMC DS    V          TEAM                                                  
VREPPRDC DS    V          PRODUCT CODE                                          
VREPCLSC DS    V          CLASS                                                 
VREPCATC DS    V          CATEGORY                                              
VREPOFFC DS    V          OFFICE                                                
VREPOWNC DS    V          OWNER                                                 
VREPMKTC DS    V          MARKET                                                
VREPGRPC DS    V          GROUP/SUBGROUP                                        
VREPMSTC DS    V          SEE ME/READ ME SUB/MASTER RECORD                      
VREPREPC DS    V          REP INFO                                              
         SPACE 1                                                                
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
         DS    A                                                                
         DS    A                   SPARE                                        
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                                                                
ARECCMP  DS    A                                                                
         SPACE 1                                                                
         DS    CL8                 LOAD ROUTINES                                
ALOADCON DS    A          CONTRACT HEADER                                       
ALOADDOL DS    A          CONTRACT DOLLARS                                      
ALOADAGY DS    A          AGENCY                                                
ALOADADV DS    A          ADVERTISER                                            
ALOADSAL DS    A          SALESPERSON                                           
ALOADCTY DS    A          CONTRACT TYPE                                         
ALOADDCT DS    A          DEV CONTRACT TYPE                                     
ALOADSTA DS    A          STATION                                               
ALOADDSP DS    A          DEV SALESPERSON                                       
ALOADTEM DS    A          TEAM                                                  
ALOADPRD DS    A          PRODUCT CODE                                          
ALOADCLS DS    A          CLASS                                                 
ALOADCAT DS    A          CATEGORY                                              
ALOADOFF DS    A          OFFICE                                                
ALOADOWN DS    A          OWNER                                                 
ALOADMKT DS    A          MARKET                                                
ALOADGRP DS    A          GROUP/SUBGROUP                                        
ALOADMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
ALOADREP DS    A          REP INFO                                              
         SPACE 1                                                                
         DS    CL8                 UPDATE ROUTINES                              
AUPDTCON DS    A          CONTRACT HEADER                                       
AUPDTDOL DS    A          CONTRACT DOLLARS                                      
AUPDTAGY DS    A          AGENCY                                                
AUPDTADV DS    A          ADVERTISER                                            
AUPDTSAL DS    A          SALESPERSON                                           
AUPDTCTY DS    A          CONTRACT TYPE                                         
AUPDTDCT DS    A          DEV CONTRACT TYPE                                     
AUPDTSTA DS    A          STATION                                               
AUPDTDSP DS    A          DEV SALESPERSON                                       
AUPDTTEM DS    A          TEAM                                                  
AUPDTPRD DS    A          PRODUCT CODE                                          
AUPDTCLS DS    A          CLASS                                                 
AUPDTCAT DS    A          CATEGORY                                              
AUPDTOFF DS    A          OFFICE                                                
AUPDTOWN DS    A          OWNER                                                 
AUPDTMKT DS    A          MARKET                                                
AUPDTGRP DS    A          GROUP/SUBGROUP                                        
AUPDTMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AUPDTREP DS    A          REP INFO                                              
         SPACE 1                                                                
         DS    CL8                 FILTER ROUTINES                              
AFILTCON DS    A          CONTRACT HEADER                                       
AFILTDOL DS    A          CONTRACT DOLLARS                                      
AFILTAGY DS    A          AGENCY                                                
AFILTADV DS    A          ADVERTISER                                            
AFILTSAL DS    A          SALESPERSON                                           
AFILTCTY DS    A          CONTRACT TYPE                                         
AFILTDCT DS    A          DEV CONTRACT TYPE                                     
AFILTSTA DS    A          STATION                                               
AFILTDSP DS    A          DEV SALESPERSON                                       
AFILTTEM DS    A          TEAM                                                  
AFILTPRD DS    A          PRODUCT CODE                                          
AFILTCLS DS    A          CLASS                                                 
AFILTCAT DS    A          CATEGORY                                              
AFILTOFF DS    A          OFFICE                                                
AFILTOWN DS    A          OWNER                                                 
AFILTMKT DS    A          MARKET                                                
AFILTGRP DS    A          GROUP/SUBGROUP                                        
AFILTMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AFILTREP DS    A          REP INFO                                              
         SPACE 1                                                                
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITCON DS    A          CONTRACT HEADER                                       
AINITDOL DS    A          CONTRACT DOLLARS                                      
AINITAGY DS    A          AGENCY                                                
AINITADV DS    A          ADVERTISER                                            
AINITSAL DS    A          SALESPERSON                                           
AINITCTY DS    A          CONTRACT TYPE                                         
AINITDCT DS    A          DEV CONTRACT TYPE                                     
AINITSTA DS    A          STATION                                               
AINITDSP DS    A          DEV SALESPERSON                                       
AINITTEM DS    A          TEAM                                                  
AINITPRD DS    A          PRODUCT CODE                                          
AINITCLS DS    A          CLASS                                                 
AINITCAT DS    A          CATEGORY                                              
AINITOFF DS    A          OFFICE                                                
AINITOWN DS    A          OWNER                                                 
AINITMKT DS    A          MARKET                                                
AINITGRP DS    A          GROUP/SUBGROUP                                        
AINITMST DS    A          SEE ME/READ ME SUB/MASTER RECORD                      
AINITREP DS    A          REP INFO                                              
         SPACE 1                                                                
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
REPDIR   DS    CL7                                                              
REPFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
ACTIVITY DS    CL1                                                              
SPACES   DS    CL80                                                             
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
* RXUSERD                                                                       
       ++INCLUDE RXUSERD                                                        
* RXRECD                                                                        
       ++INCLUDE RXRECD                                                         
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* REGENALL1A                                                                    
* REGENDSP                                                                      
* REREPQWKDD                                                                    
         PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REREPQWKDD                                                     
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068RXTRACT   05/26/05'                                      
         END                                                                    
