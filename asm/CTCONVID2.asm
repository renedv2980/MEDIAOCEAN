*          DATA SET CTCONVID2  AT LEVEL 164 AS OF 05/01/02                      
*PHASE CONVID2                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
         TITLE 'CONVID2 - ID RECORD CLEAR EXTERNAL'                             
CONVID2  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(26),=C'ID RECORD CLEARANCE REPORT'                         
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READAGY          READ DDIN & BUILD AGENCY TABLE               
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READCTF          READ CONTROL FILE RECORDS                    
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
         MVI   RETCODE,X'FF'                                                    
         MVI   ERROR,0                                                          
         MVI   COUNTERR,0                                                       
         MVI   CONTROLF,0                                                       
         L     RF,=A(AGYTAB)                                                    
         ST    RF,AAGYTAB                                                       
         ST    RF,AAGYTABL                                                      
         L     RF,=A(AGYTABX)                                                   
         ST    RF,AAGYTABX                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VCLIST                                                           
         B     VCUPDATE                                                         
*                                  CARD DATA ERROR CONDITIONS                   
VCEND    B     VCYES                                                            
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 VPRINTER            INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCYES    B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
         SPACE 1                                                                
VCLIST   EQU   *                   LIST=                                        
         CLC   P+5(3),=C'ALL'                                                   
         BNE   VCLP1                                                            
         OI    CONTROLF,LISTALL                                                 
         B     VCLP1                                                            
*                                                                               
VCUPDATE EQU   *                   UPDATE=                                      
         CLC   P+7(3),=C'YES'                                                   
         BNE   VCLP1                                                            
         OI    CONTROLF,UPDATEY                                                 
         B     VCLP1                                                            
         EJECT                                                                  
***********************************************************************         
* READ AGENCY LIST FROM DDIN AND BUILD TABLE                          *         
***********************************************************************         
READAGY  NTR1  ,                                                                
         L     R2,AAGYTAB                                                       
         OPEN  (DDIN,INPUT)                                                     
*                                                                               
RAGY010  GET   DDIN,LINEBUFF                                                    
         B     RAGY020                                                          
EODADDI  CLOSE (DDIN)                                                           
         B     RAGYOK                                                           
*                                                                               
RAGY020  MVC   0(2,R2),LINEBUFF+2  SAVE AGENCY ALPHA                            
         MVC   2(1,R2),LINEBUFF    SAVE +/- UPDATE CODE                         
         LA    R2,3(R2)                                                         
         C     R2,AAGYTABX                                                      
         BL    *+6                                                              
         DC    H'00'                                                            
         B     RAGY010                                                          
*                                                                               
RAGYOK   ST    R2,AAGYTABL                                                      
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE RECORDS AND BUILD LISTS                           *         
***********************************************************************         
READCTF  NTR1  ,                                                                
         MVC   P(19),=C'USER-ID/AGENCY LIST'                                    
         GOTO1 VPRINTER                                                         
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         GOTO1 ,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                                
*                                                                               
RCTF010  GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF2000                                                         
         DC    H'0'                                                             
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BE    RCTF100                                                          
*                                                                               
         CLI   CTIKTYP,CT0KTEQU                                                 
         BE    RCTF400                                                          
*                                                                               
         CLI   CTIKTYP,CT5KTYPQ                                                 
         BNE   RCTF1000                                                         
         B     RCTF300                                                          
*                                                                               
RCTF100  OC    CTIKID,CTIKID                                                    
         BZ    RCTF1000                                                         
         OC    CTIKID(8),CTIKID                                                 
         BZ    RCTF200                                                          
         MVC   P(10),CTIKID                                                     
         LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTAGYD,R3                                                        
RCTF110  CLI   CTAGYEL,0                                                        
         BNE   RCTF112                                                          
         MVC   P(23),=C'MISSING AGENCY ELEMENT:'                                
         GOTO1 VHEXOUT,PARM,(R2),P+25,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
RCTF112  CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   RCTF120                                                          
         B     RCTF130                                                          
RCTF120  IC    R0,CTAGYLEN                                                      
         AR    R3,R0                                                            
         B     RCTF110                                                          
*                                  GET CTAGYD DATA                              
RCTF130  EQU   *                                                                
         MVC   P+12(2),CTAGYID                                                  
         MVC   AGYALPH,CTAGYID                                                  
         BAS   RE,VALAGY                                                        
         BNE   RCTF150                                                          
         CLI   AGYIND,C'+'                                                      
         BNE   RCTF140                                                          
         TM    CONTROLF,LISTALL                                                 
         BNO   RCTF1000                                                         
         GOTO1 VPRINTER                                                         
         B     RCTF1000                                                         
RCTF140  MVC   P+16(20),=C'AGENCY TO BE REMOVED'                                
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
RCTF150  MVC   P+16(16),=C'AGENCY NOT FOUND'                                    
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
         DROP  R3                                                               
*                                                                               
RCTF200  LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTAGYD,R3                                                        
RCTF210  CLI   CTAGYEL,0                                                        
         BNE   RCTF212                                                          
         MVC   P(23),=C'MISSING AGENCY ELEMENT:'                                
         GOTO1 VHEXOUT,PARM,(R2),P+25,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
RCTF212  CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   RCTF220                                                          
         B     RCTF230                                                          
RCTF220  IC    R0,CTAGYLEN                                                      
         AR    R3,R0                                                            
         B     RCTF210                                                          
*                                  GET CTAGYD DATA                              
RCTF230  EQU   *                                                                
         MVC   AGYALPH,CTAGYID                                                  
         BAS   RE,VALAGY                                                        
         BNE   RCTF010                                                          
         CLI   AGYIND,C'+'                                                      
         BNE   RCTF010                                                          
         B     RCTF1000                                                         
         DROP  R3                                                               
*                                                                               
RCTF300  MVC   AGYALPH,CT5KALPH-CT5KEY(R2)                                      
         BAS   RE,VALAGY                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   AGYIND,C'+'                                                      
         BE    RCTF1000                                                         
         MVC   P(2),AGYALPH                                                     
         MVC   P+2(16),=C'  AGENCY REMOVED'                                     
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
         USING CT0KEY,R2                                                        
RCTF400  MVI   PASSFLAG,0                                                       
         OC    CT0KEYS(20),CT0KEYS                                              
         BZ    RCTF410                                                          
         OC    CT0KEYS(12),CT0KEYS                                              
         BNZ   RCTF410                                                          
         MVI   PASSFLAG,1                                                       
         MVC   P(10),CT0KCODE                                                   
         MVC   P+12(2),CT0KAGY                                                  
RCTF410  MVC   AGYALPH,CT0KAGY                                                  
         BAS   RE,VALAGY                                                        
         BNE   RCTF450                                                          
         CLI   AGYIND,C'+'                                                      
         BNE   RCTF440                                                          
         TM    CONTROLF,LISTALL                                                 
         BNO   RCTF1000                                                         
         CLI   PASSFLAG,0                                                       
         BE    RCTF1000                                                         
         GOTO1 VPRINTER                                                         
         B     RCTF1000                                                         
RCTF440  CLI   PASSFLAG,0                                                       
         BE    RCTF010                                                          
         MVC   P+16(20),=C'AGENCY TO BE REMOVED'                                
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
RCTF450  CLI   PASSFLAG,0                                                       
         BE    RCTF010                                                          
         MVC   P+16(16),=C'AGENCY NOT FOUND'                                    
         GOTO1 VPRINTER                                                         
         B     RCTF010                                                          
*                                                                               
         USING CTIKEY,R2                                                        
RCTF1000 SR    RE,RE                                                            
         ICM   RE,3,CTILEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         TM    CONTROLF,UPDATEY                                                 
         BNO   RCTF010                                                          
         PUT   TAPEOUT,IOL                                                      
         B     RCTF010                                                          
*                                                                               
RCTF2000 MVC   P(26),=C'END OF USER-ID/AGENCY LIST'                             
         GOTO1 VPRINTER                                                         
         CLOSE (TAPEOUT)                                                        
*                                                                               
RCTFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHAEK ID AGENCY ALPHA IN VALID AGENCY LIST                          *         
***********************************************************************         
VALAGY   NTR1  ,                                                                
         L     RF,AAGYTAB                                                       
VAGY010  CLC   0(2,RF),AGYALPH                                                  
         BE    VAGY020                                                          
         LA    RF,3(RF)                                                         
         C     RF,AAGYTABL                                                      
         BNL   VAGYNO                                                           
         B     VAGY010                                                          
*                                                                               
VAGY020  MVC   AGYIND,2(RF)                                                     
         B     VAGYOK                                                           
*                                                                               
VAGYNO   B     NO                                                               
*                                                                               
VAGYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1                                                                   
         LA    RE,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRMSG0),0(RE)                                            
         GOTO1 VPRINTER                                                         
         MVI   ERROR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(05,01),X'00',CL11'LIST='                                     
         DC    AL1(07,02),X'00',CL11'UPDATE='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'INVALID INPUT FILE LINE'                                    
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
DDIN     DCB   DDNAME=DDIN,DSORG=PS,RECFM=FB,MACRF=GM,                 *        
               BLKSIZE=3200,LRECL=80,EODAD=EODADDI                              
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
ASYSLST  DS    A                                                                
RETCODE  DS    XL1                                                              
PASSFLAG DS    XL1                                                              
CONTROLF DS    XL1                                                              
LISTALL  EQU   X'01'                                                            
UPDATEY  EQU   X'02'                                                            
AGYIND   DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
COUNTERR DS    XL1                 COUNTER OF COMPILATION ERRORS                
AGYCNT   DS    F                                                                
AAGYTAB  DS    A                                                                
AAGYTABL DS    A                                                                
AAGYTABX DS    A                                                                
AGYALPH  DS    CL2                 AGENCY ALPHA CODE                            
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CTIKEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
AGYTAB   CSECT                     ** WORKING STORAGE POOL **                   
         DS    (10000)XL3                                                       
AGYTABX  EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'164CTCONVID2 05/01/02'                                      
         END                                                                    
