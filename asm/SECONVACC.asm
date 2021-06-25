*          DATA SET SECONVACC  AT LEVEL 121 AS OF 05/01/02                      
*PHASE SECACC                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SECACF - ACCESS RECORD CONVERT RECORD ACTION'                   
SECACC   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(23),=C'CONTROL FILE CONVERSION'                            
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING SAASKEY,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SAASKEY,SAASKEY                      
         B     CONV15A                                                          
*                                                                               
CONV14   MVC   SAASKEY(L'SAASKEY),IOKEY                                         
         CLI   SQFLAG,0                                                         
         BE    CONV15                                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SAASKEY,SAASKEY                      
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
CONV15   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SAASKEY,SAASKEY                      
*                                                                               
CONV15A  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SAASKEY),SAASKEY                                         
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(1),IO                                                          
*                                                                               
CONV16   CLI   SAASTYP,SAASTYPQ                                                 
         BNE   CONV200                                                          
         CLI   SAASSUB,SAASSUBQ                                                 
         BNE   CONV200                                                          
         CLI   SAASOVS,X'06'       PROCESS ACCOUNT ACCESS RECORDS               
         BNE   CONV200                                                          
         CLI   SAASPGM,X'1D'       CONVERT AGENCY LEVEL COST RECORDS            
         BNE   CONV200                                                          
         OC    SAASUID,SAASUID                                                  
         BNZ   CONV200                                                          
         OC    SAASAGN,SAASAGN                                                  
         BNZ   CONV200                                                          
*                                                                               
CONV17   MVC   AGYALPH,SAASAGY                                                  
         BAS   RE,CHKAGY                                                        
* ??     BNE   CONV200                                                          
* ??     CLI   COUNTRY,0                                                        
* ??     BNE   CONV200                                                          
         B     CCOST                                                            
*                                                                               
CCOST    LA    R3,SAASDATA                                                      
         USING SAMIXEL,R3                                                       
*                                                                               
CENQ030  XC    WORK,WORK                                                        
         MVI   WORK,SAMIXELQ                                                    
         MVI   DUB,X'09'                                                        
         GOTO1 VHELLO,PARM,(C'G',CTFILE),(WORK,(R2)),(1,DUB),0                  
         BNE   CONV200                                                          
*                                                                               
         L     R3,12(R1)                                                        
         CLI   SAMIXLN,X'06'                                                    
         BL    CONV200                                                          
         TM    SAMIXEL+5,X'08'                                                  
         BZ    CONV200                                                          
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS ACC/COST/HISTORY/DELETE RESET'              
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
         USING SAASKEY,R2                                                       
CONV200  SR    RE,RE                                                            
         ICM   RE,3,SAASLEN                                                     
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* ADJUST ACCESS RECORD SYSTEM PROGRAM SECURITY                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5REC,R2                                                        
ADSECURE NTR1                                                                   
         MVC   P(2),CT5KALPH                                                    
         MVC   P+4(16),=C'SECURITY PRESENT'                                     
         LA    R3,CT5DATA                                                       
         USING CTSYSD,R3                                                        
ASEC010  CLI   CTSYSEL,0                                                        
         BE    ASECX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   ASEC020                                                          
         CLI   CTSYSNUM,X'06'                                                   
         BNE   ASEC020                                                          
         CLI   CTSYSLEN,X'10'                                                   
         BE    ASEC030                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BE    ASEC040                                                          
ASEC020  SR    R0,R0                                                            
         IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     ASEC010                                                          
*                                                                               
ASEC030  XC    WORK,WORK                                                        
         ZIC   RF,CTSYSLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTSYSEL                                                  
         MVC   DUB(1),CTSYSNUM                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         MVI   CTSYSLEN,X'18'                                                   
         MVC   CTSYSPGM(8),=XL8'01100020A0000000'                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+4(16),=C'SECURITY ADDED  '                                     
         B     ASEC020                                                          
*                                                                               
ASEC040  EQU   *                                                                
         OC    CTSYSPGM(8),=XL8'0100000000000000'                               
         MVC   P+4(16),=C'SECURITY CHANGED'                                     
         B     ASEC020                                                          
*                                                                               
ASECX    GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK AGENCY ALPHA COUNTRY                                          *         
***********************************************************************         
CHKAGY   NTR1  ,                                                                
         MVI   COUNTRY,0                                                        
         LA    RF,AGYTAB                                                        
CAGY002  CLI   0(RF),0                                                          
         BE    CAGY010                                                          
         CLC   0(2,RF),AGYALPH                                                  
         BE    CAGYNO                                                           
         LA    RF,2(RF)                                                         
         B     CAGY002                                                          
*                                                                               
CAGY010  LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH(2),AGYALPH                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5KEY,CT5KEY                        
         BNE   CAGYOK                                                           
* ??     DC    H'00'                                                            
         LA    R1,CT5DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
CAGY020  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R1),CTAGDELQ                                                   
         BE    CAGY030                                                          
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     CAGY020                                                          
*                                                                               
         USING CTAGDEL,R1                                                       
CAGY030  MVC   COUNTRY,CTAGDCTY                                                 
         B     CAGYOK                                                           
*                                                                               
CAGYNO   B     NO                                                               
*                                                                               
CAGYOK   B     YES                                                              
         DROP  R4                                                               
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
SSB      DC    X'0000',X'FF',X'02' FOR DATAMGR (OFFLINE NO RECOVERY)            
         DC    250X'00'                                                         
         DS    0D                                                               
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
ENGFCW   DC    XL23'0000000000000000000000077F8AA7E4073BF03C000420'             
ENGFCR   DC    XL23'0000000000000000000000077F8AA7E4073BF03C000420'             
GERFCW   DC    XL23'0000000000000000000000077F8AA7E4073FF03C0007E0'             
GERFCR   DC    XL23'0000000000000000000000077F8AA7E4073FF03C0007E0'             
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
* AGENCY DEFAULT EXCEPTIONS                                                     
*                                                                               
AGYTAB   DC    CL2'DM'                                                          
         DC    CL2'DQ'                                                          
         DC    CL2'DJ'                                                          
         DC    CL2'MQ'                                                          
         DC    CL2'YL'                                                          
         DC    CL2'YI'                                                          
         DC    CL2'C5'                                                          
         DC    CL2'HX'                                                          
         DC    CL2'UO'                                                          
         DC    CL2'TF'                                                          
AGYTABX  DC    X'00'                                                            
*                                                                               
BATTAB   DS    0D                                                               
BATTABX  DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
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
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
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
SQFLAG   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
PROGNAM  DS    CL7                                                              
AGYALPH  DS    CL2                                                              
COUNTRY  DS    CL1                                                              
PACCVAL  DS    XL2                                                              
PACCADR  DS    A                                                                
PACCSAV  DS    XL2                                                              
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121SECONVACC 05/01/02'                                      
         END                                                                    
