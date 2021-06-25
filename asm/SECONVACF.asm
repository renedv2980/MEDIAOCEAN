*          DATA SET SECONVACF  AT LEVEL 117 AS OF 05/01/02                      
*PHASE SECACF                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SECACF - ACCESS RECORD CONVERT FOR NEW FIELDS'                  
SECACF   CSECT                                                                  
         PRINT NOGEN                                                            
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
         CLI   SAASPGM,X'1B'       CONVERT ALL LEVEL BATCH RECORDS              
         BNE   CONV200                                                          
*                                                                               
CONV17   MVC   AGYALPH,SAASAGY                                                  
         BAS   RE,CHKAGY                                                        
* ??     BNE   CONV200                                                          
* ??     CLI   COUNTRY,0                                                        
* ??     BNE   CONV200                                                          
         B     CBAT                                                             
*                                                                               
CBAT     LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CBAT010  CLI   0(R3),0                                                          
         BE    CBAT040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CBAT020                                                          
         CLI   SAMIXATT,0                                                       
         BNE   CBAT020                                                          
         CLI   SAMIXRCD,X'01'                                                   
         BE    CBAT030                                                          
CBAT020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CBAT010                                                          
*                                                                               
CBAT030  XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         MVI   SAMIXLN,X'09'                                                    
         OI    SAMIXEL+8,X'40'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(27),=C'ACCESS BAT ELEMENT CHANGED'                           
         GOTO1 VPRINTER                                                         
         B     CBAT050                                                          
*                                                                               
CBAT040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(9),=XL9'BA0901000000000040'                              
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(23),=C'ACCESS BT ELEMENT ADDED'                              
         GOTO1 VPRINTER                                                         
*                                                                               
CBAT050  EQU   *                                                                
         LA    R4,BATTAB                                                        
CBAT052  CLI   0(R4),0                                                          
         BE    CONV200                                                          
         B     CBAT060                                                          
CBAT054  LA    R4,1(R4)                                                         
         B     CBAT052                                                          
*                                                                               
CBAT060  XC    WORK,WORK                                                        
         MVI   WORK,SAMIXELQ                                                    
         MVC   DUB(1),0(R4)                                                     
         GOTO1 VHELLO,PARM,(C'G',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),X'06'                                                     
         BE    CBAT064                                                          
         CLI   12(R1),0                                                         
         BE    CBAT070                                                          
         DC    H'0'                                                             
CBAT064  LA    R3,WORK                                                          
         MVC   SAMIXEL(9),=XL9'BA0900010000000002'                              
         MVC   SAMIXRCD(1),0(R4)                                                
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(23),=C'ACCESS BT ELEMENT ADDED'                              
         GOTO1 VPRINTER                                                         
         B     CBAT054                                                          
*                                                                               
CBAT070  L     R1,12(R1)                                                        
         XC    WORK,WORK                                                        
         LR    R3,R1                                                            
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),0(R4)                                                     
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,WORK                                                          
         MVI   SAMIXLN,X'09'                                                    
         OI    SAMIXEL+8,X'02'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(26),=C'ACCESS BT ELEMENT CHANGED'                            
         GOTO1 VPRINTER                                                         
         B     CBAT054                                                          
         DROP  R3                                                               
*                                                                               
CENQ     LA    R3,SAASDATA                                                      
         USING SAMIXEL,R3                                                       
*                                                                               
CENQ030  XC    WORK,WORK                                                        
         MVI   WORK,SAMIXELQ                                                    
         MVI   DUB,X'03'                                                        
         GOTO1 VHELLO,PARM,(C'G',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),X'06'                                                     
         BE    CENQ032                                                          
         CLI   12(R1),0                                                         
         BE    CENQ040                                                          
         DC    H'0'                                                             
CENQ032  LA    R3,WORK                                                          
         MVC   SAMIXEL(6),=XL6'BA0603000080'                                    
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(24),=C'ACCESS ENQ ELEMENT ADDED'                             
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CENQ040  LA    R3,WORK                                                          
         L     R1,12(R1)                                                        
         MVC   WORK,0(R1)                                                       
         MVI   DUB,X'03'                                                        
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,WORK                                                          
         MVI   SAMIXLN,X'06'                                                    
         OI    SAMIXEL+5,X'80'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(26),=C'ACCESS ENQ ELEMENT CHANGED'                           
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CPROD    LA    R3,SAASDATA                                                      
         USING SAMIXEL,R3                                                       
*                                                                               
CPRO030  XC    WORK,WORK                                                        
         MVI   WORK,SAMIXELQ                                                    
         MVI   DUB,X'1F'                                                        
         GOTO1 VHELLO,PARM,(C'G',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),X'06'                                                     
         BE    CPRO032                                                          
         CLI   12(R1),0                                                         
         BE    CPRO040                                                          
         DC    H'0'                                                             
CPRO032  LA    R3,WORK                                                          
         MVC   SAMIXEL(8),=XL8'BA081F0000F44040'                                
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(24),=C'ACCESS PRO ELEMENT ADDED'                             
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CPRO040  LA    R3,WORK                                                          
         B     CPRO044        LEAVE IT ALONE                                    
         L     R1,12(R1)                                                        
         MVC   WORK,0(R1)                                                       
         MVI   DUB,X'1F'                                                        
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,WORK                                                          
         MVI   SAMIXLN,X'08'                                                    
         OI    SAMIXEL+5,X'80'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
CPRO044  MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(26),=C'ACCESS ENQ ELEMENT CHANGED'                           
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
         USING SAFCKEY,R2                                                       
CFCON    LA    R3,SAFCDATA                                                      
         SR    R0,R0                                                            
CFCW010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),SAFCWELQ                                                   
         BE    CFCW030                                                          
CFCW020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFCW010                                                          
*                                                                               
         USING SAFCWEL,R3                                                       
CFCW030  XC    WORK,WORK                                                        
         ZIC   RF,SAFCWLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAFCWEL                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),0,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAFCWLN,X'1C'                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         OI    SAFCWEL+4,X'04'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(18),=C'FCONTROL W CHANGED'                                   
         GOTO1 VPRINTER                                                         
         DROP  R3                                                               
*                                                                               
         LA    R3,SAFCDATA                                                      
         SR    R0,R0                                                            
CFCR010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),SAFCRELQ                                                   
         BE    CFCR030                                                          
CFCR020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFCR010                                                          
*                                                                               
         USING SAFCREL,R3                                                       
CFCR030  XC    WORK,WORK                                                        
         ZIC   RF,SAFCRLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAFCREL                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),0,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAFCRLN,X'1C'                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         OI    SAFCREL+4,X'04'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(18),=C'FCONTROL R CHANGED'                                   
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
         USING CT5REC,R2                                                        
CONV100  CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   CONV200                                                          
         MVC   AGYALPH,CT5KALPH                                                 
         BAS   RE,CHKAGY                                                        
         BNE   CONV200                                                          
         CLI   COUNTRY,0                                                        
         BNE   CONV200                                                          
         BAS   RE,ADSECURE                                                      
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(21),=C'AGENCY RECORD CHANGED'                                
         B     CONV200                                                          
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
         EJECT                                                                  
         LTORG                                                                  
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
**PAN#1  DC    CL21'117SECONVACF 05/01/02'                                      
         END                                                                    
