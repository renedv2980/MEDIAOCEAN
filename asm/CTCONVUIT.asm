*          DATA SET CTCONVUIT  AT LEVEL 115 AS OF 03/02/99                      
*PHASE CONVUIT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'CONVUIT - USERID TEST REPORT'                                   
CONVUIT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(29),=CL29'USER-ID TEST REPORT'                             
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
         L     RF,=A(UIDTAB)                                                    
         STCM  RF,15,AUIDTAB                                                    
         STCM  RF,15,AUIDPTR                                                    
         L     RF,=A(UIDTABX)                                                   
         STCM  RF,15,AUIDTABX                                                   
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                        
         B     CONV15A                                                          
*                                                                               
CONV14   MVC   CTIKEY(L'CTIKEY),IOKEY                                           
         CLI   SQFLAG,0                                                         
         BE    CONV15                                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
CONV15   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY                        
*                                                                               
CONV15A  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
*                                                                               
         MVC   P,SPACES                                                         
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   CONV200                                                          
         OC    CTIKID(10),CTIKID                                                
         BZ    CONV200                                                          
         OC    CTIKID(8),CTIKID                                                 
         BZ    CONV100                                                          
         LA    RF,L'CTIKID                                                      
         LA    RE,CTIKID                                                        
         MVI   BYTE,C'N'                                                        
CONV010  CLI   0(RE),C'*'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'Y'                                                        
         B     CONV020                                                          
         CLI   0(RE),X'00'                                                      
         BE    CONV030                                                          
         CLI   0(RE),C' '                                                       
         BE    CONV030                                                          
CONV020  LA    RE,1(RE)                                                         
         BCT   RF,CONV010                                                       
CONV030  CLI   BYTE,C'N'                                                        
         BE    CONV040                                                          
         MVC   P(10),CTIKID                                                     
         BCTR  RE,0                                                             
         MVC   P+12(40),=CL40'USER ID CONTAINS *'                               
         CLI   0(RE),C'*'                                                       
         BNE   *+10                                                             
         MVC   P+12(40),=CL40'USER ID ENDS WITH *'                              
         GOTO1 VPRINTER                                                         
CONV040  EQU   *                                                                
         LA    R3,CTIDATA                                                       
CONV050  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    CONV060                                                          
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CONV050                                                          
CONV060  ICM   RF,15,AUIDTAB                                                    
CONV062  OC    0(10,RF),0(RF)                                                   
         BZ    CONV080                                                          
         CLC   0(2,RF),2(R3)                                                    
         BE    CONV070                                                          
         LA    RF,10(RF)                                                        
         B     CONV062                                                          
CONV070  OC    2(8,RF),2(RF)                                                    
         BNZ   CONV090                                                          
         MVC   2(8,RF),CTIKID                                                   
         B     CONV200                                                          
CONV080  EQU   *                                                                
         MVC   P+12(40),=CL40'MISSING ID NUMBER: '                              
         MVC   P+31(8),CTIKID                                                   
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
CONV090  EQU   *                                                                
         MVC   P+12(40),=CL40'DUPLICATE ID NUM: '                               
         MVC   P+31(8),CTIKID                                                   
         MVC   P+40(8),2(RF)                                                    
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV100  EQU   *                                                                
         ICM   RF,15,AUIDPTR                                                    
         MVC   0(2,RF),CTIKID+8                                                 
         LA    RF,10(RF)                                                        
         STCM  RF,15,AUIDPTR                                                    
         LA    R3,CTIDATA                                                       
CONV110  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    CONV120                                                          
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CONV110                                                          
CONV120  CLC   2(8,R3),=CL8'OBSTO   '                                           
         BNE   CONV200                                                          
         MVC   P+12(40),=CL40'DNSEDDS ID NUM: '                                 
         MVC   P+31(8),CTIKID+8                                                 
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CTILEN                                                      
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
* GET AGENCY PRINCIPLE USER ID                                        *         
***********************************************************************         
GETPID   NTR1  ,                                                                
         LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,IDNUM                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                        
         BNE   GPIDOK                                                           
* ??     DC    H'00'                                                            
         LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GPID020  CLI   0(R1),0                                                          
         BE    GPIDOK                                                           
         CLI   0(R1),X'02'                                                      
         BE    GPID030                                                          
         CLI   0(R1),CTDSTELQ                                                   
         BE    GPID040                                                          
GPID022  IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GPID020                                                          
*                                                                               
GPID030  MVC   IDNAME,2(R1)                                                     
         B     GPID022                                                          
*                                                                               
GPID040  MVC   IDDEST,2(R1)                                                     
         B     GPID022                                                          
*                                                                               
GPIDNO   B     NO                                                               
*                                                                               
GPIDOK   B     YES                                                              
         DROP  R4                                                               
         USING CTIREC,R2                                                        
         EJECT                                                                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* TABLE OF AGENCIES FOR WHICH TO REMOVE OLD AUTH RECORDS                        
*                                                                               
AGYTAB   DC    CL2'SN'                                                          
         DC    CL2'FI'                                                          
         DC    CL2'FJ'                                                          
         DC    CL2'NM'                                                          
         DC    CL2'L3'                                                          
         DC    CL2'L4'                                                          
         DC    CL2'L5'                                                          
         DC    CL2'L6'                                                          
         DC    CL2'L7'                                                          
         DC    CL2'L8'                                                          
         DC    CL2'LC'                                                          
AGYTABX  DC    X'00'                                                            
         SPACE 2                                                                
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
PROGRAM  DS    XL1                                                              
PACCVAL  DS    XL2                                                              
PACCADR  DS    A                                                                
PACCSAV  DS    XL2                                                              
BYTE     DS    XL1                                                              
SYSINDSV DS    XL1                                                              
SQFLAG   DS    XL1                                                              
IDNUM    DS    XL2                                                              
IDNAME   DS    CL10                                                             
IDDEST   DS    CL33                                                             
IDCTRY   DS    CL11                                                             
AGYALPH  DS    CL2                                                              
SYSEL    DS    XL24                                                             
WORK     DS    XL256                                                            
AUIDTAB  DS    A                                                                
AUIDPTR  DS    A                                                                
AUIDTABX DS    A                                                                
IOKEY    DS    XL(L'CTIKEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
UIDTAB   CSECT                     ** UID TABLE **                              
         DC    (10000*10)X'00'                                                  
UIDTABX  EQU   *                   ** UID TABLE **                              
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115CTCONVUIT 03/02/99'                                      
         END                                                                    
