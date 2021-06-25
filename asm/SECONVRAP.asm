*          DATA SET SECONVRAP  AT LEVEL 105 AS OF 02/02/96                      
*PHASE SECRAP                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SECRAS - REPORT ON ACCESS SECURITY'                             
SECRAS   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(29),=C'AGENCY ACCESS SECURITY REPORT'                      
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING CT0KEY,R2                                                        
         XC    CT0KEY,CT0KEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CT0KEY,CT0KEY                        
         B     CONV15A                                                          
*                                                                               
CONV14   MVC   CT0KEY(L'CT0KEY),IOKEY                                           
         CLI   SQFLAG,0                                                         
         BE    CONV15                                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT0KEY,CT0KEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
CONV15   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CT0KEY,CT0KEY                        
*                                                                               
CONV15A  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'CT0KEY),CT0KEY                                           
*                                                                               
         MVC   P,SPACES                                                         
CONV0    CLI   CT0KTYP,C'0'                                                     
         BNE   CONV200                                                          
         CLC   CT0KAGY,=CL2'TH'                                                 
         BNE   CONV200                                                          
         OC    CT0KCODE(8),CT0KCODE                                             
         BZ    CONV200                                                          
         MVC   AGYALPH,CT0KAGY                                                  
*                                                                               
CONV20   MVC   P(2),AGYALPH                                                     
         MVC   P+2(10),CT0KCODE                                                 
         LA    R3,CT0DATA                                                       
         SR    R0,R0                                                            
         USING CTSYSD,R3                                                        
CONV22   CLI   CTSYSEL,0           FIND ACC OR MED SYSTEM ELEMENT               
         BE    CONV100                                                          
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   CONV28                                                           
         CLI   CTSYSNUM,X'0D'      STR SYSTEM                                   
         BNE   CONV28                                                           
         B     CONV30                                                           
*                                                                               
CONV28   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CONV22                                                           
*                                                                               
CONV30   EQU   *                                                                
         OC    CTSYSLMT,CTSYSLMT                                                
         BZ    CONV100                                                          
         MVC   P+20(40),=CL40'<<* LIMIT ACCESS ON STR'                          
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CONV100  EQU   *                                                                
         MVC   P+20(40),=CL40'NO LIMIT ACCESS'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CT0LEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
*        PUT   TAPEOUT,IOL                                                      
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
SYSINDSV DS    XL1                                                              
SQFLAG   DS    XL1                                                              
IDNUM    DS    XL2                                                              
IDNAME   DS    CL10                                                             
IDDEST   DS    CL33                                                             
IDCTRY   DS    CL11                                                             
AGYALPH  DS    CL2                                                              
SYSEL    DS    XL24                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT0KEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105SECONVRAP 02/02/96'                                      
         END                                                                    
