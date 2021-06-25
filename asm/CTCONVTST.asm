*          DATA SET CTCONVTST  AT LEVEL 141 AS OF 10/19/99                      
*PHASE CONVTST                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'CONVTVI - CONVERT TERMINAL VALID IDS'                           
CONVTVI  CSECT                                                                  
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
         LA    R2,IO                                                            
         USING CTTKEY,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKEY,C'U'                                                      
         GOTO1 ,DMCB,DMRDHI,CTFILE,CTTKEY,CTTKEY                                
*                                                                               
CONV14   GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         B     CONV200                                                          
*                                                                               
         MVC   P,SPACES                                                         
CONV0    CLI   CTTKTYP,CTTKTYPQ                                                 
         BNE   CONV200                                                          
         B     CONV004                                                          
*                                                                               
CONV004  EQU   *                                                                
         OC    CTTKSPAR(24),CTTKSPAR                                            
         BZ    CONV200                                                          
         OC    CTTKPASS(8),CTTKPASS                                             
         BNZ   CONV200                                                          
         TM    CTTSTAT,X'04'                                                    
         BO    CONV200                                                          
         OC    CTTKTID,CTTKTID                                                  
         BZ    CONV006                                                          
         MVC   P(L'CTTKTID),CTTKTID                                             
         CLC   CTTKTID(2),=CL2'KZ'                                              
         BE    CONV008                                                          
         CLC   CTTKTID(2),=CL2'SZ'                                              
         BE    CONV008                                                          
         B     CONV200                                                          
CONV006  EQU   *                                                                
         GOTO1 VHEXOUT,PARM,CTTKPASS+8,P,2,=C'TOG'                              
         GOTO1 VHELLO,PARM,(C'G',CTFILE),(X'03',CTTREC),0,0                     
         CLI   PARM+12,0                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R3,PARM+12                                                       
         CLI   1(R3),10                                                         
         BNE   CONV200                                                          
         CLC   2(2,R3),=CL2'KZ'                                                 
         BE    CONV008                                                          
         CLC   2(2,R3),=CL2'SZ'                                                 
         BE    CONV008                                                          
         B     CONV200                                                          
*                                                                               
CONV008  EQU   *                                                                
         LA    R3,CTTDATA                                                       
         SR    R0,R0                                                            
*                                                                               
         USING CTIDD,R3                                                         
CONV010  CLI   CTIDEL,0                                                         
         BE    CONV040                                                          
         CLI   CTIDEL,CTIDELQ                                                   
         BE    CONV020                                                          
*                                                                               
CONV012  SR    R0,R0                                                            
         IC    R0,CTIDLEN                                                       
         AR    R3,R0                                                            
         B     CONV010                                                          
*                                                                               
CONV020  EQU   *                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(12),0(R3)                                                   
         SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,PARM,(C'D',CTFILE),((R0),CTTREC),0,0                      
         CLI   PARM+12,0                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+12(40),=CL40'VALID ID DROPPED:'                                
         MVC   P+40(12),WORK                                                    
         GOTO1 VPRINTER                                                         
         XC    WORK,WORK                                                        
         B     CONV010                                                          
         DROP  R3                                                               
*                                                                               
         USING CTIDD,R3                                                         
CONV040  EQU   *                                                                
         LA    R4,VIDTAB                                                        
CONV042  EQU   *                                                                
         CLI   0(R4),X'FF'                                                      
         BE    CONV200                                                          
         CLM   R4,15,VIDTABX                                                    
         BNL   CONV200                                                          
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   CTIDEL,CTIDELQ                                                   
         MVI   CTIDLEN,12                                                       
         MVC   CTID(10),0(R4)                                                   
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+12(40),=CL40'VALID ID ADDED:'                                  
         MVC   P+40(12),WORK                                                    
*??      GOTO1 VPRINTER                                                         
         CLC   CTTLEN(2),=YL2(980)                                              
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R4,10(R4)                                                        
         B     CONV042                                                          
         DROP  R3                                                               
*                                                                               
CONV100  MVC   P+40(40),=CL40'TERMINAL RECORD NOT CONVERTED'                    
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV120  MVC   P+40(40),=CL40'PRINTER RECORD NOT CONVERTED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV140  MVC   P+40(40),=CL40'TERMINAL RECORD ALREADY CONVERTED'                
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV160  MVC   P+40(40),=CL40'AUTOMODE RECORD NOT CONVERTED'                    
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CONV200  SR    RE,RE                                                            
         ICM   RE,3,CTTLEN                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
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
*                                                                               
* TABLE OF VALID IDS                                                            
*                                                                               
VIDTAB   DC    CL10'KRGSEC'                                                     
         DC    CL10'KTVSEC'                                                     
         DC    CL10'KAM*'                                                       
         DC    CL10'KNA*'                                                       
         DC    CL10'KCO*'                                                       
         DC    CL10'AM*'                                                        
         DC    CL10'NK*'                                                        
         DC    CL10'CQ*'                                                        
         DC    CL10'KTV*'                                                       
         DC    CL10'KR*'                                                        
         DC    CL10'CHR*'                                                       
         DC    CL10'EAS*'                                                       
         DC    CL10'KH*'                                                        
         DC    CL10'BAN*'                                                       
         DC    CL10'SEN*'                                                       
         DC    CL10'SYN*'                                                       
         DC    CL10'SELSEC'                                                     
         DC    XL2'00',CL8'DIMNY'                                               
         DC    XL2'00',CL8'DIMLA'                                               
         DC    CL10'SZ*'                                                        
         DC    CL10'SEL*'                                                       
         DC    CL10'INT*'                                                       
         DC    CL10'KRG*'                                                       
         DC    CL10'ABC*'                                                       
         DC    CL10'ETV*'                                                       
         DC    CL10'TTV*'                                                       
         DC    CL10'CR*'                                                        
         DC    CL10'KU*'                                                        
         DC    CL10'EA*'                                                        
         DC    CL10'KF*'                                                        
         DC    CL10'BF*'                                                        
         DC    XL2'00',CL8'EAGLE'                                               
         DC    XL2'00',CL8'KATZ'                                                
VIDTABX  DC    X'FF'                                                            
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
WORK     DS    XL256                                                            
IOL      DS    F                                                                
IO       DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141CTCONVTST 10/19/99'                                      
         END                                                                    
