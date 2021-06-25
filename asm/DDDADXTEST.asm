*          DATA SET DDDADXTEST AT LEVEL 021 AS OF 02/24/00                      
*PHASE DADXTEST,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
DADXTEST TITLE '- PROGRAM TO TEST DMDANDX'                                      
                                                                                
DADXTEST CSECT                                                                  
         NBASE WORKL,**TEST**,WORK=V(REGSAVE)                                   
         USING WORKD,RC                                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(19),=C'Tester for DMDANDX'                                 
                                                                                
INIT02   GOTOR VCARDS,DMCB,C,=C'RE00'                                           
         CLI   C,C'/'                                                           
         BE    ENDRUN                                                           
         MVC   P(L'C),C                                                         
         GOTOR =V(PRINTER)                                                      
         CLI   C,C'*'              IGNORE COMMENTS                              
         BE    INIT02                                                           
         CLI   C,C' '                                                           
         BE    INIT02                                                           
         CLC   =C'RUN',C           USER WANTS TO RUN NOW                        
         BE    TEST                                                             
         CLC   =C'DDSIO',C                                                      
         BNE   INIT04                                                           
         L     RF,VDDSIO                                                        
         MVC   0(8,RF),C+6                                                      
         B     INIT02                                                           
                                                                                
INIT04   DS    0H                                                               
         DC    H'0'                                                             
                                                                                
TEST     GOTO1 VDATAMGR,DMCB,DMOPEN,DMSPOT,DMFLST,IO                            
         XC    KEY,KEY                                                          
*        MVC   KEY(3),=C'RTA'                                                   
         MVI   KEY,C'P'                                                         
*        GOTO1 VDATAMGR,DMCB,DMRDHI,DEMDIR,KEY,KEY                              
         GOTO1 VDATAMGR,DMCB,DMRDHI,PAVDIR,KEY,KEY                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TEST04                                                           
                                                                                
*EST02   GOTO1 VDATAMGR,DMCB,DMRSEQ,DEMDIR,KEY,KEY                              
TEST02   GOTO1 VDATAMGR,DMCB,DMRSEQ,PAVDIR,KEY,KEY                              
         BNE   TESTX                                                            
                                                                                
TEST04   MVC   IO(L'KEY),KEY                                                    
         XC    IO+L'KEY(2),IO+L'KEY                                             
*        GOTO1 VDATAMGR,DMCB,DMRDHI,DEMFIL,NDXDA,IO,0                           
         GOTO1 VDATAMGR,DMCB,DMRDHI,PAVFIL,NDXDA,IO,0                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   PKEY,KEY                                                         
         TR    PKEY,TRTAB                                                       
         GOTO1 VHEXOUT,HEXP,NDXDA,PDA,L'NDXDA,=C'TOG'                           
         L     R1,DMCB5                                                         
         ICM   R1,15,DNDX-DTFPHD(R1)                                            
         SR    RF,RF                                                            
         ICM   RF,3,4(R1)                                                       
         SHI   RF,10                                                            
         SR    RE,RE                                                            
         D     RE,=F'6'                                                         
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PENTRY,DUB                                                       
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,HEXP,KEY,HEXWRK1,L'KEY,=C'SEP'                           
         MVC   PKEY,HEXWRK1                                                     
         GOTO1 VPRINTER                                                         
         MVC   PKEY,HEXWRK2                                                     
         BASR  RE,RF                                                            
         B     TEST02                                                           
                                                                                
                                                                                
TESTX    ZAP   LINE,=P'99'                                                      
         ZAP   LINE,=P'99'         SET NEW PAGE                                 
         B     INIT02              GO BACK FOR MORE PUNISHMENT                  
                                                                                
ENDRUN   XBASE ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
VDATAMGR DC    V(DATAMGR)                                                       
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
                                                                                
DMSPOT   DC    C'SPOT   '                                                       
DMOPEN   DC    C'OPEN   '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DEMDIR   DC    C'DEMDIR '                                                       
DEMFIL   DC    C'DEMFIL '                                                       
PAVDIR   DC    C'PAVDIR '                                                       
PAVFIL   DC    C'PAVFIL '                                                       
                                                                                
DMFLST   DS    0C                                                               
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRO'                                                      
         DC    C'NDEMFILN'                                                      
         DC    C'NDEMFILO'                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMFILA'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NDEMFILR'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NPAVFIL '                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NNTIFIL '                                                      
         DC    C'NHUTFL  '                                                      
DMFLSTX  DC    C'X'                                                             
                                                                                
         ENTRY UTL                                                              
UTL      DC    F'0',AL1(2),AL3(0)                                               
SSB      DC    256X'00'                                                         
                                                                                
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 00-0F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 10-1F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 20-2F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 30-3F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B4A4B4C4D4E4F' 40-4F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B5A5B5C5D5E5F' 50-5F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B6A6B6C6D6E6F' 60-6F                        
         DC    X'4B4B4B4B4B4B4B4B4B4B7A7B7C7D7E7F' 70-7F                        
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' 80-8F                        
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B' 90-9F                        
         DC    X'4B40E2E3E4E5E6E7E8E94B4B4B4B4B4B' A0-AF                        
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' B0-BF                        
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' C0-CF                        
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B' D0-DF                        
         DC    X'4B40E2E3E4E5E6E7E8E94B4B4B4B4B4B' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' F0-FF                        
                                                                                
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
DUB      DS    D                                                                
DMCB     DS    6A                                                               
         ORG   DMCB                                                             
DMCB1    DS    A                                                                
DMCB2    DS    A                                                                
DMCB3    DS    A                                                                
DMCB4    DS    A                                                                
DMCB5    DS    A                                                                
DMCB6    DS    A                                                                
                                                                                
HEXP     DS    6F                                                               
                                                                                
C        DS    CL80                                                             
                                                                                
HEXWRK1  DS    CL(L'KEY)                                                        
HEXWRK2  DS    CL(L'KEY)                                                        
                                                                                
KEY      DS    XL18                                                             
         DS    XL1                                                              
NDXDA    DS    XL4                                                              
IO       DS    XL2000                                                           
WORKL    EQU   *-WORKD                                                          
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         ORG   P+1                                                              
PKEY     DS    CL(L'KEY)                                                        
         DS    C                                                                
PDA      DS    CL(L'NDXDA*2)                                                    
         DS    C                                                                
PENTRY   DS    CL4                                                              
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DDDADXTEST02/24/00'                                      
         END                                                                    
