*          DATA SET DEMODIC    AT LEVEL 051 AS OF 05/26/87                      
*PHASE DICTCONV,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
         TITLE 'DICTIONARY TRANSFER PROGRAM'                                    
DEMODIC  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEMODIC,VREGSAVE                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT                                                             
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
*                                                                               
         EJECT                                                                  
INIT     DS    0H                                                               
         LA    RE,DEMODIC          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         EJECT                                                                  
         OPEN  (DICOUT,(OUTPUT))                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'PERSON',FLIST,AREC                
         MVC   TITLE(30),=CL30'DICTIONARY TRANSFER PROGRAM'                     
         XC    DICKEY,DICKEY                                                    
         MVI   DICKEY,C'D'                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'PERDIR',DICKEY,DICKEY             
         SR    R4,R4                                                            
LOOP     GOTO1 =V(DATAMGR),DMCB,=C'GETREC',=C'PERFIL',DICKEY+38,REC,DMW         
         ICM   R0,3,SRCLEN                                                      
         SH    R0,=H'2'                                                         
         STCM  R0,3,DESLEN                                                      
         AH    R0,=H'4'                                                         
         STCM  R0,3,NEWREC                                                      
         XC    PAD3,PAD3                                                        
         XC    PAD2,PAD2                                                        
         MVC   ENTDES,ENTSRC                                                    
         MVC   DICDES,DICSRC                                                    
         XC    PAD1,PAD1                                                        
         MVI   DICTYP,3                                                         
         MVI   DICSYS,0                                                         
         XC    PAD0,PAD0                                                        
         PUT   DICOUT,NEWREC                                                    
         LA    R4,1(R4)                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'PERDIR',DICKEY,DICKEY             
         CLI   DICKEY,C'D'                                                      
         BE    LOOP                                                             
*                                                                               
         EJECT                                                                  
ENDIN    CLOSE (DICOUT,)                                                        
         MVC   P(30),=CL30'NUMBER OF RECORDS =           '                      
         EDIT  (R4),(5,P+24)                                                    
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
DICOUT   DCB   DDNAME=DICOUT,DSORG=PS,RECFM=VB,LRECL=4004,             X        
               BLKSIZE=8142,MACRF=PM                                            
*                                                                               
         EJECT                                                                  
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
DMW      DS    12D                                                              
DUB      DS    D                                                                
DICKEY   DS    CL42                                                             
AREC     DS    A(REC)                                                           
UTL      DC    F'0',X'0E'                                                       
SSB      DC    F'0'                                                             
FLIST    DS    0H                                                               
         DC    CL8' PERFILE'                                                    
         DC    CL8' PERDIR '                                                    
         DC    CL8'X       '                                                    
NEWREC   DS    CL2                                                              
REC      DS    CL2100                                                           
         ORG   REC                                                              
         DS    CL3                                                              
DICSRC   DS    CL8                                                              
ENTSRC   DS    CL8                                                              
         DS    CL17                                                             
SRCLEN   DS    XL2                                                              
         DS    CL6                                                              
         ORG   REC                                                              
PAD0     DS    CL2                                                              
DICSYS   DS    XL1                                                              
DICTYP   DS    XL1                                                              
PAD1     DS    CL10                                                             
DICDES   DS    CL8                                                              
ENTDES   DS    CL8                                                              
PAD2     DS    CL4                                                              
DESLEN   DS    XL2                                                              
PAD3     DS    CL8                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051DEMODIC   05/26/87'                                      
         END                                                                    
