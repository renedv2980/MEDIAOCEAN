*          DATA SET NEMED89    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T31E89A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'T31E89 - QUICK FIX MODULE'                                      
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK FILEFIX                                                               
*   THIS PROGRAM IS A TEMPORARY TO FIX A SPECIAL PROBLEM                        
*   IT USES SCREEN AND EDIT MODULE OF FILEFIX REPORT                            
**************************************************************                  
T31E89   CSECT                                                                  
         NMOD1 0,**NEQF**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         XC    COUNTER,COUNTER                                                  
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
         XIT1                                                                   
*                                                                               
VK       DS    0H                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   FTERMFLG,1          OPTIONAL                                     
         MVI   NBSELPST,C'B'       LOCKED AND UNLOCKED                          
         LA    RE,PAKTBL           CLEAR TABLE                                  
         LA    RF,500                                                           
         XCEF                                                                   
         LA    R2,UVLCLIH          CLIENT                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         CLI   TWAOFFC,C'*'        DDS                                          
         BNE   EDTINV                                                           
         NETGO NVCLIALL,DMCB,UVLCLIN                                            
*                                                                               
         LA    R2,UVLESTH                                                       
         NETGO NVESTALL,DMCB                                                    
*                                                                               
         LA    R2,UVLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,UVLPAKH                                                       
         NETGO NVPAK,DMCB                                                       
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPAKEL,R2                                                        
         TM    NPAKCNTL,X'40'      ..IS IT IMP BASED PACKAGE                    
         BNO   EDSTR                                                            
         LA    R3,PAKTBL           ..YES/SET IN PAKTBL                          
         L     R2,NBAIO                                                         
         USING NPKEY,R2                                                         
         MVC   0(1,R3),NPKPACK                                                  
         MVC   1(4,R3),NPKNET                                                   
*                                                                               
EDSTR    LA    R2,UVLSTRH                                                       
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,UVLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,0                                                       
         LA    R2,UVLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   *+8                                                              
         MVI   UVLTST,C'Y'         DEFAULT TO TEST RUN                          
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,UVLPRTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   EDIT20                                                           
         MVI   UVLPRT,C'N'         DEFAULT TO NO PRINT                          
*                                                                               
EDIT20   MVI   FTERMFLG,0                                                       
         MVI   ERROR,INVALID                                                    
         LA    R2,UVLTYPH          S=STATION,P=POST,B=BOTH                      
         NETGO NVGETFLD,DMCB                                                    
         CLI   UVLTYP,C'S'                                                      
         BE    EDITXX                                                           
         CLI   UVLTYP,C'P'                                                      
         BE    EDITXX                                                           
         CLI   UVLTYP,C'B'                                                      
         BNE   EDTINV                                                           
*                                                                               
*                                                                               
EDITXX   LA    R2,UVLCLIH                                                       
         B     XMOD                                                             
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
*                                                                               
EDTINV   MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR,DMCB                                                     
*                                                                               
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
*                                                                               
PR       DS    0H                                                               
         LA    R2,MAINLINE         SET NBHOOK WITH MAINLINE                     
         ST    R2,NBHOOK                                                        
         MVI   NBSELPST,C'B'                                                    
FX6      NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   FX6                                                              
         MVC   P(13),=C'UNITS UPDATED'                                          
         L     R3,COUNTER                                                       
         EDIT  (R3),(7,P+14)                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,PAKTBL                                                        
         LA    R2,P                                                             
FX6B     CLI   0(R3),0                                                          
         BE    FX6C                                                             
         EDIT  (B1,0(R3)),(3,1(R2))                                             
         MVC   4(4,R2),1(R3)                                                    
         LA    R3,5(R3)                                                         
         LA    R2,8(R2)                                                         
         B     FX6B                                                             
FX6C     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE                                                                  
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,0                                                       
         MVI   NBNOWRIT,0                                                       
*                                                                               
         CLI   NBMODE,NBPROCPK    IF PACKAGE                                    
         BNE   FXM30                                                            
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   P(5),=C'PACKG'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         USING NPAKEL,R2                                                        
         TM    NPAKCNTL,X'40'      ..IS IT IMP BASED PACKAGE                    
         BNO   XIT                                                              
         BAS   RE,HEXIT                                                         
         LA    R3,PAKTBL           ..YES/SET IN PAKTBL                          
FXM10    CLI   0(R3),0                                                          
         BE    FXM20                                                            
         CLC   0(2,R3),=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                PAKTBL FULL/EXPAND IT                        
         LA    R3,5(R3)                                                         
         B     FXM10                                                            
FXM20    L     R2,NBAIO                                                         
         USING NPRECD,R2                                                        
         MVC   0(1,R3),NPKPACK     SET PACKAGE NUMBER IN TABLE                  
         MVC   1(4,R3),NPKNET            NETWORK                                
         B     XIT                                                              
*                                                                               
FXM30    CLI   NBMODE,NBPROCUN    IF UNIT                                       
         BNE   XIT                                                              
         LA    R3,PAKTBL           FILTER ON PACKAGE                            
FXM32    CLC   0(2,R3),=X'FFFF'    NO MATCH/EXIT                                
         BE    XIT                                                              
         CLC   0(1,R3),NBPACK                                                   
         BNE   FXM34                                                            
         CLC   1(4,R3),NBACTNET       AND NETWORK                               
         BE    FXM40                                                            
FXM34    LA    R3,5(R3)                                                         
         B     FXM32                                                            
FXM40    L     R2,NBAIO                                                         
         MVI   ELCODE,X'5D'        GET BOOK ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(3,R2),=C'EVN'     IS IT EVN                                    
         BNE   XIT                                                              
         MVC   2(3,R2),=C'EIN'     YES/CHANGE TO IMPRESSION BASED               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'33'            AND CLEAR VPH ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R2)            GET LENGTH OF ELEMENT                        
         S     R1,=F'4'            SET UP FOR EXECUTE                           
         LA    R2,3(R2)            POINT TO VPHS                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       CLEAR VPHS                                   
*                                                                               
         CLI   UVLTST,C'Y'         IS IT TEST                                   
         BE    FX30                                                             
         MVI   NBUPUNIT,C'Y'       NO/SET ON WRITE SWITCHES                     
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
FX30     L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         BAS   RE,HEXIT                                                         
*                                                                               
FFXIT    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
HEXIT    NTR1                                                                   
         L     R2,NBAIO                                                         
         GOTO1 HEXOUT,DMCB,(R2),P,100                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
HEX200   NTR1                                                                   
         GOTO1 HEXOUT,DMCB,(R2),P,200                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
*                                                                               
COUNTER  DS    F                                                                
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
**** PASDATA STORAGE (IN W/S AREA1 FROM EDIT OVERLAY) ***                       
WORKD    DSECT                                                                  
         DS    0F                                                               
RELO     DS    F                                                                
NOSTAT   DS    F                                                                
NO02     DS    F                                                                
MYELEM   DS    CL20                                                             
ONECNT   DS    F                                                                
NSCNT    DS    F                                                                
OCCNT    DS    F                                                                
PAKTBL   DS    CL500                                                            
         DC    X'FFFF'                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDCD                                                       
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NEMED89   05/01/02'                                      
         END                                                                    
