*          DATA SET SRGRF00    AT LEVEL 038 AS OF 05/18/07                      
*PHASE T10800A                                                                  
         TITLE '$SRGRF - PROCESS IBM 3780 RJE AS A TERMINAL'                    
         PRINT NOGEN                                                            
GRF00    CSECT                                                                  
         NMOD1 WRKX-WRKD,*$GRF**,CLEAR=YES,RR=R4                                
         USING WRKD,RC                                                          
         ST    R4,RELO                                                          
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM2                                                       
*                                  R3=A(TIA)                                    
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
*                                                                               
         L     R4,SRPARM4          R4=A(COMFACS)                                
         USING COMFACSD,R4                                                      
*                                                                               
         LA    R5,CIREC                                                         
*                                                                               
         LA    R3,64(R3)           GET START OF MESSAGE FROM GRAPHNET           
         CLC   10(3,R3),=C'DLN'       PROCESS ONLY DELIVERY NOTICES             
         BE    FNDLP                                                            
         CLC   20(3,R3),=C'DLN'       2ND FORMAT... THE RATS                    
         BNE   NPEXIT                                                           
*                                                                               
FNDLP    LA    R3,1(R3)                                                         
         CLI   0(R3),3                 EXIT IF NO ID                            
         BE    NPEXIT                                                           
         CLC   0(3,R3),=C'ID-'         LOOK FOR ID                              
         BNE   FNDLP                                                            
         EJECT                                                                  
*              EXTRACT PQ KEY FROM RETURNED DATA                                
         SPACE 3                                                                
         PACK  DUB,3(6,R3)         GET USER ID                                  
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         MVC   NXSRCID,DUB                                                      
*                                                                               
         MVC   NXSUBID,9(R3)       GET SUB ID                                   
*                                                                               
         PACK  DUB,12(4,R3)        GET REPORT NO                                
         CVB   R0,DUB                                                           
         STH   R0,DUB                                                           
         MVC   NXREPNO,DUB                                                      
         EJECT                                                                  
*                                  SEARCH PRTQUE INDEX FOR REPORT               
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'08',INDEX),PRTQUE,NDX,SAVE,(R5),0               
         CLI   8(R1),0                                                          
         BE    SRCH2                                                            
         TM    8(R1),X'80'         EOF - REPORT NOT FOUND                       
         BO    NPEXIT                                                           
         DC    H'0'                ERR - DIE ON INDEX READ DISK ERROR           
*                                                                               
*                 TEST TO MAKE SURE THIS IS THE RIGHT REPORT                    
         SPACE 2                                                                
SRCH2    CLI   NXCLASS,C'G'        MUST BE CLASS G                              
         BNE   NPEXIT                                                           
         CLI   NXSTAT,PQSTSE       MUST BE SENT                                 
         BNE   NPEXIT                                                           
*                                                                               
*                 CHANGE STATUS TO PRINTED AND RETAIN TO 6 HRS                  
*                                                                               
*                   UPDATE STATUS                                               
*                                                                               
         GOTO1 CDATAMGR,DMCB,STAPTD,PRTQUE,NDX,SAVE,(R5),0                      
         CLI   8(R1),0                                                          
         BNE   EREX                                                             
    B EXIT THIS IS TEMP TILL TESTING DONE                                       
*                                                                               
*                   UPDATE RETAIN                                               
*                                                                               
         MVC   NXINFO(3),=X'0001FF'          1 HOURS                            
         GOTO1 CDATAMGR,DMCB,STARTN,PRTQUE,NDX,SAVE,(R5),0                      
         CLI   8(R1),0                                                          
         BNE   EREX                                                             
*                                                                               
NPEXIT   NOP   EXIT                                                             
EXIT     XMOD1 1                                                                
*                                                                               
*                   ERROR IN UPDATE                                             
*                                                                               
EREX     DC    H'0'    DIE ON ERROR                                             
         EJECT                                                                  
PRTQUE   DC    CL8'PRTQUE'                                                      
INDEX    DC    CL8'INDEX'                                                       
STAPTD   DC    CL8'PRINTED'                                                     
STARTN   DC    CL8'RETAIN'                                                      
         LTORG                                                                  
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
NDX      DS    0XL32               INDEX ENTRY                                  
NXSRCID  DS    XL2                                                              
NXSUBID  DS    CL3                                                              
NXREPNO  DS    XL2                                                              
NXCLASS  DS    CL1                                                              
NXTYPE   DS    XL1                                                              
NXATTB   DS    XL1                                                              
NXSTAT   DS    XL1                                                              
NXSEQ    DS    XL1                                                              
NXAGES   DS    XL1                                                              
NXAGELD  DS    XL2                                                              
NXAGEDD  DS    XL2                                                              
NXAGERD  DS    XL2                                                              
NXAGERT  DS    XL1                                                              
NXINFO   DS    XL6                                                              
NXREPNOX DS    XL2                                                              
NXCIADDR DS    XL2                                                              
NXFLAG   DS    X                                                                
         DS    X                                                                
         DS    CL8                                                              
*                                                                               
*                                                                               
SAVE     DS    360C                                                             
*                                                                               
CIREC    DS    14336C                                                           
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
*PRTQD                                                                          
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SRGRF00   05/18/07'                                      
         END                                                                    
