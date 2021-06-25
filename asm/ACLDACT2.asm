*          DATA SET ACLDACT2   AT LEVEL 034 AS OF 11/09/15                      
*PHASE ACXACT2A                                                                 
*INCLUDE ACRECTYP                                                               
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
         DC    C'*VERIFY*'                                                      
         DC    X'FB'                                                            
         DC    C'31030'                                                         
         DC    X'FC'                                                            
         DC    C'91231',C'T2'                                                   
COMPNYQL EQU   X'41'               DDST2                                        
COMPNYQH EQU   X'98'               QAROD3                                       
                                                                                
         USING ACTRECD,R2                                                       
DMXRTSTN GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         CLI   RECTYPE,ACRTCPY     COMPANY RECORDS                              
         BNE   DMXKEEP                                                          
         CLI   1(R1),COMPNYQL                                                   
         BL    DMXKEEP                                                          
         CLI   1(R1),COMPNYQH                                                   
         BNH   DMXPURGE                                                         
DMXRTST  DS    0H                                                               
*        B     DMXPURGE                                                         
         B     DMXKEEP             KEEP FOR LIVE RUNNING                        
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
DMXRET   DS    0H                                                               
DMXEOF   DS    0H                                                               
DMXINIT  DS    0H                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
*                                                                               
DMXREC   L     R2,VISREC                                                        
         USING ACTRECD,R2                                                       
         TM    VTAPEOUT,X'10'                                                   
         BNO   DMXREC00                                                         
         GOTO1 VRECTYP,DMCB,(C'I',ACTRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         CLI   RECTYPE,ACRTCPYP    ENSURE RECORD                                
         BL    DMXREC00            IS                                           
         CLI   RECTYPE,ACRTDIRP    NOT CONFINED                                 
         BH    DMXREC00            TO VISREC                                    
         CLI   ACTKSTA,ACTSDELT    PURGE LOGICALLY DELETED PLARECS              
         BL    DMXREC04                                                         
         MVC   P+1(13),=C'PLAREC PURGED'                                        
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
                                                                                
DMXREC00 L     R2,VREC                                                          
         GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
                                                                                
DMXREC04 CLI   SW,C'N'             WANT EVERYTHING FROM NOW ON                  
         BE    DMXRTSTN            WAS DMXRTST BUT NO LONGER                    
         CLI   RECTYPE,ACRTTRNA    IS RECORD TYPE SPECIAL                       
         BH    DMXREC08            YES                                          
         CLI   1(R1),COMPNYQL                                                   
         BL    DMXKEEP                                                          
         CLI   1(R1),COMPNYQH                                                   
         BNH   DMXREC06                                                         
         MVI   SW,C'N'             IT IS TOO HIGH, CHANGE SWITCH                
         B     DMXKEEP                                                          
                                                                                
DMXREC06 CLI   RECTYPE,ACRTCPY     COMPANY RECORDS                              
         BE    DMXKEEP             NOT TO BE REFRESHED TONIGHT                  
                                                                                
DMXREC08 B     DMXPURGE            YES DELETE                                   
                                                                                
         EJECT                                                                  
         LTORG                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
SW       DC    C'Y'                                                             
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    XL64                                                             
         DS    0D                                                               
*                                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
*                                                                               
RECTYPE  DS    X                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACLDACT2  11/09/15'                                      
         END                                                                    
