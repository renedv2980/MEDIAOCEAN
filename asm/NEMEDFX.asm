*          DATA SET NEMEDFX    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T31EFXA                                                                  
         TITLE 'T31EFX - NETWORK FILEFX '                                       
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK FILE FIX REPORT                                                       
*                                                                               
*                                                                               
**************************************************************                  
T31EFX   CSECT                                                                  
         NMOD1 0,**NEFX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
***      L     RA,ATWA                                                          
***      USING T31EFFD,RA                                                       
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T31EFX,RB,RA        NOTE 2ND BASE REGISTER                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    RE,PROGTBL          CLEAN PROGTBL                                
         SR    RF,RF                                                            
         A     RF,=F'6000'                                                      
         XCEF                                                                   
         AR    RE,RF                                                            
         MVI   0(RF),X'FF'         SET EOF OF PROGTBL                           
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************                           
*                                                                               
VK       DS    0H                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
*****************************************************                           
*                                                                               
PR       DS    0H                                                               
         MVI   NBDATA,C'U'         READ UNITS                                   
         MVI   NBSELPST,C'B'       LOCKED AND UNLOCKED                          
         LA    R2,NETIOHK          SET NBHOOK WITH MAINLINE                     
         ST    R2,NBHOOK                                                        
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         B     FX20                             NO MORE UNITS                   
         SPACE                                                                  
*                                                                               
NETIOHK  NTR1                                                                   
         CLI   NBMODE,NBPROCUN                                                  
         BNE   FXX                                                              
*                                                                               
         CLC   =C'ABC',NBACTNET    JUST AFURTHER CHECK ON NETIO                 
         BNE   FXX                                                              
         CLC   =C'NC',NBACTCLI     ANOTHER CHECK ON NETIO                       
         BNE   FXX                                                              
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,1                                                         
         USING NUMAINEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =X'0003',NUMARKET   AND ANOTHER CHECK                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NUMARKET,=X'0001'   SET ABC TO 01 MARKET NUMBER                  
*                                                                               
         LA    R3,PROGTBL          ADD PROGRAM CODES TO PROGTBL                 
FX7      CLI   0(R3),X'FF'         EOF                                          
         BNE   *+6                                                              
         DC    H'0'                PROGTBL FULL BOMB                            
         CLC   0(6,R3),NBACTPRG    IF DUPLICATE                                 
         BE    FX12                   SKIP                                      
         CLI   0(R3),0                                                          
         BE    FX10                                                             
         LA    R3,6(R3)                                                         
         B     FX7                                                              
FX10     MVC   0(6,R3),NBACTPRG                                                 
*                                                                               
FX12     DS    0H                                                               
         MVI   NBUPUNIT,C'Y'       SET ON PUTREC SWITCH FOR NETIO               
         MVI   NBNOWRIT,C'Y'       SET ON WRITE SWITCH FOR NETIO                
*                                                                               
FXX      B     EXIT                                                             
         EJECT                                                                  
************************************************                                
* READ THE STATION MASTER RECORD                                                
*                                                                               
FX20     DS    0H                                                               
** LET US NOT DO THIS HERE. SPOOF CANNOT READ STATION FILE FOR UPDATE           
**                                                                              
**                                                                              
         B     FX30                                                             
         EJECT                                                                  
*********************************************                                   
* PROGRAM RECORDS DELETE THE ABC MKTNO=3                                        
*                 ADD        ABC MKTNO=1                                        
*                                                                               
FX30     DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         LA    R3,PROGTBL                                                       
         LA    R2,KEY                                                           
         USING NPGRECD,R2                                                       
FX32     XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'    ACTIVE KEY                                   
         MVC   NPGKAM,NBSELAM                                                   
         MVC   NPGKNET,=X'0003'                                                 
         MVC   NPGKPROG,0(R3)                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         BAS   RE,PRINTIT                        ** BEFORE                      
*                                                                               
         BAS   RE,DELFILE          DELETE RECORD                                
*                                                                               
         MVC   NPGKNET,=X'0001'    SET KEY TO ABC                               
*                                                                               
         BAS   RE,ADDFILE          ADD NEW REC                                  
         BAS   RE,PRINTIT                          ** AFTER                     
*                                                                               
*   NOW DO PASSIVE KEY                                                          
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0DA0'    PASSIVE KEY                                  
         MVC   NPGKAM,NBSELAM                                                   
         MVC   NPGKNET,=X'0003'                                                 
         MVC   NPGKPROG(1),NPGDAY                                               
         MVC   NPGKPROG+1(4),NPGTIME                                            
         MVC   NPGKPROG+5(1),NPGUNIQ                                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         BAS   RE,WRTDIR                                                        
*                                                                               
         MVC   NPGKNET,=X'0001'        SET ABC                                  
         NI    KEY+13,X'7F'        TURN OF DELETE BIT                           
         BAS   RE,WRTDIR                                                        
*                                                                               
*                                                                               
         LA    R3,6(R3)            BUMP PROG TABLE                              
         CLI   0(R3),X'FF'         EOF                                          
         BE    FX40                                                             
         CLI   0(R3),0             EOF                                          
         BE    FX40                                                             
         B     FX32                GET NEXT PROG REC                            
*                                                                               
FX40     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
DELFILE  NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   DELFX                                                            
         L     R1,NBAIO                                                         
         OI    15(R1),X'80'        DELETE RECORD                                
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOPUT                                                         
         OI    KEY+13,X'80'        DLELETE KEY                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR           WRITE TO DIRECTORY                           
DELFX    B     EXIT                                                             
*                                                                               
ADDFILE  NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   ADDFX                                                            
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOADD                                                         
ADDFX    B     EXIT                                                             
         EJECT                                                                  
* DATAMGR CALLS                                                                 
*                                                                               
WRTDIR   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         B     EXIT                                                             
*                                                                               
IOGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,NBAIO,(0,DMWORK)         
         B     EXIT                                                             
*                                                                               
IOADD    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',FILENAME,KEY,NBAIO,(0,DMWORK)            
         B     EXIT                                                             
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,NBAIO,P,50      ** AFTER                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
HOOK     DS 0H                                                                  
         NTR1                                                                   
         MVC   H3+10(3),NBSELCLI                                                
         MVC   H5+15(4),NBSELNET                                                
HKX      B     XIT                 (XIT1)                                       
         EJECT                                                                  
         SPACE                                                                  
* SUB-ROUTINE TO DELETE ELMENT (R6 POINTS TO RECORD)                            
*                                                                               
DELELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(ELCODE,(R6)),0                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* SUB-ROUTINE TO PUT ELEMENT (R4 POINTS TO ELEMENT)                             
*                                                                               
PUTELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(ELCODE,(R6)),(R4)                     
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PROGTBL  DS    CL6000       FOR 600 PROGRAM RECORDS                             
PROGTBLX DS    CL1                                                              
         EJECT                                                                  
         SPACE                                                                  
WORKD    DSECT                WORKING STORAGE                                   
RELO     DS    F                                                                
ELCODE   DS    CL1                                                              
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE1D                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENPROG                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMEDFX   05/01/02'                                      
         END                                                                    
