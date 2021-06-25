*          DATA SET NEWRIFX    AT LEVEL 048 AS OF 05/01/02                      
*PHASE T32021A                                                                  
         TITLE 'T32021 - NETWORK FILEFX '                                       
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK FILE FIX REPORT                                                       
*                                                                               
*                                                                               
**************************************************************                  
T32021   CSECT                                                                  
         NMOD1 0,**NEFX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32021,RB,RA        NOTE 2ND BASE REGISTER                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    RE,PROGTBL                                                       
         LA    RF,600                                                           
         XCEF                                                                   
         LA    RE,PROGTBL                                                       
         A     RE,=F'600'                                                       
         MVI   0(RE),X'FF'         SET PRGTBL EOF                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************                           
*                                                                               
VK       DS    0H                                                               
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
         MVI   FTERMFLG,1          OPTIONAL                                     
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTALL,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,0                REQUIRED                               
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         MVI   DELETESW,0                                                       
         CLI   FLD,C'N'            IS IT A TEST RUN                             
         BNE   VKX                                                              
         MVI   DELETESW,C'Y'       NO/SO SET DELETESW                           
VKX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************                           
*                                                                               
PR       DS    0H                                                               
         MVI   NBDATA,C'U'         READ UNITS                                   
         MVI   NBSELPST,C'B'       LOCKED AND UNLOCKED                          
         LA    R2,NETIOHK          SET NBHOOK WITH MAINLINE                     
         ST    R2,NBHOOK                                                        
         MVI   COUNT,0                                                          
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         CLI   NBMODE,NBREQLST                                                  
         BNE   GETUNIT                                                          
         B     FX20                             NO MORE UNITS                   
         SPACE                                                                  
*                                                                               
NETIOHK  NTR1                                                                   
         CLI   NBMODE,NBPROCUN                                                  
         BNE   HOOKX                                                            
*                                                                               
         CLC   =C'ATI',NBACTNET    JUST AFURTHER CHECK ON NETIO                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,1                                                         
         USING NUMAINEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   HOOKX                                                            
         CLC   =X'0057',NUMARKET   AND ANOTHER CHECK                            
         BNE   HOOKX                                                            
         MVC   NUMARKET,=X'0058'   SET ATI TO 88 MARKET NUMBER                  
*                                                                               
         MVC   NBAIO,AIO1                                                       
         LA    R3,PROGTBL          ADD PROGRAM TO TABLE                         
PRGLOOP  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(6,R3),NBACTPRG    SKIP DUPLICATES                              
         BE    FX6                                                              
         CLI   0(R3),X'40'                                                      
         BNH   *+12                                                             
         LA    R3,6(R3)                                                         
         B     PRGLOOP                                                          
         MVC   0(6,R3),NBACTPRG                                                 
         BAS   RE,HEXREC                                                        
         BAS   RE,PRINTREC                                                      
*                                                                               
FX6      DS    0H                                                               
         CLI   DELETESW,C'Y'                                                    
         BNE   HOOKX                                                            
         MVI   NBUPUNIT,C'Y'       SET ON PUTREC SWITCH FOR NETIO               
         MVI   NBNOWRIT,C'Y'       SET ON WRITE SWITCH FOR NETIO                
*                                                                               
HOOKX    B     EXIT                                                             
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
* PROGRAM RECORDS DELETE THE ATI MKTNO=87                                       
*                 ADD        ATI MKTNO=88                                       
*                                                                               
FX30     DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         LA    R2,KEY                                                           
         USING NPGRECD,R2                                                       
FX32     XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'    ACTIVE KEY                                   
         MVC   NPGKAM,=X'B3'       FOOTE CONE                                   
         MVC   NPGKNET,=X'0057'                                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(5),SAVEKEY                                                   
         BNE   FX40                                                             
         B     FX35                                                             
*                                                                               
FXSEQ    MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEY,SAVEKEY         RESET ACTIVE KEY                             
         GOTO1 HIGH                READ HIGH TO REESTABLISH SEQ                 
         GOTO1 SEQ                                                              
         CLC   KEY(5),SAVEKEY                                                   
         BNE   FX40                                                             
         MVC   SAVEKEY,KEY                                                      
FX35     LA    R3,PROGTBL          TEST PROG AGAINST TABLE                      
FX35A    CLC   NPGKPROG,0(R3)                                                   
         BE    FXGET                                                            
         CLI   0(R3),X'FF'                                                      
         BE    FXSEQ                                                            
         CLI   0(R3),0                                                          
         BE    FXSEQ                                                            
         LA    R3,6(R3)            BUMP TBL                                     
         B     FX35A                                                            
*                                                                               
FXGET    MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         BAS   RE,DELFILE          DELETE RECORD                                
         BAS   RE,HEXREC                                                        
         L     R2,NBAIO            SET R2 TO NBAIO                              
         MVC   NPGKNET,=X'0058'    SET RECORD KEY TO ATI                        
         BAS   RE,ADDFILE          ADD NEW REC                                  
         BAS   RE,HEXREC                                                        
*                                                                               
*   NOW DO PASSIVE KEY                                                          
         LA    R2,KEY              RESET R2 TO KEY                              
         MVC   NBDTADSP,=H'24'                                                  
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         XC    KEY(11),KEY                                                      
         MVC   NPGKTYP,=X'0DA0'    PASSIVE KEY                                  
         MVC   NPGKAM,=X'B3'       NORTH CASTLE                                 
         MVC   NPGKNET,=X'0057'                                                 
         MVC   NPGKPROG(1),NPGRDAY                                              
         MVC   NPGKPROG+1(4),NPGTIME                                            
         MVC   NPGKPROG+5(1),NPGUNIQ                                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         BAS   RE,WRTDIR                                                        
*                                                                               
         MVC   NPGKNET,=X'0058'        SET ATI                                  
         NI    KEY+13,X'7F'        TURN OF DELETE BIT                           
         BAS   RE,ADDDIR                                                        
*                                                                               
*                                                                               
FX37     DS    0H                                                               
         B     FXSEQ               GET NEXT PROG REC                            
*                                                                               
FX40     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
DELFILE  NTR1                                                                   
         L     R1,NBAIO                                                         
         OI    15(R1),X'80'        DELETE RECORD                                
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOPUT                                                         
         NI    15(R1),X'7F'        TURN OF DELETE BITS FOR LATER ADD            
         OI    KEY+13,X'80'        DLELETE KEY                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR                                                        
DELFX    B     EXIT                                                             
*                                                                               
ADDFILE  NTR1                                                                   
         BAS   RE,PRINTREC                                                      
         BAS   RE,HEXREC                                                        
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOADD                                                         
ADDFX    B     EXIT                                                             
         EJECT                                                                  
* DATAMGR CALLS                                                                 
*                                                                               
WRTDIR   NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   DTMX                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         B     DTMX                                                             
*                                                                               
ADDDIR   NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   DTMX                                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',FILENAME,KEY,KEY                          
         B     DTMX                                                             
*                                                                               
IOGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,NBAIO,(0,DMWORK)         
         B     DTMX                                                             
*                                                                               
IOADD    NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   DTMX                                                             
         GOTO1 DATAMGR,DMCB,=C'ADDREC',FILENAME,NBAIO,NBAIO,(0,DMWORK)          
         B     DTMX                                                             
*                                                                               
IOPUT    NTR1                                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   DTMX                                                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,NBAIO,(0,DMWORK)         
         B     DTMX                                                             
*                                                                               
DTMX     B     EXIT                                                             
*                                                                               
HEXREC   NTR1                                                                   
         GOTO1 HEXOUT,DMCB,NBAIO,P,50                                           
         B     SPX                                                              
HEXKEY   NTR1                                                                   
         GOTO1 HEXOUT,DMCB,KEY,P,30                                             
         B     SPX                                                              
PRINTREC NTR1                                                                   
         L     R1,NBAIO                                                         
         MVC   P(100),0(R1)                                                     
         B     SPX                                                              
PRINTKEY NTR1                                                                   
         MVC   P(20),KEY                                                        
         B     SPX                                                              
*                                                                               
SPX      GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* SUB-ROUTINE TO DELETE ELMENT (R6 POINTS TO RECORD)                            
*                                                                               
DELELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'UNTFIL  '),(ELCODE,(R6)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* SUB-ROUTINE TO PUT ELEMENT (R4 POINTS TO ELEMENT)                             
*                                                                               
PUTELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFIL  '),(ELCODE,(R6)),(R4)                
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PROGTBL  DS    CL600                                                            
         DS    CL1                                                              
         SPACE 2                                                                
WORKD    DSECT                WORKING STORAGE                                   
RELO     DS    F                                                                
COUNT    DS    CL1                                                              
DELETESW DS    CL1                                                              
SAVEKEY  DS    CL15                                                             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE1D                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENPROG                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048NEWRIFX   05/01/02'                                      
         END                                                                    
