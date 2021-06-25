*          DATA SET NEMEDAE    AT LEVEL 011 AS OF 04/14/03                      
*PHASE T31EAEA,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T31EAE - UPDATE SUBDAYUPART'                                    
         PRINT   GEN                                                            
************************************************************                    
* UPDATE SUBDAYPART FROM PACKAGE RECORD                    *                    
*                                                          *                    
************************************************************                    
T31EAE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**USUB**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         L     R5,ANETWS4          R5=ANETWS4=WORKING STORAGE                   
         USING WORKD,R5                                                         
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
***      ICM   R1,15,TWAMASTC                                                   
***      BZ    ENDMSTC                                                          
***      USING MCBLOCK,R1                                                       
***      L     R1,MCSSB                                                         
***      USING SSBD,R1                                                          
***      OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
***      DROP  R1                                                               
***ENDMSTC  DS    0H                                                            
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
*                                                                               
VK       DS    0H                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB                                                       
*                                                                               
         XC    WORK(20),WORK                                                    
         MVI   WORK,11                                                          
         MVI   WORK+5,3                                                         
         MVC   WORK+8(3),=C'POL'        PRODUCT                                 
         LA    R2,WORK                                                          
         NETGO NVPRDALL,DMCB                                                    
         MVC   WORK+8(3),=C'ALL'                                                
         LA    R2,WORK                   ESTIMATE                               
         NETGO NVEST,DMCB                                                       
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNET,DMCB,NETMKT                                                
*                                                                               
         LA    R2,SPLPRGH          PROGRAM REC                                  
         CLI   5(R2),0                                                          
         BE    ENDPROG                                                          
         MVC   NBSELPRG,SPLPRG                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKPROG,SPLPRG     PROGRAM                                      
         MVC   NPGKNET,NETMKT       MARKET NUMBER                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         BNE   INVERROR                                                         
ENDPROG  DS    0H                                                               
                                                                                
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    INVERROR                                                         
         MVC   TESTRUN,SPLTST                                                   
         CLI   SPLTST,C'Y'                                                      
         BE    VKEXIT                                                           
         CLI   SPLTST,C'N'                                                      
         BNE   INVERROR                                                         
         B     VKEXIT                                                           
*                                                                               
INVERROR MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
PR       DS    0H                                                               
*  READ UNITS/UPDATE RECORD IF SUBDAYAPRT CHANGES                               
*                                                                               
         MVI   NBSEQ,C'Q'     PROGRAM ORDER READ X'84' KEY                      
         MVI   NBDATA,C'U'                                                      
         MVI   NBACTOPT,0                                                       
         MVI   NBESTOPT,0                                                       
         LA    R1,NETIOHK                                                       
         ST    R1,NBHOOK                                                        
VR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   VR10                                                             
         MVC   P+1(13),=C'UNITS UPDATED'                                        
         EDIT  (B4,COUNTER),(6,P+15)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
NETIOHK  NTR1                                                                   
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
         CLI   NBMODE,NBPROCUN                                                  
         BNE   NETHKX                                                           
*                                                                               
*                                                                               
         L     R6,NBAIO            FOR GETEL                                    
         USING NURECD,R2                                                        
         MVI   ELCODE,2            IS THERE 02 ELEM                             
         BAS   RE,GETEL                                                         
         BNE   NETHKX                                                           
         USING NUSDRD,R2                                                        
*                                                                               
         LA    R1,STATBL                                                        
         CLC   NBACTNET(10),0(R1)     NETWORK+PROGRAM?                          
         BE    NT14                                                             
         BAS   RE,GETPROG       NEW PROGRAM/GET RECORD                          
*                               RESET UNIT POINTER                              
         MVI   NBFUNCT,NBFRDHI                                                  
*                                                                               
NT14     LA    R1,10(R1)        JUMP OVER NETWRK+PROG                           
*                                                                               
NT15     CLC   NBACTDAT,0(R1)   REC END DATE>PROG END DATE?                     
         BNH   NT16             NO                                              
         LA    R1,5(R1)         BUMP TO NXT PROG ENDDATE                        
         CLI   0(R5),0                                                          
         BNE   NT15                                                             
         DC    H'0'                                                             
NT16     CLC   2(3,R1),NUSDPT  SUB DAYPARTS MATCH?                              
         BE    NETHKX            NO REASON TO UPDATE                            
         MVC   NUSDPT,2(R1)    UPDATE SUBDAYPART                                
         L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         GOTO1 =V(PRNT),DMCB,=C'UPDATE',NBKEY,C'DUMP',20,=C'1D'                 
         CLI   TESTRUN,C'N'        TEST RUN?                                    
         BNE   NETHKX                                                           
         MVI   NBNOWRIT,C'Y'                                                    
         MVI   NBUPUNIT,C'Y'                                                    
NETHKX   XIT1                                                                   
                                                                                
                                                                                
         EJECT                                                                  
* READ PROGRAM RECS/FILLTABLE WITH PROGRAM/END DATE /SUBDAYPART                 
GETPROG  NTR1                                                                   
         XC    KEY,KEY             NOW READ PROGRAM RECORD                      
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKPROG,NBSELPRG   PROGRAM                                      
         MVC   NPGKNET,NBMARKET                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PROGRAM MUST EXIST                           
         LA    R3,KEY+14                                                        
         L     R2,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFIL',(R3),(R2),DMWRKR          
         MVC   NBDTADSP,=H'24'                                                  
         MVI   ELCODE,X'3'                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL03,R2                                                       
         LA    R4,STATBL                                                        
         MVC   0(10,R4),KEY+4    UNITS NETWORK+PROGRAM                          
         MVC   10(2,R4),KEY+11        END DATE                                  
         MVC   12(3,R4),NPGSDPT  PROGRAM RECORDS SUB-DAYPART                    
         LA    R4,15(R4)           POINT TO NXT DATE/SUBDPT                     
*                                                                               
PROGSEQ  GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEYSAVE(11),KEY                                                  
         BNE   PROGX                                                            
         LA    R3,KEY+14                                                        
         L     R2,=A(MYIO)                                                      
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'SPTFIL',(R3),(R2),DMWRKR          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R4),KEY+11     PROGRAM END DATE                              
         MVC   2(3,R4),NPGSDPT     SUBDAYPART                                   
         LA    R4,5(R4)                                                         
         B     PROGSEQ                                                          
PROGX    MVC   NBDTADSP,=H'27'  RESET UNIT DATADSP                              
         DROP  R2,R4                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'NETWORK'                                                
         MVC   H4+10(4),SPLCLI                                                  
         MVC   H3+15(20),SPLNET                                                 
         MVC   H3+10(3),SPLPRG                                                  
*                                                                               
         MVC   H10+20(3),=C'CLT'                                                
         MVC   H11+20(3),=10C'-'                                                
         MVC   H10+25(3),=C'PKG'                                                
         MVC   H11+25(3),=10C'-'                                                
         MVC   H10+30(4),=C'NTWK'                                               
         MVC   H11+30(4),=10C'-'                                                
         MVC   H10+36(3),=C'EST'                                                
         MVC   H11+36(3),=10C'-'                                                
         MVC   H10+41(4),=C'PROG'                                               
         MVC   H11+41(4),=10C'-'                                                
HDX      B     EXIT                 (XIT1)                                      
*                                                                               
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'UPDATE SUBDAYPAR'                                        
         SSPEC H2,52,C'----------------'                                        
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,RUN                                                       
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
*                                                                               
STATBL   DS    CL200     PROGRAM/NETWORK(CL10)+SUBDPT(CL3)+ENDAT(CL2)           
         DC    X'FF'     ......... EOF                                          
*                                                                               
MYIO     DS    CL2000                                                           
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
DMWRKR   DS    CL96                                                             
COUNTER  DS    F                                                                
TESTRUN  DS    CL1                                                              
NETMKT   DS    CL2                                                              
*                                                                               
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDAFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011NEMEDAE   04/14/03'                                      
         END                                                                    
