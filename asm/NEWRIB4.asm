*          DATA SET NEWRIB4    AT LEVEL 019 AS OF 07/11/02                      
*PHASE T320B4A,+0                                                               
         TITLE 'T320B4 - MODULE TO READ AOR AND TRAFFIC'                        
*******************************************************************             
*          ***    CALLED FROM NET WRITER  ***                                   
*                                                                               
*                                                                               
*   0-100     AOR KEYWORDS                                                      
*   101-200  TRAFFIC KEYWORDS                                                   
*                                                                               
*                                                                               
* - NOTE R6 RESERVED FOR GLOBALD                                                
* - NOTE R2,R3 RESERVED FOR DRIVER                                              
**********************************************************************          
T320B4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKLEN,T320B4,RA,R8,CLEAR=YES                                    
         LR    R7,RC                                                            
         USING WRKAREA,R7                                                       
         L     RC,0(R1)            *CALLING PROGRAMS RC                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,NDGLOBAL                                                      
         USING GLOBALD,R6                                                       
                                                                                
* LET THIS BE CALLED IN BHO FLAVOR - NEWRI82 SETS UP NETBLOCK                   
**       CLI   GLMODE,GLINPUT      IF INPUT                                     
**       BNE   *+12                                                             
**       CLI   NBMODE,NBPROCUN     GOT TO BE UNITS                              
**       BNE   EXIT                                                             
                                                                                
         L     R1,=A(MYIO)                                                      
         ST    R1,AMYIO                                                         
         ZIC   RF,GLARGS           * REQUESTED ROUTINE NUMBER                   
         SLL   RF,2                                                             
         B     BRANCHTB(RF)                                                     
                                                                                
BRANCHTB DS    0H                                                               
*                                 0-100 AOR RECS                                
         B     IAGYNM             0=IN AOR AGENCY NAME                          
         B     OAGYNM             1=OUT AOR AGY NAME                            
*                                                                               
         B     IAORAMT            2=IN AOR AMOUNT                               
         B     OAORAMT            3=OUT AOR AMOUNT                              
*                                                                               
         B     IAORBAS            4=IN AOR BASIS                                
         B     OAORBAS            5=OUT AOR BASIS                               
*                                                                               
         B     IEFFDT             6=IN AOR EFFECTIVE DATE                       
         B     OEFFDT             7=OUT AOR EFFECTIVE DATE                      
*                                                                               
         B     IINCA              8=IN AOR INCOME ACCOUNT CODE                  
         B     OINCA              9=OUT AOR INCOME ACCOUNT CODE                 
*                                                                               
         B     IPCT               10=IN AOR COMMISSION PCT                      
         B     OPCT               11=OUT AOR COMMISSION PCT                     
*                                                                               
         B     IRECPAY            12=IN AOR RECEIVABLE ACC CODE                 
         B     ORECPAY            13=OUT AOR RECEIVABLE ACC CODE                
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
IAGYNM   DS    0H                                                               
         BAS   RE,GETAOR                                                        
         BNE   EXIT                                                             
         BAS   RE,ELCOD2                                                        
         USING AORADREL,R4                                                      
         MVC   0(30,R3),AORLIN1                                                 
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
OAGYNM   DS    0H                                                               
         MVC   0(30,R3),0(R2)                                                   
*****    GOTO1 =V(PRNTBL),DMCB,=C'B42UT',0(R3),C'DUMP',10,=C'1D'                
         B     EXIT                                                             
*                                                                               
IAORAMT  DS    0H                                                               
OAORAMT  DS    0H                                                               
*                                                                               
IAORBAS  DS    0H                   AOR BASIS                                   
         BAS   RE,GETAOR                                                        
         BNE   IAORBASX                                                         
         BAS   RE,ELCOD3                                                        
         USING AORELEM,R4                                                       
         MVC   0(5,R3),=C'GROSS'                                                
         CLI   AORBAS,0                                                         
         BE    IAORBASX                                                         
         MVC   0(5,R3),=C'NET  '                                                
         TM    AORBAS,X'40'                                                     
         BO    IAORBASX                                                         
         MVC   0(5,R3),=C'COM  '          AGENCY COMMISSION                     
IAORBASX B     EXIT                                                             
         DROP  R4                                                               
OAORBAS  DS    0H                                                               
         MVC   0(5,R3),0(R2)                                                    
         B     EXIT                                                             
*                                                                               
IEFFDT   DS    0H                  EFFECTIVE DATE                               
         BAS   RE,GETAOR                                                        
         BNE   IEFFDTX                                                          
         BAS   RE,ELCOD3                                                        
         USING AORELEM,R4                                                       
         MVC   WORK(2),AOREFF                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(8,0(R3))                                   
IEFFDTX  B     EXIT                                                             
         DROP  R4                                                               
OEFFDT   DS    0H                                                               
         MVC   0(9,R3),0(R2)                                                    
         B     EXIT                                                             
*                                                                               
IINCA    DS    0H                                                               
         BAS   RE,GETAOR                                                        
         BNE   IINCAX                                                           
         BAS   RE,ELCOD3                                                        
         USING AORELEM,R4                                                       
         MVC   0(14,R3),AORCOMM                                                 
IINCAX   B     EXIT                                                             
*                                                                               
OINCA    DS    0H                                                               
         MVC   0(14,R3),0(R2)                                                   
         B     EXIT                                                             
*                                                                               
IPCT     DS    0H                                                               
         BAS   RE,GETAOR                                                        
         BNE   IPCTX                                                            
         BAS   RE,ELCOD3                                                        
         USING AORELEM,R4                                                       
         PRINT GEN                                                              
         EDIT  (B4,AORPCT),(8,0(R3)),4,FLOAT=-                                  
         PRINT NOGEN                                                            
IPCTX    B     EXIT                                                             
*                                                                               
OPCT     DS    0H                                                               
         MVC   0(8,R3),0(R2)                                                    
         B     EXIT                                                             
*                                                                               
IRECPAY  DS    0H                                                               
         BAS   RE,GETAOR                                                        
         BNE   IRECPAYX                                                         
         BAS   RE,ELCOD3                                                        
         USING AORELEM,R4                                                       
         MVC   0(14,R3),AORRCVBL                                                
IRECPAYX B     EXIT                                                             
*                                                                               
ORECPAY  DS    0H                                                               
         MVC   0(14,R3),0(R2)                                                   
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
* RETURNS R4 -> TO X'02' ELEM                                                   
ELCOD2   NTR1                                                                   
         L     R4,AMYIO                                                         
         MVI   MYELCODE,2                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1  REGS=(R4)                                                        
                                                                                
* RETURNS R4 -> TO X'03' ELEM                                                   
ELCOD3   NTR1                                                                   
         L     R4,AMYIO                                                         
         MVI   MYELCODE,3                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1  REGS=(R4)                                                        
*                                                                               
GETAOR   NTR1                            READ AOR RECORD                        
         LA    R4,MYKEY                                                         
         USING AORKEY,R4                                                        
         MVC   MYDATDSP,=H'24'                                                  
         XC    MYKEY,MYKEY                                                      
         MVC   AORKTYP,=X'0D45'                                                 
         MVC   AORKAGMD,NBACTAM                                                 
         MVC   AORKCLT,NBACTCLI                                                 
         MVC   AORKPRD,NDCURPRD          NDCURPRD SET IN NEWRI20                
         MVC   AORKEST+1(1),NBACTEST     1ST BYTE FOR FUTURE USE                
         MVC   AORKDPT,NBACTDP                                                  
         MVC   AORKSTYP,NBSTATYP   STATION TYPE FROM UNIT 02 ELEM               
         GOTO1 HISPOT                                                           
GTAOR5   CLC   MYSVKEY(13),MYKEY    EXACT MATCH?                                
         BE    GTAOR10                                                          
         CLC   MYSVKEY(8),MYKEY    ID/AGY/CLT/PROD                              
         BNE   NO                                                               
         CLC   AORKEST,=X'FFFF'    ALL EST ?                                    
         BE    *+14                                                             
         CLC   AORKEST+1(1),NBACTEST                                            
         BNE   NO                                                               
         CLI   AORKDPT,X'FF'       ALL DAYPARTS?                                
         BE    GTAOR10                                                          
         CLC   AORKDPT,NBACTDP                                                  
         BNE   NO                                                               
         CLC   AORKSTYP,MYKEY+11   STATION TYPE?                                
         BE    GTAOR10                                                          
GTAOR7   GOTO1 SEQSPOT                                                          
         B     GTAOR5                                                           
GTAOR10  L     R1,AMYIO                                                         
         CLC   0(13,R1),MYKEY      DO WE ALREADY HAVE THE RECORD                
         BE    YES                                                              
         GOTO1 MYGET                                                            
*                                                                               
YES      SR    RE,RE                                                            
NO       LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
* DATAMGR ROUTINES                                                              
*          DATA SET NENETIO    AT LEVEL 010 AS OF 08/28/00                      
*              DATAMGR INTERFACE                                                
         SPACE 3                                                                
HISPOT   NTR1                                                                   
         MVC   MYSVKEY,MYKEY                                                    
         MVC   COMAND,=CL8'DMRDHI'          HANDLE COMMANDS                     
         MVC   FILE(8),=C'SPTDIR  '                                             
         B     DIRALL                                                           
         SPACE 1                                                                
SEQSPOT  NTR1                                                                   
         MVC   MYSVKEY,MYKEY                                                    
         MVC   COMAND,=CL8'DMRSEQ'                                              
         B     DIRALL                                                           
         SPACE 1                                                                
DIRALL   DS    0H                                                               
         GOTO1 NBDM,DMCB,(0,COMAND),FILE,MYKEY,MYKEY,0                          
         B     EXIT                                                             
         SPACE 1                                                                
MYGET    NTR1                                                                   
         L     R2,AMYIO                                                         
         MVC   FILE(8),=C'SPTFILE '     FILE                                    
         MVC   MYDATDSP,=H'24'                                                  
         LA    R3,MYKEY+14                                                      
         GOTO1 NBDM,DMCB,(0,=C'GETREC'),FILE,(R3),(R2),MYDM                     
         B     EXIT                                                             
                                                                                
*                                                                               
         GETEL (R4),MYDATDSP,MYELCODE                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
MYIO     DS    CL3000                                                           
MYIO2    DS    CL3000                                                           
         EJECT                                                                  
*                                                                               
                                                                                
* - WORK AREA FOR RDBELEM                                                       
WRKAREA  DSECT                                                                  
MYDM     DS    CL96                                                             
AMYIO    DS    F                                                                
         DS    0D                                                               
MYDUB    DS    CL8                                                              
MYHALF   DS    H                                                                
MYDATDSP DS    H                                                                
MYELCODE DS    CL1                                                              
MYKEY    DS    CL30                                                             
MYSVKEY  DS    CL30                                                             
COMAND   DS    CL8                                                              
FILE     DS    CL8                                                              
*                                                                               
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
SCANBLK  DS    CL420                                                            
WRKLEN   EQU   *-WRKAREA                                                        
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPBVALD                                                        
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENAOR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEWRIB4   07/11/02'                                      
         END                                                                    
