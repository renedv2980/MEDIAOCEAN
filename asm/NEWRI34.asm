*          DATA SET NEWRI34    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T32034A,+0                                                               
         TITLE 'T32034 - CPM SEED REPORT'                                       
T32034   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE34**,RR=R2                                                 
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING T32034,RB,R5       NOTE R5 = 2ND BASE REGISTER                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3          ANETWS3=WORKING STORAGE                      
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         MVC   AMYIO,ANETWS1       ANETWS1+ANETWS2 = MY I/O AREA                
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   FTERMFLG,0          REQUIRED FIELDS                              
*                                                                               
         LA    R2,SPLCLIH          *CLIENT                                      
         NETGO NVGETFLD,DMCB                                                    
         NETGO NVCLI,DMCB                                                       
*                                                                               
         XC    MYWORK(20),MYWORK   *PRODUCT (FUDGE)                             
         MVI   MYWORK,20                                                        
         MVI   MYWORK+5,3                                                       
         MVC   MYWORK+8(3),=C'ALL'                                              
         LA    R2,MYWORK                                                        
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         LA    R2,SPLESTH           *ESTIMATE                                   
         NETGO NVGETFLD,DMCB                                                    
         ZIC   R3,5(R2)                                                         
         LA    R4,SPLEST                                                        
         CLI   0(R4),C','            ARE THERE MULTIPLE ESTS REQUESTED          
         BE    SCAN0                 YES                                        
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         NETGO NVEST,DMCB,0,NDDEMBLK  VALIDATES/SETS SYSTEM FILTER              
         B     ED10                                                             
*                                                                               
*-MULTIPLE ESTS INPUT / SCAN AND SAVE ESTIMATES                                 
SCAN0    DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,MYWORK)                                    
         MVI   5(R2),0             SET INPUT LENGTH TO FUDGE                    
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         LA    R4,ESTSAVE          ESTIMATE SAVE AREA                           
         LA    R3,MYWORK                                                        
SCAN5    CLI   2(R3),X'80'         1ST HALF OF FIELD = NUMBERIC                 
         BNE   EDINV                                                            
         CLI   5(R2),0             IS 1ST EST SET TO SCREEN                     
         BNE   *+8                 YES                                          
         BAS   RE,SETDATA          NO/SET FIRST EST TO SCREEN REQ FIELD         
         MVC   0(1,R4),7(R3)       SAVE EST (BINARY)                            
         LA    R4,1(R4)            BUMP ESTSAVE AREA                            
         LA    R3,32(R3)                                                        
         BCT   R0,SCAN5                                                         
         SPACE                                                                  
*                                                                               
ED05     NETGO NVEST,DMCB,0,NDDEMBLK  VALIDATE FIRST ESTIMATE                   
         MVI   NBSELEST,0          BUT APPLICATION NEEDS TO FILTER              
         MVI   NBSELESE,0                                                       
         XC    NBSELEFL,NBSELEFL   CLEAR EST FILTER                             
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
* - ONLY COME HERE IF MULTIPLE ESTS OR PACKAGES REQUESTED                       
* - SETS FIRST VALID EST/PAKG FROM SCREEN FIELD                                 
* - BACK TO SCREEN FIELD / THIS EST/PAKG IS VALIDATED                           
* - IF EST, ITS TARGET DEMO USED FOR PACK CPM COMPUTATION                       
*                                                                               
* - R3 POINTS TO SCANNER OUT AREA                                               
* - R2 SREEN FIELD HEADER                                                       
         SPACE                                                                  
SETDATA  NTR1                                                                   
         ZIC   R1,5(R2)            CLEAR SCREEN FIELD                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVC   5(1,R2),0(R3)       SET LENGTH OF DATA                           
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R3)       MOVE DATA TO SCREEN FIELD                   
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
*                                                                               
ED10     LA    R2,SPLNETH           *NETWORK                                    
         NETGO NVGETFLD,DMCB                                                    
         NETGO NVNET,DMCB                                                       
*                                                                               
         LA    R2,SPLPAKH           *PACKAGE                                    
         MVI   NBDATA,C'P'                                                      
         NETGO NVGETFLD,DMCB                                                    
         ZIC   R3,5(R2)                                                         
         LA    R4,SPLPAK                                                        
         CLI   0(R4),C','            ARE THERE MULTIPLE PAKS REQUESTED          
         BE    PAK00                 YES                                        
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         NETGO NVPAKALL,DMCB         NO                                         
         CLI   NBMODE,NBPROCPK                                                  
         BNE   EDINV                                                            
         B     ED11                                                             
         SPACE                                                                  
*                                                                               
PAK00    DS    0H                   NEED TO SCAN AND SAVE PAKS                  
         LA    R2,SPLPAKH                                                       
         GOTO1 SCANNER,DMCB,(R2),(10,MYWORK)                                    
         MVI   5(R2),0             SET INPUT LENGTH TO FUDGE                    
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         LA    R4,PAKSAVE          PACKAGE SAVE AREA                            
         LA    R3,MYWORK                                                        
PAK10    CLI   2(R3),X'80'         1ST HALF OF FIELD = NUMBERIC                 
         BNE   EDINV                                                            
         CLI   5(R2),0             IS 1ST PKG SET                               
         BNE   *+8                                                              
         BAS   RE,SETDATA          NO/SET FIRST PAK TO SCREEN REQ FIELD         
         MVC   0(1,R4),7(R3)       SAVE PAK (BINARY)                            
         LA    R4,1(R4)            BUMP PAKSAVE AREA                            
         LA    R3,32(R3)                                                        
         BCT   R0,PAK10                                                         
*                                                                               
PAK20    DS    0H                                                               
         NETGO NVPAK,DMCB         VALIDATE AT LEAST ONE PKG                     
         CLI   NBMODE,NBPROCPK                                                  
         BNE   EDINV                                                            
         MVI   NBSELPAK,0          BUT APPLICATION MUST FILTER                  
*                                                                               
         EJECT                                                                  
*                                                                               
ED11     LA    R2,SPLDPTH           *DAYPART                                    
         MVI   FTERMFLG,1            (OPTIONAL)                                 
         NETGO NVDPT,DMCB,SPLDPTN                                               
*                                                                               
         LA    R2,SPLCSTH           *COST (ACTUAL/PACKAGE)                      
         MVI   COSTYP,C'A'                                                      
         CLI   5(R2),0                                                          
         BE    ED12                                                             
         MVC   COSTYP,SPLCST                                                    
         CLI   COSTYP,C'P'                                                      
         BE    ED12                                                             
         CLI   COSTYP,C'A'                                                      
         BNE   EDINV                                                            
*                                                                               
ED12     LA    R2,SPLPDMH           *PKG GUARANTEE                              
         MVI   DOPAKG,0                                                         
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED14                                                             
         CLI   FLD,C'Y'                                                         
         BNE   ED13                                                             
         MVI   DOPAKG,C'Y'                                                      
         B     *+12                                                             
ED13     CLI   FLD,C'N'                                                         
         BNE   EDINV                                                            
*                                                                               
         LA    R2,SPLPCPH         *PACKAGE CPM                                  
         CLI   5(R2),0            (ONLY GETS HERE IF PKG DEMO ENTERED)          
         BE    EDMIS                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,SPLPCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
         MVC   TPAKCPM,4(R1)        SET TARGET CPM                              
*                                                                               
         EJECT                                                                  
*                                                                               
ED14     LA    R2,SPLDEMH            *DEMO GUARANTEE                            
         MVI   DODEMG,C'Y'                                                      
         NETGO NVGETFLD,DMCB                                                    
         BNZ   ED20                                                             
         CLI   DOPAKG,C'Y'         ..IF NO DEMO GUA NEED PAK GUA                
         BE    ED15                                                             
         LA    R2,SPLPDMH          ..POINT CURSOR TO PKG FIELD                  
         BNE   EDMIS               ..NO PAG GUA = ERROR                         
ED15     MVI   DODEMG,0                                                         
         XC    SPLDCP,SPLDCP       CLEAR DEMO CPM FIELD IN CASE                 
         OI    SPLDCPH+6,X'80'                                                  
         B     ED40                                                             
         SPACE                                                                  
ED20     DS    0H                           VALIDATE DEMO GUARANTEE             
         GOTO1 NBCALLOV,DMCB,0,X'D9000AD9'           (DEMOVAL)                  
         L     RF,DMCB                                                          
         L     R1,ANETWS1                                                       
         USING DBLOCK,R1                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'T'                                                    
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'S'                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,(1,(R2)),(1,WORK)                                      
         MVC   TDEMVAL,WORK        SAVE 3 BYTE DEMO CODE                        
*                                                                               
         DS    0H                                                               
ED30     LA    R2,SPLDCPH             *TARGET CPM FOR DEMO                      
         CLI   5(R2),0                                                          
         BE    EDMIS                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,SPLDCP),(R3)                                     
         CLI   0(R1),0                                                          
         BNE   EDINV                                                            
         MVC   TDEMCPM,4(R1)             SAVE TARGET CPM FOR DEMO               
*                                                                               
ED40     MVI   UPDATE,0                                                         
         LA    R2,SPLTSTH         *TEST RUN                                     
         CLI   5(R2),0                                                          
         BE    EDMIS                                                            
         CLI   SPLTST,C'N'         NO                                           
         BE    EDTX                                                             
         MVI   UPDATE,C'Y'         MARK UNITS                                   
         CLI   SPLTST,C'Y'                                                      
         BNE   EDINV                                                            
*                                                                               
EDTX     DS    0H                                                               
         SPACE                                                                  
EDTXX    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
EDMIS    DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE CPMWORKD                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDDD                                                       
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049NEWRI34   05/01/02'                                      
         END                                                                    
