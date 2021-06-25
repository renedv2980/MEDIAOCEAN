*          DATA SET ACLNK02    AT LEVEL 004 AS OF 05/28/09                      
*PHASE T61F02A                                                                  
ACLNK02  TITLE '- ACCOUNTING SYSTEM SERVER SUPPORT ROUTINES 2'                  
ACLNK02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AL02**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,ROU2RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ROUTINE NOT DEFINED                          
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(GETCPY-ACLNK02),AL2(0)                                       
         DC    AL2(GETLDG-ACLNK02),AL2(GLWORKL)                                 
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* GET COMPANY RECORD                                                  *         
***********************************************************************         
                                                                                
GETCPY   J     *+12                                                             
         DC    C'*GETCPY*'                                                      
         LR    RB,RF                                                            
         USING GETCPY,RB                                                        
         LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         L     RF,ALP                                                           
         MVC   CPYKCPY,LP_AGYB-LP_D(RF)                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOACCDIR+#CPYREC'                        
         JNE   EXIT                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOACCMST+#CPYREC'                       
         JNE   EXIT                                                             
                                                                                
         L     R2,ACPYREC          ESTABLISH FISCAL START FOR SOFDAT            
         LA    R2,CPYRFST                                                       
         USING CPYELD,R2                                                        
         SR    R0,R0                                                            
GETCPY02 CLI   CPYEL,0                                                          
         JE    EXITN                                                            
         CLI   CPYEL,CPYELQ                                                     
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R2,R0                                                            
         B     GETCPY02                                                         
         L     RF,ALP                                                           
         MVI   LP_SOFSY-LP_D(RF),ACCSYSQ                                        
         MVC   LP_SOFSF-LP_D(,RF),CPYSFST                                       
         J     EXITY                                                            
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET LEDGER RECORD                                                   *         
* NTRY - R1=A(PARAMETER LIST AS FOLLOWS):-                            *         
*        P1=A(UNIT/LEDGER CODE)                                       *         
*        P2=A(OUTPUT AREA)                                            *         
***********************************************************************         
                                                                                
GETLDG   J     *+12                                                             
         DC    C'*GETLDG*'                                                      
         LR    RB,RF                                                            
         USING GETLDG,RB                                                        
         USING GLWORKD,RC          RC=A(LOCAL W/S)                              
         MVC   GLIOSAVE,IOVALUES   SAVE CURRENT I/O VALUES                      
         LM    R3,R4,0(R1)                                                      
         USING LDGTABD,R4          R4=A(OUTPUT AREA)                            
         XC    LDGTABD(LDGTABL2),LDGTABD                                        
         MVC   LDGTUL,0(R3)        SET UNIT/LEDGER IN OUTPUT AREA               
         LA    R2,IOKEY                                                         
         USING LDGRECD,R2          R2=A(LEDGER KEY)                             
         MVC   LDGKEY,SPACES                                                    
         L     RF,ALP                                                           
         MVC   LDGKCPY,LP_AGYB-LP_D(RF)                                         
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),LDGTUL                              
         LA    R0,GLIOAREA                                                      
         ST    R0,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOACCDIR'                                
         JNE   GETLDGN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOACCMST'                               
         JNE   GETLDGN                                                          
                                                                                
         L     R2,IOADDR           R2=A(LEDGER RECORD)                          
         LA    R1,LDGRFST                                                       
         USING LDGELD,R1                                                        
         SR    R0,R0                                                            
GETLDG02 CLI   LDGEL,0             TEST END OF RECORD                           
         BE    GETLDGY                                                          
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BE    GETLDG06                                                         
         CLI   LDGEL,ACLELQ        TEST ACCOUNT LENGTHS ELEMENT                 
         BE    GETLDG08                                                         
         CLI   LDGEL,RSTELQ        TEST ACCOUNT STATUS ELEMENT                  
         BE    GETLDG10                                                         
*        CLI   LDGEL,APRELQ        TEST ACCOUNT EQUIV ELEMENT                   
*        BE    GETLDG20                                                         
GETLDG04 IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         B     GETLDG02                                                         
                                                                                
GETLDG06 MVC   LDGTLIKE,LDGLIKE                                                 
         MVC   LDGTOFFP,LDGOPOS                                                 
         MVC   LDGTCLOS,LDGCLOS                                                 
         CLI   LDGLN,LDGSTAT2-LDGELD                                            
         BL    *+10                                                             
         MVC   LDGTSTA2,LDGSTAT2                                                
         MVC   LDGTDDL,LDGDPOS     EXTRACT DEPARTMENT DISPLACEMENT              
         CLC   LDGTUL,CCOSTLDG     TEST COSTING CLIENT LEDGER                   
         BNE   GETLDG04                                                         
         MVC   LDGTDDL,LDGCPOS     YES - EXTRACT CLIENT DISPLACEMENT            
         B     GETLDG04                                                         
                                                                                
         USING ACLELD,R1                                                        
GETLDG08 MVC   LDGTLVA,ACLVLEN+(L'ACLVALS*0)                                    
         MVC   LDGTLVB,ACLVLEN+(L'ACLVALS*1)                                    
         MVC   LDGTLVC,ACLVLEN+(L'ACLVALS*2)                                    
         MVC   LDGTLVD,ACLVLEN+(L'ACLVALS*3)                                    
         MVC   LDGTLVAD,ACLVDESC+(L'ACLVALS*0)                                  
         MVC   LDGTLVBD,ACLVDESC+(L'ACLVALS*1)                                  
         MVC   LDGTLVCD,ACLVDESC+(L'ACLVALS*2)                                  
         MVC   LDGTLVDD,ACLVDESC+(L'ACLVALS*3)                                  
         B     GETLDG04                                                         
                                                                                
         USING RSTELD,R1                                                        
GETLDG10 MVC   LDGTSEC,RSTSECY+1                                                
         B     GETLDG04                                                         
                                                                                
*        USING APRELD,R1                                                        
*ETLDG20 LA    RE,LDGTLV1E                                                      
*        SR    RF,RF                                                            
*        IC    RF,APRSEQ                                                        
*        MHI   RF,L'APRDESC                                                     
*        AR    RE,RF                                                            
*        MVC   0(L'APRDESC,RE),APRDESC                                          
*        B     GETLDG04                                                         
*                                                                               
GETLDGY  SR    R0,R0               SET CC TO EQUAL IF OKAY                      
         J     GETLDGX                                                          
                                                                                
GETLDGN  LHI   R0,1                SET CC TO NOT EQUAL ON ERROR                 
                                                                                
GETLDGX  MVC   IOVALUES(IOVALUEL),GLIOSAVE                                      
         LTR   R0,R0               SET CONDITION CODE                           
         J     EXIT                                                             
         DROP  R2,R4,RB                                                         
                                                                                
         LTORG                                                                  
                                                                                
GLWORKD  DSECT                                                                  
GLIOSAVE DS    XL(IOVALUEL)        SAVED I/O VALUES                             
GLIOAREA DS    XL(IOLENQ)          I/O AREA FOR LEDGER RECORD READ              
GLWORKL  EQU   *-GLWORKD                                                        
                                                                                
ACLNK02  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
                                                                                
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACLNK02   05/28/09'                                      
         END                                                                    
