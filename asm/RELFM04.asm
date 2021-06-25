*          DATA SET RELFM04    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T80404A,*                                                                
         TITLE 'T80404 - CLS/CTG/REPORT/ERR RECS'                               
*                                                                               
**********************************************************************          
*                                                                    *          
*- RELFM04 -- PHASE T80404                                           *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*  08/24/89  PJS  CHANGE PHASE CARD TO 'A' LEVEL                     *          
*                                                                    *          
*  OCT09/90 (MRR) --- REMOVE REPORT AND ERROR RECORDS                *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
T80404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80404                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         CLI   BREC,X'0D'                                                       
         BE    CLS                                                              
         CLI   BREC,X'0F'                                                       
         BE    CTG                                                              
         DC    H'0'                                                             
NUMERR   EQU   117                                                              
NOCLSERR EQU   122                                                              
         TITLE 'T80404 - CLASS RECORDS'                                         
CLS      CLI   BFMTSW,0                                                         
         BNE   CLSEDT                                                           
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RCLSNAME,R2),RCLSNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RCLSPROF                                                      
CLSFMT2  BAS   RE,NEXTUF                                                        
         BE    CLSFMTX                                                          
         MVC   8(1,R2),0(R4)                                                    
         LA    R4,1(R4)                                                         
         B     CLSFMT2                                                          
*                                                                               
CLSFMTX  B     EXXMOD                                                           
         EJECT                                                                  
CLSEDT   MVC   REC+34(2),=X'015C'                                               
         MVC   REC+27(2),=Y(126)                                                
         MVC   RCLSPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RCLSNAME,WORK                                                    
*                                                                               
         LA    R4,RCLSPROF                                                      
CLSEDT2  BAS   RE,NEXTUF                                                        
         BE    CLSEDTX                                                          
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     CLSEDT2                                                          
*                                                                               
CLSEDTX  B     FLFILE                                                           
         TITLE 'T80404 - CATEGORY RECORDS'                                      
CTG      CLI   BFMTSW,0                                                         
         BNE   CTGEDT                                                           
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RCTGNAME,R2),RCTGNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(2,R2),RCTGCLSS                                                 
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RCTGPROF                                                      
CTGFMT2  BAS   RE,NEXTUF                                                        
         BE    CTGFMTX                                                          
         MVC   8(1,R2),0(R4)                                                    
         LA    R4,1(R4)                                                         
         B     CTGFMT2                                                          
*                                                                               
CTGFMTX  B     EXXMOD                                                           
         EJECT                                                                  
CTGEDT   MVC   REC+34(2),=X'015E'                                               
         MVC   REC+27(2),=Y(128)                                                
         MVC   RCTGPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RCTGNAME,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RCTGCLSS,WORK                                                    
*                                                                               
         LA    R4,RCTGPROF                                                      
CTGEDT2  BAS   RE,NEXTUF                                                        
         BE    CTGEDTX                                                          
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     CTGEDT2                                                          
         SPACE 2                                                                
CTGEDTX  XC    KEY,KEY             VALIDATE CLASS                               
         MVI   KEY,X'0D'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RCTGCLSS                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    CTGEDTX2                                                         
* POSITION TO CLASS FIELD                                                       
         LA    R2,LFMLAST                                                       
         LA    R3,2                                                             
         BAS   RE,NEXTUF                                                        
         BCT   R3,*-4                                                           
         LA    R3,NOCLSERR                                                      
         B     ERROR                                                            
CTGEDTX2 B     FLFILE                                                           
         EJECT                                                                  
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
* CHANGE - READ REC THEN WRITE NEW                                              
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,XCREC            SWAP REC/REC2                                
         BAS   RE,PUTREC                                                        
         B     EXXMOD                                                           
*                                                                               
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
         B     EXXMOD                                                           
         EJECT                                                                  
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BCR   8,RE                                                             
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
         BR    RE                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
         SPACE 2                                                                
* SUBROUTINE TO MOVE 1000 BYTES TO R4 FROM R5                                   
MOVEREC  MVC   000(250,R4),000(R5)                                              
         MVC   250(250,R4),250(R5)                                              
         MVC   500(250,R4),500(R5)                                              
         MVC   750(250,R4),750(R5)                                              
         BR    RE                                                               
         SPACE 2                                                                
XCREC    LA    R0,4                                                             
         XC    0(250,R4),0(R5)                                                  
         XC    0(250,R5),0(R4)                                                  
         XC    0(250,R4),0(R5)                                                  
         LA    R4,250(R4)                                                       
         LA    R5,250(R5)                                                       
         BCT   R0,XCREC+4                                                       
         BR    RE                                                               
         SPACE 2                                                                
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
ZEROS    DC    30C'0'                                                           
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCLS                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCTG                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE RERPTREC                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REERRREC                                                       
         EJECT                                                                  
         ORG   REC                                                              
         ORG                                                                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RELFM04   05/01/02'                                      
         END                                                                    
