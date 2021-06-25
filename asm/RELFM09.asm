*          DATA SET RELFM09    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T80409A,*                                                                
         TITLE 'T80409 - TER/REPORT/ERR RECS'                                   
*                                                                               
**********************************************************************          
*                                                                    *          
*- RELFM09 -- PHASE T80409                                           *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*  15DEC95 (WSB) --- INITIAL ENTRY                                   *          
*                                                                    *          
*                                                                    *          
*                     ***  END TOMBSTONE  ***                        *          
**********************************************************************          
*                                                                               
T80409   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80409                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         CLI   BREC,X'3D'                                                       
         BE    TER                                                              
         DC    H'0'                                                             
         TITLE 'T80409 - TERRITORY RECORDS'                                     
TER      CLI   BFMTSW,0                                                         
         BNE   TEREDT                                                           
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RTERNAME,R2),RTERNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RTERPROF                                                      
TERFMT2  BAS   RE,NEXTUF                                                        
         BE    TERFMTX                                                          
         MVC   8(1,R2),0(R4)                                                    
         LA    R4,1(R4)                                                         
         B     TERFMT2                                                          
*                                                                               
TERFMTX  B     EXXMOD                                                           
         EJECT                                                                  
TEREDT   MVC   REC+34(2),=X'015C'                                               
         MVC   REC+27(2),=Y(126)                                                
         MVC   RTERPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RTERNAME,WORK                                                    
*                                                                               
         LA    R4,RTERPROF                                                      
TEREDT2  BAS   RE,NEXTUF                                                        
         BE    TEREDTX                                                          
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     TEREDT2                                                          
*                                                                               
TEREDTX  B     FLFILE                                                           
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
       ++INCLUDE REGENTER                                                       
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
**PAN#1  DC    CL21'023RELFM09   05/01/02'                                      
         END                                                                    
