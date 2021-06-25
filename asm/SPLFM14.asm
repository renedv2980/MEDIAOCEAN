*          DATA SET SPLFM14    AT LEVEL 027 AS OF 05/01/02                      
*PHASE T21914A,+0,NOAUTO                                                        
         TITLE 'SPLFM14 - PRODUCT GROUP DEFINITIONS'                            
T21914   CSECT                                                                  
         NMOD1 0,T21914                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PRGRECD,R8                                                       
*                                                                               
         CLI   SVFMTSW,0           TEST FMT OR EDT                              
         BE    FMT                                                              
         B     EDT                                                              
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         LA    R2,PGDBK1H                                                       
         LA    R4,PRGBK1                                                        
         BAS   R9,FMTBK                                                         
*                                                                               
         LA    R2,PGDBK2H                                                       
         LA    R4,PRGBK2                                                        
         BAS   R9,FMTBK                                                         
*                                                                               
*NOP*    LA    R2,PGDBK3H                                                       
*NOP*    LA    R4,PRGBK3                                                        
*NOP*    BAS   R9,FMTBK                                                         
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
         MVC   SVBKLNS+2(1),PRGBK3LN                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PRGBK1LN                                                      
         SR    RE,RE                                                            
         IC    RE,PRGBK2LN                                                      
         AR    R0,RE                                                            
         IC    RE,PRGBK3LN                                                      
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL DIGITS                            
*                                                                               
         B     FMT4                                                             
         SPACE 2                                                                
FMTBK    DS    0H                                                               
         FOUT  (R2),0(R4),12       BREAK NAME                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   12(R4),0                                                         
         BE    FMTBKX                                                           
         MVC   BYTE,12(R4)                                                      
         OI    BYTE,X'F0'                                                       
         FOUT  (R2),BYTE,1                                                      
FMTBKX   OI    4(R2),X'20'                                                      
*                                                                               
         BR    R9                                                               
         EJECT                                                                  
* CLEAR DISPLAY LINES WITH DATA                                                 
*                                                                               
FMT4     DS    0H                                                               
         LA    R2,PGDLISTH                                                      
FMT4A    CLI   0(R2),80                                                         
         BNE   FMT4X                                                            
                                                                                
         OC    8(72,R2),8(R2)                                                   
         BZ    FMT4B                                                            
         XC    8(72,R2),8(R2)                                                   
         FOUT  (R2)                                                             
FMT4B    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FMT4A                                                            
*                                                                               
* DISPLAY PRODUCT GROUP NAMES                                                   
*                                                                               
FMT4X    LA    R2,PGDLISTH                                                      
         XC    HALF,HALF           CLEAR DSPL                                   
* READ HI FOR GRP DEF'N REC                                                     
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
*                                                                               
FMT6     GOTO1 SEQ                                                              
*                                                                               
FMT8     CLC   KEY(PRGKGRP-PRGKEY),KEYSAVE  TEST SAME THRU GRP ID               
         BNE   FMTX                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
*                                                                               
         LH    R4,HALF                                                          
         LA    R4,8(R4,R2)         POINT TO DISPLAY POSITION                    
         MVC   0(1,R4),PRGKID                                                   
         UNPK  DUB,PRGKGRP(3)                                                   
         IC    RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),DUB+3 ** EXECUTED **                                     
         MVC   6(24,R4),PRGNAM1                                                 
         CLI   SVBKLNS+1,0                                                      
         BE    FMT9                                                             
         MVC   6(24,R4),PRGNAM2                                                 
         CLI   SVBKLNS+2,0                                                      
         BE    FMT9                                                             
         MVC   6(24,R4),PRGNAM3                                                 
*                                                                               
FMT9     DS    0H                                                               
         FOUT  (R2)                                                             
         SPACE 2                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R1,PGDLLSTH                                                      
         CR    R2,R1               END OF LIST                                  
         BNH   FMT6                                                             
*                                                                               
         OC    HALF,HALF                                                        
         BNZ   FMTX                                                             
*                                                                               
         LA    R0,36                                                            
         STH   R0,HALF                                                          
         LA    R2,PGDLISTH                                                      
         B     FMT6                                                             
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   EDT2                                                             
*                                                                               
* COUNT NUMBER OF PRDGRPS ON FILE                                               
         ZAP   HALF,=P'0'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY        X'0D01'/A-M/CLT                              
         GOTO1 HIGH                                                             
         B     EDT1B                                                            
*                                                                               
EDT1A   MVC   KEY+6(2),=X'FFFF'                                                 
         GOTO1 HIGH                                                             
*                                                                               
EDT1B    CLC   KEY(5),SVKEY                                                     
         BNE   EDT1X                                                            
         AP    HALF,=P'1'                                                       
         CP    HALF,=P'4'          LIMIT OF 5 SCHEMES                           
         BNH   EDT1A                                                            
         MVI   ERRCD,GRPOVFLW                                                   
         B     LFMERR                                                           
EDT1X    MVC   PRGKEY,SVKEY                                                     
         MVC   PRGLEN,=H'65'                                                    
         MVC   PRGAGYA,AGYALPHA                                                 
         MVI   PRGEL,X'01'                                                      
         MVI   PRGEL+1,41                                                       
         B     EDT4                                                             
         SPACE 2                                                                
EDT2     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         EJECT                                                                  
EDT4     LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         LA    R2,PGDBK1H                                                       
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         LA    R4,PRGBK1                                                        
         BAS   R9,EDTBK                                                         
         MVI   ERRCD,INVERR                                                     
         CLI   PRGBK1LN,0                                                       
         BE    LFMERR                                                           
*                                                                               
         LA    R2,PGDBK2H                                                       
         LA    R4,PRGBK2                                                        
         BAS   R9,EDTBK                                                         
         B     EDT4X               ***** NOP *****                              
*                                                                               
*NOP*    LA    R2,PGDBK3H                                                       
*NOP*    LA    R4,PRGBK3                                                        
*NOP*    BAS   R9,EDTBK                                                         
*                                                                               
EDT4X    SR    RE,RE                                                            
         IC    RE,PRGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         IC    R0,PRGBK3LN                                                      
         AR    RE,R0                                                            
         MVI   ERRCD,GRPLNERR      TOO MANY DIGITS                              
         LA    R2,PGDBK1H                                                       
         CH    RE,=H'3'                                                         
         BH    LFMERR                                                           
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT6                                                             
*                                                                               
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDRESS                            
         GOTO1 CNADDSPT                                                         
         B     EDTX                                                             
*                                                                               
EDT6     GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
*                                                                               
EDTX     B     FMT                 GO RE-DISPLAY                                
         EJECT                                                                  
         SPACE 2                                                                
EDTBK    MVC   BYTE,5(R2)          SAVE INPUT LENGTH                            
         GOTO1 MOVE                                                             
         MVC   0(12,R4),WORK                                                    
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   ERRCD,NOCHGERR                                                   
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),X'20'         MAY NOT CHANGE BREAK LENGTHS                 
         BZ    LFMERR                                                           
*                                                                               
         CLI   BYTE,0              TEST DESC INPUT                              
         BNE   EDTBK2              YES                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0             NO DESC SO SHOULD NOT ENTER LEN              
         BNE   LFMERR                                                           
         BR    R9                                                               
*                                                                               
EDTBK2   MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'0'                                                       
         BNH   LFMERR                                                           
         CLI   8(R2),C'3'                                                       
         BH    LFMERR                                                           
         MVC   12(1,R4),8(R2)                                                   
         NI    12(R4),X'0F'                                                     
*                                                                               
         OI    4(R2),X'20'         NOW SET VALID                                
         BR    R9                                                               
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMF4                                                                        
       ++INCLUDE SPLFMF4D                                                       
         EJECT                                                                  
*SPGENPRG                                                                       
       ++INCLUDE SPGENPRG                                                       
 END                                                                            
