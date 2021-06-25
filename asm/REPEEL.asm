*          DATA SET REPEEL     AT LEVEL 016 AS OF 08/31/00                      
*          DATA SET REPEEL     AT LEVEL 015 AS OF 10/21/88                      
*PHASE REPEELA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
         TITLE 'REPEEL - PEEL DUMP DATA'                                        
REPEEL   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,REPEEL,VREGSAVE                                                
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING REPEEL+4096,RC                                                   
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
PEELREP  DC    CL2'TO'                                                          
         EJECT                                                                  
INIT2    DS    0H                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'DUMP PEEL PROGRAM'                               
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RSPARE                                                        
         GET   RECVIN,(0)                                                       
         B     IN4                 ******************                           
*                                                                               
         CLI   RFILTY,X'82'        TEST REPFILE                                 
         BNE   IN2                                                              
*                                                                               
         LA    R3,TABLE                                                         
IN3      CLC   RKEY,0(R3)          LOOK FOR RECORD IN TABLE                     
*********BE    IN4                                                              
         BE    IN8                 ***************                              
         LA    R3,27(R3)           NEXT ENTRY                                   
         CLI   0(R3),X'FF'         TEST END OF TABLE                            
         BE    IN2                 YES -- GET NEXT RECOVERY RECORD              
         B     IN3                                                              
*                                                                               
IN4      LA    R4,PUTTABLE                                                      
IN6      OC    0(2,R4),0(R4)       TEST END OF PUTTABLE                         
         BZ    IN7                 YES                                          
*                                                                               
         L     RF,APUTTBLX                                                      
         CR    R4,RF                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RKEY,4(R4)                                                       
         BE    IN7                                                              
         LA    R4,1050(R4)                                                      
         B     IN6                                                              
*                                                                               
IN7      XCEF  (R4),1050                                                        
         SR    R5,R5                                                            
         ICM   R5,3,RKEY+27                                                     
         LA    R5,4(R5)                                                         
         STCM  R5,3,0(R4)                                                       
         SH    R5,=H'4'                                                         
         LA    R4,4(R4)                                                         
         LR    RF,R5                                                            
         LA    RE,RKEY                                                          
         MVCL  R4,RE               PUT RECORD IN TABLE                          
         B     IN2                                                              
*                                                                               
IN8      LA    R0,RSPARE                                                        
         PUT   RECVOUT,(R0)                                                     
         AP    COUNT,=P'1'                                                      
         B     IN2                                                              
*                                                                               
ENDIN    LA    R4,PUTTABLE                                                      
ENDIN5   PUT   RECVOUT,(R4)                                                     
         LA    R4,1050(R4)                                                      
         OC    0(2,R4),0(R4)       TEST END OF PUTTABLE                         
         BZ    *+16                YES                                          
         L     RF,APUTTBLX                                                      
         CR    R4,RF                                                            
         BL    ENDIN5                                                           
         DC    H'0'                                                             
*                                                                               
         CLOSE (RECVIN,)                                                        
         B     EOJ                 *****************                            
         EJECT                                                                  
EOJ      MVC   P(21),=C'RECOVERY RECORDS OUT='                                  
         OI    COUNT+3,X'0F'                                                    
         UNPK  P+23(6),COUNT                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLOSE (RECVOUT,)                                                       
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*RECVIN  DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4004,                      
*              BLKSIZE=32760,MACRF=GM,EODAD=ENDIN                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=ENDIN             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,MACRF=PM,LRECL=1100,   +        
               BLKSIZE=1104                                                     
         EJECT                                                                  
DMCB     DS    6F                                                               
COUNT    DC    PL4'0'                                                           
APUTTBLX DC    A(PUTTABLX)                                                      
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RSPARE   DS    XL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL27                                                            
         DS    1050C                                                            
*                                                                               
        DC    C'**TABL**'                                                       
TABLE   DS    0D                                                                
        DC    X'1400000000000000000000000000000000E6C21453999900010000'         
        DC    X'1400000000000000000000000000000000E7E54437999900010000'         
        DC    X'1600000000000000000000000000E7E43706999900010000000000'         
        DC    X'1600000000000000000000000000E7E437069999000100F1D60000'         
        DC    X'1600000000000000000000000000E7E437069999000100F1D60001'         
        DC    X'1600000000000000000000000000E7E437069999000100F1D60002'         
        DC    X'1600000000000000000000000000E7E437069999000100F1D60003'         
        DC    X'0C00C2D3E3C1E6C1E3C540D5E8E2C1C1E3D5E8D3F9F6F102704335'         
        DC    X'0C00C2D3E3C9E6D3E5C940D5E8E8D94040D5E8D2D9404002704334'         
        DC    X'0C00C2D3E3D5D2E7C1E240C3C1D3C8C3404040C1F0F1F302666450'         
        DC    X'0C00C2D3E3D5E6D7E3E540D7C8C8F6F4F24040C3D4C6E202704328'         
        DC    X'0C00E6C2F1F0E6C2C1D340C2C1D3C5D5C44040D1C1C8C500006458'         
        DC    X'0C00E7E4E340E6C9D5D240C6D4F0F0F0F04040F0F0F0F000003926'         
        DC    X'0C00E7E5E340E6C5E6E240C3D3F2F7F7F74040F3F9F94000002655'         
        DC    X'12000000000000000000C2D3D2E7C1D5E348F6F900570719FF0029'         
        DC    X'12000000000000000000C2D3D2E7C1D5E34AF6D90058071FD9580B'         
        DC    X'12000000000000000000C2D3D2E7C1D5E34AF6D90058071FD95902'         
        DC    X'12000000000000000000C2D3D2E7C1D5E34AF6D90058071FFF0029'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0303'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0404'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0505'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0606'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0707'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0808'         
        DC    X'0B000000000000000000000000000000C2D346659279FFFFFF0101'         
        DC    X'0B000000000000000000000000000000C2D346659279FFFFFF0202'         
        DC    X'0B000000000000000000000000000000C2D346659279FFFFFF0303'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0101'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0202'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0303'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0404'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0505'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0606'         
        DC    X'0B000000000000000000000000000000C2D349659279FFFFFF0707'         
        DC    X'0B000000000000000000000000000000C2D356269279FFFFFF0303'         
        DC    X'0B000000000000000000000000000000C2D356659279FFFFFF0101'         
        DC    X'0B000000000000000000000000000000C2D372051379FFFFFF0202'         
        DC    X'0B000000000000000000000000000000C2D372051379FFFFFF0204'         
        DC    X'0B000000000000000000000000000000C2D394533379FFFFFF0404'         
        DC    X'0B000000000000000000000000000000C2D394533379FFFFFF0405'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0202'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0303'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0404'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0505'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0606'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0707'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0808'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0909'         
        DC    X'0B000000000000000000000000000000C2D317659279FFFFFF0A0A'         
        DC    X'0B000000000000000000000000000000C2D317810379FFFFFF0202'         
        DC    X'080000000000000000000000000000000000000000D1C1C8C5E6C2'         
        DC    X'080000000000000000000000000000000000000000F3F9F940E7E5'         
        DC    X'0A000000000000000000000000000000000000D3C5D5C44040E6C2'         
        DC    X'0A000000000000000000000000000000000000F2F7F7F74040E7E5'         
        DC    X'FF'                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**PUTB**'                                                      
PUTTABLE DC    60000X'00'                                                       
PUTTABLX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016REPEEL    08/31/00'                                      
         END                                                                    
