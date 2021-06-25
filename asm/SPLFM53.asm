*          DATA SET SPLFM53    AT LEVEL 066 AS OF 05/01/02                      
*PHASE T21953A,+0                                                               
         TITLE 'SPLFM53 - ESTDOL   T21953'                                      
         SPACE 2                                                                
T21953   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21953                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ESTHDRD,R8                                                       
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    EXXMOD              NO RECORD                                    
*                                                                               
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    *+8                                                              
         BAS   RE,EDT              CHANGE RECORD THEN                           
         BAS   RE,FMT              DISPLAY RECORD                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*        DISPLAY RECORD                                                         
*                                                                               
FMT      NTR1                                                                   
         FOUT  ESTDESCH,EDESC,20                                                
         GOTO1 VDATCON,DMCB,(0,ESTART),(5,ESTSTRD)                              
         FOUT  ESTSTRDH                                                         
         GOTO1 VDATCON,DMCB,(0,EEND),(5,ESTENDD)                                
         FOUT  ESTENDDH                                                         
*                                                                               
         SR    R3,R3                    FOR COUNTER                             
         LA    R2,ESTAJANH                                                      
*                                                                               
FMT10    LA    R4,EAUTHN                AUTHORIZATIONS                          
         MVI   BYTE,C'A'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         LA    R4,EORDN                 ORDERED                                 
         MVI   BYTE,C'O'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         LA    R4,EPAIDN           PAID                                         
         MVI   BYTE,C'P'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         BAS   RE,NEXTFLD          BUMP PAST LABLE                              
         LA    R3,1(R3)            NEXT MONTH                                   
         CH    R3,=H'12'                                                        
         BL    FMT10                                                            
*                                                                               
         LA    R3,3                FOR BCTR                                     
         LA    R2,ESTATOTH                                                      
         LA    R4,ATOTAL                                                        
*                                                                               
FMT20    L     R7,0(R4)                                                         
         SR    R6,R6                                                            
         D     R6,=F'100'          GET RID OF PENNIES                           
         EDIT  (R7),(10,8(R2)),0                                                
         FOUT  (R2)                                                             
         LA    R4,4(R4)            BUMP TOTAL                                   
         BAS   RE,NEXTFLD          FIND NEXT FIELD                              
         BCT   R3,FMT20                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        OUTPUT AMOUNT                                                          
*                                                                               
PUTAMT   NTR1                                                                   
         XC    8(VALLEN,R2),8(R2)                                               
         SLL   R3,2                                                             
         AR    R4,R3               A(BUCKET TO DISPLAY)                         
         L     R7,0(R4)                                                         
         LA    R1,ATOTAL                                                        
         CLI   BYTE,C'A'                                                        
         BE    PUT20                                                            
*                                                                               
PUT10    LA    R4,(L'EORDN/2)(R4)  ADD CURRENT ORDER TO YTD                     
         A     R7,0(R4)                                                         
         LA    R1,OTOTAL           ORDER TOTAL                                  
         CLI   BYTE,C'O'                                                        
         BE    PUT20                                                            
         LA    R1,PTOTAL           PAID TOTAL                                   
*                                                                               
PUT20    L     R6,0(R1)            ADD TO TOTAL                                 
         AR    R6,R7                                                            
         ST    R6,0(R1)                                                         
*                                                                               
PUT40    SR    R6,R6                                                            
         D     R6,=F'100'          GET RID OF PENNIES                           
         EDIT  (R7),(10,8(R2)),0                                                
         FOUT  (R2)                                                             
         BAS   RE,NEXTFLD                                                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
*        CHANGE RECORD                                                          
*                                                                               
EDT      NTR1                                                                   
***      GOTO1 VDATCON,DMCB,(0,ESTART),(3,SYR)                                  
***      GOTO1 (RF),(R1),(0,EEND),(3,EYR)                                       
*                                                                               
         LA    R2,ESTAJANH         AUTHORIZATIONS                               
         MVI   ERRCD,INVERR                                                     
         SR    R3,R3               COUNTER                                      
         LA    R4,ELEM+200                                                      
         MVC   0(BUCKLEN,R4),EAUTHN     SAVE OLD AUTH $'S                       
         LA    R5,EAUTHN                                                        
         XC    0(BUCKLEN,R5),0(R5)                                              
*                                                                               
EDT10    CLI   5(R2),0                                                          
         BNE   EDT20                                                            
         OC    0(4,R4),0(R4)         SEE IF $ WERE THERE                        
         BZ    EDT30                                                            
         MVI   ERRCD,AUTHDERR        MUST USE 0 TO DELETE $'S                   
         B     LFMERR                                                           
*                                                                               
EDT20    MVI   ERRCD,INVERR                                                     
         ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(C'N',8(R2)),(R0)                                  
         CLI   0(R1),0                                                          
         BNE   LFMERR                                                           
         L     R0,4(R1)                                                         
         C     R0,=F'0'                                                         
         BL    LFMERR                CAN'T BE NEGATIVE                          
         BE    *+8                                                              
         BAS   RE,VALPER           VALIDATE AUTH WITHIN EST DATES               
         MH    R0,=H'100'            TO ADD CENTS                               
         ST    R0,0(R5)                                                         
*                                                                               
EDT30    LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BAS   RE,FNDNXUF1           NEXT UNPROTECTED FIELD                     
         LA    R3,1(R3)                                                         
         CH    R3,=H'12'                                                        
         BL    EDT10                                                            
*                                                                               
         GOTO1 PUTREC              WRITE BACK RECORD                            
         B     XIT                 & RE-DISPLAY IT                              
         EJECT                                                                  
*                                                                               
*        ADD CURRENT AMOUNTS TO YTD AMOUNTS & STORE IN ELEM + 200               
*                                                                               
MERGEAMT NTR1                                                                   
         LA    R3,12                                                            
         LA    R4,ELEM+200                                                      
         XC    0(BUCKLEN,R4),0(R4)                                              
*                                                                               
MAMT10   L     R1,0(R5)                                                         
         A     R1,BUCKLEN(R5)                                                   
         ST    R1,0(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,MAMT10                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ENSURE AUTHORIZATION INPUT IS WITHIN ESTIMATE DATES                    
*                                                                               
VALPER   NTR1                                                                   
*                                                                               
         GOTO1 VGETBRD,DMCB,(1,ESTART),WORK,VGETDAY,VADDAY                      
*                                 WORK+6(6)=BRD START DT FOR EST START          
         GOTO1 VGETBRD,DMCB,(1,EEND),WORK+12,VGETDAY,VADDAY                     
*                                 WORK+18(6)=BRD END DATE FOR EST END           
         GOTO1 VDATCON,DMCB,(0,WORK),(3,SYR)                                    
         GOTO1 (RF),(R1),(0,WORK+18),(3,EYR)                                    
*                                                                               
         LA    R3,1(R3)            R1 = REAL MONTH NUMBER                       
         CLC   SYR,EYR             SAME YEAR                                    
         BE    VP20                                                             
         ZIC   R1,SYR                                                           
         ZIC   R4,EYR                                                           
         SR    R4,R1               R4 = NUM YEARS                               
         CH    R4,=H'1'            IF MORE THAN 1 YEAR                          
         BH    XIT                 ALL MONTHS ARE OK                            
*                                                                               
         ZIC   R1,EMN              IF MONTH IS LOWER THAN END MONTH             
         CR    R3,R1                                                            
         BE    XIT                                                              
         BH    VP15                                                             
         ZIC   R1,SMN              IT MUST BE HIGHER (=) TO START MONTH         
         LA    R4,12(R3)                                                        
         CR    R4,R1                                                            
         BL    LFMERR                                                           
         B     XIT                                                              
*                                                                               
VP15     ZIC   R1,SMN              MONTH IS HIGHER THAN END MONTH               
         CR    R3,R1               MUST BE HIGHER (=) TO START MONTH            
         BL    LFMERR                                                           
         B     XIT                                                              
*                                                                               
VP20     ZIC   R1,SMN              START MONTH                                  
         CR    R3,R1                                                            
         BL    LFMERR                                                           
         ZIC   R1,EMN              END MONTH                                    
         CR    R3,R1                                                            
         BH    LFMERR                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FIND UNPROTECTED FIELD                                                 
*                                                                               
FNDUF1   TM    1(R2),X'20'         FIND UNPROTECTED FIELD                       
         BZR   RE                                                               
*                                                                               
FNDNXUF1 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF1                                                           
         DC    H'0'                END OF SCREEN                                
*                                                                               
NEXTFLD  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNER  RE                                                               
         DC    H'0'                END OF SCREEN                                
         EJECT                                                                  
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
YES      SR    RC,RC               SET CC                                       
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
VALLEN   EQU   10                  LENGTH OF FIELD                              
BUCKLEN  EQU   52                  LENGTH OF BUCKETS                            
*                                                                               
MASLINTB DS    0H                                                               
         DC    C'DDNY916T'                                                      
         DC    C'DDNYF11T'                                                      
         DC    C'HDTO10CT'                                                      
         DC    C'DD13C2D1'                                                      
         DC    C'HDT1C4C2'                                                      
         DC    C'XDDSC84A'                                                      
         DC    C'DDLA110T'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMB3                                                                        
       ++INCLUDE SPLFMB3D                                                       
         EJECT                                                                  
*                                                                               
GENOLD   DSECT                                                                  
         ORG   REC+1500                                                         
SYR      DS    XL1                                                              
SMN      DS    XL1                                                              
SDY      DS    XL1                                                              
EYR      DS    XL1                                                              
EMN      DS    XL1                                                              
EDY      DS    XL1                                                              
*                                                                               
ATOTAL   DS    F                   AUTHORIZATION TOTAL                          
OTOTAL   DS    F                   ORDERED TOTAL                                
PTOTAL   DS    F                   PAID TOTAL                                   
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066SPLFM53   05/01/02'                                      
         END                                                                    
