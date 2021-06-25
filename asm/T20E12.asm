*          DATA SET T20E12     AT LEVEL 022 AS OF 05/01/02                      
*PHASE T20E12C,+0,NOAUTO                                                        
*INCLUDE UNDAY                                                                  
*INCLUDE UNTIME                                                                 
         TITLE 'EXPAND AND COMPUTE DEMOS'                                       
T20E12   CSECT                                                                  
         NMOD1 0,T20E12,RR=R8                                                   
         L     RC,0(R1)            A(WORK AREA)                                 
         LA    RC,0(RC)                                                         
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R2,FULL                                                          
         LA    R7,BUYREC2                                                       
         USING BUYRECD,R7                                                       
         ST    R8,RELO                                                          
         B     CLRFLD                                                           
RELO     DC    A(0)                                                             
CLRFLD   CLI   0(R2),0                                                          
         BE    CLRFLDX                                                          
         ZIC   R3,FLDLEN                                                        
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA,FLDDATA                                                  
         BZ    CLRFLD1                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA,FLDDATA                                                  
         FOUT  (R2)                                                             
CLRFLD1  LA    R2,9(R3,R2)                                                      
         B     CLRFLD                                                           
CLRFLDX  L     R2,FULL                                                          
*                                                                               
*        FORMAT DEMO LOOKUP INFO IN STANDARD FORMAT                             
*                                                                               
         XC    WORKEND(255),WORKEND                                             
         OI    6(R2),1                                                          
         LA    R5,BUYREC2+108                                                   
         USING NDELEM,R5                                                        
         MVC   WORKEND(16),NDPROG                                               
         LA    R6,WORKEND+16                                                    
         CLI   SPILLSW,2                                                        
         BNE   RFORMAT                                                          
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
RFORMAT  CLI   NDEMNO+1,0                                                       
         BE    RFEXIT                                                           
         XC    FULL,FULL           APPLY ADJUSTMENT FACTOR                      
         MVC   FULL+2(2),NDEMNO+6                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),NDEMNO+3                                               
         L     RF,FULL                                                          
         MH    RF,HALF                                                          
         AH    RF,=H'50'                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STH   RF,HALF                                                          
         XC    NDEMNO+4(4),NDEMNO+4                                             
         MVC   NDEMNO+6(2),HALF                                                 
         MVC   0(8,R6),NDEMNO                                                   
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         B     RFORMAT                                                          
         EJECT                                                                  
*                                                                               
*        DEMOS HAVE BEEN LOOKED UP - NOW BUILD SCREEN                           
*                                                                               
RFEXIT   LA    R8,WORKEND+16                                                    
         XC    FLDDATA(8),FLDDATA                                               
         GOTO1 =V(UNDAY),DMCB,DUB,FLDDATA,RR=RELO                               
         FOUT  (R2)                                                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         XC    FLDDATA(11),FLDDATA                                              
         MVC   DMCB(2),DUB+1                                                    
         BAS   R9,SET15                                                         
         CLC   DUB+1(2),DUB+3                                                   
         BNE   RFE1                                                             
         XC    DUB+3(2),DUB+3                                                   
         MVC   DUB+1(2),DMCB                                                    
RFE1     GOTO1 =V(UNTIME),DMCB,DUB+1,FLDDATA,RR=RELO                            
         FOUT  (R2)                                                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,FULL             SAVE FIELD HEADER ADDRESS                    
         OC    HALF2,HALF2         PROGRAM NAME REQUIRED                        
         BZ    DCOMP2               NO - SEND DEMOS                             
         AH    R2,HALF2             YES - SEND PROGRAM NAME                     
         MVC   FLDDATA(16),WORKEND                                              
         L     R2,FULL             RESET R2 TO FIELD HEADER                     
DCOMP2   MVC   HALF,6(R8)               GET DEMO VALUE                          
         CLI   FNO,3               ADJUSTMENT SCREEN                            
         BL    *+14                 NO                                          
         MVI   HALF,0               YES - MOVE IN ADJUSTMENTS                   
         MVC   HALF+1(1),3(R8)                                                  
         CLI   FNO,3                                                            
         BNL   EDIT2                                                            
         EDIT  (2,HALF),(7,FLDDATA),1                                           
E2RTRN   LA    R2,8(R2)                                                         
         LA    R8,8(R8)                                                         
         CLI   1(R8),0                  END OF LIST                             
         BNE   DCOMP2                                                           
         LA    R5,4(R5)                                                         
         L     R2,FULL                   YES - SEND TO SCREEN                   
         FOUT  (R2)                                                             
         B     EXXMOD                                                           
*                                                                               
EDIT2    EDIT  (2,HALF),(7,FLDDATA)                                             
         B     E2RTRN                                                           
*                                                                               
*        SET TIME TO START OF QUARTER HOUR                                      
*                                                                               
SET15    LH    RE,DMCB                                                          
         LTR   RE,RE                                                            
         BZR   R9                                                               
         SRDA  RE,32                                                            
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZR   R9                                                               
         SRDA  RE,32                                                            
         D     RE,=F'15'                                                        
         LH    RF,DMCB                                                          
         SR    RF,RE                                                            
         STH   RF,DMCB                                                          
         BR    R9                                                               
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
       ++INCLUDE GENOLD                                                         
BUYREC2  DS    2000C                                                            
         ORG   IOAREA                                                           
       ++INCLUDE T20EWORK                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022T20E12    05/01/02'                                      
         END                                                                    
