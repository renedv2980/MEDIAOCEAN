*          DATA SET SPDEMEXT   AT LEVEL 047 AS OF 07/20/17                      
*PHASE T00AC3B                                                                  
SDEMEXT  TITLE 'SDEMEXT - EXTRACT SPOT DEMO VALUES FROM BUYS'                   
SDEMEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*SDEMEXT,RR=R3,CLEAR=YES                             
         USING WORKD,RC                                                         
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         LR    RA,R1                                                            
         USING DEMEXTD,RA                                                       
*                                                                               
         ICM   R3,15,DXDEMTAB      A(OUTPUT TABLE)                              
         JNZ   *+12                                                             
         LA    R3,DEMTAB           IF USER DOESN'T PROVIDE                      
         ST    R3,DXDEMTAB         GIVE HIM THIS ONE                            
*                                                                               
         MVC   NTBUYNMS,DXNTNMS    SAVE A(NONT DEMO 50EL)                       
         OC    NTBUYNMS,NTBUYNMS   TEST IT'S THERE                              
         JNZ   SD10                                                             
*                                                                               
         L     R6,DXDEMEL                                                       
         MVI   ELCODE,X'50'        FIND NT DEMO NAMES EL IN BUYREC              
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R6,2(R6)                                                         
         ST    R6,NTBUYNMS                                                      
*                                                                               
SD10     L     R2,DXESTDEM         POINT TO  DEMO LIST                          
         OC    0(3,R2),0(R2)       ANY DEMOS??                                  
         JZ    SDX                                                              
*                                                                               
         L     R3,DXDEMTAB                                                      
         USING DXDEMTABD,R3                                                     
         LA    R0,20                                                            
*                                                                               
SD12     MVC   DXDEMCD,0(R2)       MOVE DEMO CODE                               
*                                                                               
         CLI   DXDEMCD+2,0         TEST FOR NONT DEMO                           
         JNE   SD20                NO                                           
                                                                                
* IF DEMO IS OF FORM 00NN00, THEN IT'S A NONT DEMO                              
                                                                                
         LLC   RE,DXDEMCD+1        GET INDEX INTO NTDEMO LIST                   
         BCTR  RE,0                                                             
         MHI   RE,L'ENONTDMS                                                    
         ICM   RF,15,NTBUYNMS                                                   
         JZ    *+2                 MUST HAVE NAMES ELEMENT                      
         AR    RE,RF                                                            
         MVC   DXDEMNM,0(RE)                                                    
         J     SD30                                                             
*                                                                               
SD20     CLI   DXDEMCD+1,X'21'     TEST USER DEMO                               
         JNE   SD30                                                             
         LLC   RE,DXDEMCD+1        GET DEMO NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         A     RE,DXUSRNMS                                                      
         MVC   DXDEMNM,0(RE)                                                    
         MVI   DXDEMNM+7,C' '      BLANK 8TH CHAR THAT ISN'T THERE              
*                                                                               
SD30     CLI   DXDEMNM,C'R'        TEST RATING                                  
         JE    SD32                                                             
         CLI   DXDEMNM,C'E'        OR EXTENDED RATING                           
         JNE   SD40                                                             
*                                                                               
SD32     OI    DXDEMFLG,X'20'      SET DEMO IS A RATING FLAG                    
*                                                                               
SD40     LA    R2,3(R2)                                                         
         AHI   R3,DXDEMTABL                                                     
         AHI   R0,-1                                                            
         JZ    SD45                                                             
         OC    0(2,R2),0(R2)                                                    
         JNZ   SD12                                                             
SD45     LA    RE,20                                                            
         SR    RE,R0                                                            
         STC   RE,DXDEMCNT                                                      
         EJECT                                                                  
*================================================================               
* LOOK FOR POST-BUY DEMO ELEMENT                                                
*================================================================               
                                                                                
SD50     L     R6,DXDEMEL          POINT TO DEMO ELEMENT                        
*                                                                               
         MVC   ELCODE,0(R6)        MOVE DEMO ELEMENT CODE                       
         CLI   ELCODE,X'03'        TEST SPILL DEMEL                             
         JNE   *+10                                                             
         MVC   SVSPLMKT,NDAGYMKT-NDELEM(R6)   SAVE SPILL MARKET                 
         OI    ELCODE,X'20'        SET POSTBUY ELEMENT FLAG                     
*                                                                               
SD52     BRAS  RE,NEXTEL                                                        
         JNE   SD60                                                             
*                                                                               
         CLI   0(R6),X'22'         TEST ORIGINATING                             
         JE    SD56                                                             
         CLC   SVSPLMKT,SDAGYMKT-SDELEM(R6)  MATCH SPILL MKT                    
         JNE   SD52                                                             
*                                                                               
SD56     ST    R6,DXPBDEL          SAVE POINTER TO POST-BUY DEMEL               
*                                                                               
         EJECT                                                                  
*==============================================================                 
* EXTRACT VALUES INTO TABLE FROM 02/03 AND 22/23 ELEMENTS                       
*==============================================================                 
                                                                                
SD60     L     R3,DXDEMTAB                                                      
*                                                                               
SD62     BRAS  RE,GETVAL           GET VALUE FROM 02/03 ELEM                    
*                                                                               
         AHI   R3,DXDEMTABL                                                     
         OC    DXDEMCD,DXDEMCD     TEST ANY MORE DEMOS                          
         JNZ   SD62                                                             
*                                                                               
SDX      XIT1                                                                   
         EJECT                                                                  
*==================================================================             
* NOW EXTRACT DEMO VALUES FROM BUY RECORD DEMO ELEMENT                          
* DXDEMEL POINTS TO 02/03 ELEM                                                  
* DXPBDEL POINTS TO 22/23 ELEM                                                  
*==================================================================             
                                                                                
         USING DXDEMTABD,R3                                                     
GETVAL   L     R1,DXDEMEL          POINT TO 02/03 DEMEL IN BUYREC               
         LLC   R0,1(R1)                                                         
         AHI   R0,-24                                                           
         BNPR  RE                  NO DEMOS IN ELEM!                            
         SRL   R0,3                SET COUNTER FOR NUMBER OF DEMOS              
         LA    R1,24(R1)                                                        
*                                                                               
         ICM   R6,15,DXPBDEL       TEST POST BUY DEMEL                          
         JZ    GETVAL10                                                         
*                                                                               
         CLI   0(R6),X'22'         TEST ORIG POST BUY DEMEL                     
         JNE   GETVAL2                                                          
         AHI   R6,PDEMO-PDELEM                                                  
         J     GETVAL10                                                         
*                                                                               
GETVAL2  AHI   R6,SDEMO-SDELEM     FIRST VALUE FOR POST BUY SPILL DEMEL         
*                                                                               
GETVAL10 CLC   DXDEMCD,0(R1)       MATCH DEMO CODE                              
         JE    GETVAL20                                                         
         LA    R1,8(R1)            NEXT DEMO                                    
         LTR   R6,R6                                                            
         JZ    *+8                                                              
         LA    R6,3(R6)            NEXT POSTBUY DEMO VALUE                      
         JCT   R0,GETVAL10                                                      
         BR    RE                                                               
*                                                                               
GETVAL20 MVC   DXDEMVAL,4(R1)      MOVE DEMO VALUE FROM DEMEL                   
         NI    DXDEMVAL,X'3F'      DROP FLAGS                                   
         MVC   DXDEMFLG,4(R1)      MOVE FLAGS                                   
         NI    DXDEMFLG,X'C0'      DROP ALL BUT OVRD/2-DEC                      
*                                                                               
         LTR   R6,R6               TEST ANY POST-BUY DEMS                       
         JZ    GETVAL22                                                         
*                                                                               
         MVC   DXPOSTVAL+1(3),0(R6) MOVE POST-BUY DEMO VALUE                    
         NI    DXPOSTVAL+1,X'7F'                                                
         MVC   DXPOSTFLG,0(R6)     MOVE POST-BUY FLAG                           
         NI    DXPOSTFLG,X'80'                                                  
*                                                                               
GETVAL22 CLI   DXDEMCD+2,0         TEST NON-TD DEMO                             
         JNE   GETVALX             NO                                           
         LLC   R1,DXDEMCD+1        GET INDEX VALUE                              
         BCTR  R1,0                                                             
         SLL   R1,3                X 8                                          
         A     R1,NTBUYNMS         POINT TO NAME IN X'50' ELEM                  
         CLC   DXDEMNM(7),0(R1)    TEST NAMES ARE THE SAME                      
         JE    GETVALX                                                          
         XC    DXDEMENT,DXDEMENT   ELSE CLEAR VALUE                             
*                                                                               
GETVALX  BR    RE                                                               
         GETEL (R6),24,ELCODE                                                   
         LTORG                                                                  
WORKD    DSECT                                                                  
RELO     DS    A                                                                
NTBUYNMS DS    A                                                                
SVSPLMKT DS    H                                                                
ELCODE   DS    X                                                                
         DS    0D                                                               
DEMTAB   DS    XL512                                                            
WORKX    EQU   *                                                                
       ++INCLUDE SPDEMEXTD                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPDEMEXT  07/20/17'                                      
         END                                                                    
