*          DATA SET DDZIPTEST  AT LEVEL 028 AS OF 04/28/98                      
         TITLE 'PKZIP - COMPRESS/DECOMPRESS INFORMATION TEST'                   
*PHASE ZIPTEST                                                                  
         ENTRY WORKAREA                                                         
MAIN     CSECT                                                                  
         NBASE WORKL,ZIPINIT*,=V(WORKAREA)                                      
         USING WORKD,RC                                                         
*                                                                               
         BAS   RE,GETSPC                                                        
         BAS   RE,ZIPTO            MOVE INFORMATION TO DATASPACE                
         BAS   RE,ZIPWAKE          CALL PKZIP TO WAKE UP                        
         BAS   RE,WAIT             WAIT UNTIL POSTED FROM PKZIP                 
         BAS   RE,ZIPFR            MOVE RESULTS FROM DATASPACE                  
         BAS   RE,CZIPTO           MOVE INFORMATION TO DATASPACE                
         BAS   RE,ZIPWAKE          CALL PKZIP TO WAKE UP                        
         BAS   RE,WAIT             WAIT UNTIL POSTED FROM PKZIP                 
         BAS   RE,CZIPFR           MOVE RESULTS FROM DATASPACE                  
*                                                                               
MAINX    XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* GET DATASPACE                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETSPC   NTR1  ,                                                                
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),=CL12'TABTXXXXXXXX'                                   
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ALET,WORK+24                                                     
         OC    ALET,ALET                                                        
         BNZ   GETSPCX                                                          
         DC    H'0'                                                             
*                                                                               
GETSPCX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MOVE INFORMATION TO DATASPACE FROM CALLER                           *         
***********************************************************************         
         SPACE 1                                                                
ZIPTO    NTR1  ,                                                                
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
*                                                                               
         L     R0,ASIDFLD                                                       
         STH   R0,ASID                                                          
         MVI   FACPAK,C'A'         SET UP TEMP TOKEN FROM FAC/TASK              
         MVI   TASK,C'A'                                                        
*                                                                               
         LAM   R2,R2,ALET                                                       
         XR    R2,R2                                                            
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSZIP                                                    
         AHI   R2,(TABSID+TZIPDSP+TABSID)                                       
         USING TZIPHDRD,R2                                                      
*                                                                               
         LA    R0,TZHNUM           NUMBER OF SLOTS IN DATASPACE                 
         L     RF,LOCKID           REPLACEMENT VALUE IS LOCK TOKEN              
*                                                                               
ZIPTO02  XR    RE,RE               COMPARISON VALUE                             
         CS    RE,RF,TZHID                                                      
         BE    ZIPTO04             FILLED THIS SLOT                             
         LA    R2,TZHLENQ(,R2)                                                  
         BCT   R0,ZIPTO02                                                       
         DC    H'0'                NEED TO FIND A WAY TO WAIT                   
*                                                                               
ZIPTO04  STCM  R2,15,AHEADER       SAVE A(THIS HEADER)                          
*                                                                               
         LA    RF,ECB                                                           
         STCM  RF,15,TZHECB        SET A(ECB)                                   
*                                                                               
         XC    TZHZIP,TZHZIP                                                    
         XC    TZHULEN,TZHULEN                                                  
         XC    TZHCLEN,TZHCLEN                                                  
         XC    TZHFLAG3,TZHFLAG3                                                
         XC    TZHCRC,TZHCRC                                                    
*                                                                               
         MVI   TZHFLAG1,TZHF1CMP                                                
*                                                                               
         ICM   RF,15,ZIPINL        SET INPUT LENGTH IN CORRECT AREA             
         STH   RF,TZHULEN                                                       
         LR    R5,RF                                                            
         LA    RE,ZIPBUFF                                                       
*                                                                               
         CPYA  R4,R2                                                            
         ICM   R4,15,TZHBODY                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVCL  R4,RE               COPY DATA TO BODY OF THIS SLOT               
         XR    R4,R4                                                            
         SAR   R4,R4                                                            
*                                                                               
         MVI   TZHFLAG2,TZHF2NEW   SET NEW WORK - DO THIS LAST                  
         SAC   0                   CLEAN UP                                     
         LAM   R0,RF,ARZERO                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MOVE INFORMATION TO DATASPACE FROM CALLER (FOR COMPRESSED DATA)     *         
***********************************************************************         
         SPACE 1                                                                
CZIPTO   NTR1  ,                                                                
         LAM   R2,R2,ALET                                                       
         XR    R2,R2                                                            
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSZIP                                                    
         AHI   R2,(TABSID+TZIPDSP+TABSID)                                       
         USING TZIPHDRD,R2                                                      
*                                                                               
         LA    R0,TZHNUM           NUMBER OF SLOTS IN DATASPACE                 
         L     RF,LOCKID           REPLACEMENT VALUE IS LOCK TOKEN              
*                                                                               
CZIPTO02 XR    RE,RE               COMPARISON VALUE                             
         CS    RE,RF,TZHID                                                      
         BE    CZIPTO04            FILLED THIS SLOT                             
         LA    R2,TZHLENQ(,R2)                                                  
         BCT   R0,CZIPTO02                                                      
         DC    H'0'                NEED TO FIND A WAY TO WAIT                   
*                                                                               
CZIPTO04 STCM  R2,15,AHEADER       SAVE A(THIS HEADER)                          
         LA    RF,ECB                                                           
         STCM  RF,15,TZHECB        SET A(ECB)                                   
*                                                                               
         XC    TZHZIP,TZHZIP                                                    
         XC    TZHULEN,TZHULEN                                                  
         XC    TZHCLEN,TZHCLEN                                                  
         XC    TZHFLAG3,TZHFLAG3                                                
         XC    TZHCRC,TZHCRC                                                    
*                                                                               
         MVI   TZHFLAG1,TZHF1UNC                                                
*                                                                               
         ICM   RF,15,ZIPCMPL       SET INPUT LENGTH IN CORRECT AREA             
         STH   RF,TZHCLEN                                                       
         LR    R5,RF                                                            
         L     RE,=A(ZIPCMP)                                                    
*                                                                               
         CPYA  R4,R2                                                            
         ICM   R4,15,TZHBODY                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVCL  R4,RE               COPY DATA TO BODY OF THIS SLOT               
         XR    R4,R4                                                            
         SAR   R4,R4                                                            
*                                                                               
         MVI   TZHFLAG2,TZHF2NEW   SET NEW WORK - DO THIS LAST                  
         SAC   0                   CLEAN UP                                     
         LAM   R0,RF,ARZERO                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CALL PKZIP TO AWAKEN                                                *         
***********************************************************************         
         SPACE 1                                                                
ZIPWAKE  NTR1  ,                                                                
         LAM   R2,R2,ALET                                                       
         XR    R2,R2                                                            
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSZIP                                                    
         AHI   R2,TABSID           GO PAST HEADER TO TABLE                      
         USING TABSZIPD,R2                                                      
*                                                                               
         LA    R0,TAZNUM           NUMBER OF SLOTS IN DATASPACE                 
         MVI   POSTNUM,0                                                        
         XC    AFSTZIP,AFSTZIP                                                  
*                                                                               
ZIPWK02  OC    TAZJOB,TAZJOB       JOB REGISTERED?                              
         BZ    ZIPWK08                                                          
         OC    AFSTZIP,AFSTZIP                                                  
         BNZ   *+8                                                              
         ST    R2,AFSTZIP          SET A(FIRST ZIP IMAGE)                       
         CLI   TAZFLAG,TAZFSLP     JOB SLEEPING?                                
         BNE   ZIPWK06                                                          
*                                                                               
ZIPWK04  ICM   R3,15,TAZPOST       SET A(WAKEUP ECB) IN R3                      
         LH    R5,TAZASID          GET ASCB FOR THE PKZIP IMAGE                 
         SAC   0                                                                
*                                                                               
         XR    RF,RF               INCREMENT COUNT OF POSTS                     
         IC    RF,POSTNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,POSTNUM                                                       
*                                                                               
         LOCASCB ASID=(R5)                                                      
         LTR   RF,RF               ASCB RETURNED IN R1 OK?                      
         BNZ   ZIPWK06             NO                                           
*                                                                               
         LR    R4,R1               SET ASCB IN R4                               
*                                                                               
         POST (R3),99,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTLF)           
         B     ZIPWAKEX                                                         
*                                                                               
POSTLF   POST  ERRET=ZIPWK06,ECBKEY=YES,MF=L                                    
*                                                                               
ZIPWK06  SAC   512                                                              
         MVI   TAZFLAG,TAZFDED     SET IMAGE ABENDED (SO NO MORE POSTS)         
         CLI   POSTNUM,255                                                      
         BE    ZIPFUCK                                                          
*                                                                               
ZIPWK08  LA    R2,TZLENQ(,R2)      TRY NEXT IMAGE                               
         BCT   R0,ZIPWK02                                                       
         CLI   POSTNUM,0           POSTED ANYONE?                               
         BNE   ZIPWAKEX            YES                                          
         ICM   R2,15,AFSTZIP       A(FIRST ZIP IMAGE) SET?                      
         BZ    ZIPFUCK             NO                                           
         MVI   POSTNUM,255                                                      
         B     ZIPWK04             GO POST FIRST ONE FOR GOOD MEASURE           
*                                                                               
ZIPFUCK  DC    H'0',C'NO ZIP POST' DID NOT POST ANY ZIP IMAGES                  
*                                                                               
ZIPWAKEX SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MOVE RESULTS FROM DATASPACE TO CALLER                               *         
***********************************************************************         
         SPACE 1                                                                
ZIPFR    NTR1  ,                                                                
         LAM   R2,R2,ALET                                                       
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING TZIPHDRD,R2                                                      
*                                                                               
         L     RE,=A(ZIPCMP)                                                    
         LH    RF,TZHCLEN          PICK UP COMPRESSED LENGTH                    
         ST    RF,ZIPCMPL          SET INPUT LENGTH IN CORRECT AREA             
         CHI   RF,TZBUFLEN                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MUCH TO COPY BACK                        
*                                                                               
         CPYA  R4,R2                                                            
         ICM   R4,15,TZHBODY                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R5,RF                                                            
         MVCL  RE,R4               COPY DATA TO OUTPUT BUFFER                   
         XR    R4,R4                                                            
         SAR   R4,R4                                                            
*                                                                               
         XC    TZHZIP,TZHZIP                                                    
         XC    TZHULEN,TZHULEN                                                  
         XC    TZHCLEN,TZHCLEN                                                  
         XC    TZHFLAG3,TZHFLAG3                                                
         XC    TZHCRC,TZHCRC                                                    
         XC    TZHECB,TZHECB                                                    
         XC    TZHID,TZHID         CLEAR THIS LAST!                             
         SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MOVE RESULTS FROM DATASPACE TO CALLER (UNCOMPRESS)                  *         
***********************************************************************         
         SPACE 1                                                                
CZIPFR   NTR1  ,                                                                
         LAM   R2,R2,ALET                                                       
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING TZIPHDRD,R2                                                      
*                                                                               
         L     RE,=A(ZIPUNC)                                                    
         LH    RF,TZHULEN          PICK UP UNCOMPRESSED LENGTH                  
         ST    RF,ZIPUNCL          SET INPUT LENGTH IN CORRECT AREA             
         CHI   RF,TZBUFLEN                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MUCH TO COPY BACK                        
*                                                                               
         CPYA  R4,R2                                                            
         ICM   R4,15,TZHBODY                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R5,RF                                                            
         MVCL  RE,R4               COPY DATA TO OUTPUT BUFFER                   
         XR    R4,R4                                                            
         SAR   R4,R4                                                            
*                                                                               
         XC    TZHZIP,TZHZIP                                                    
         XC    TZHULEN,TZHULEN                                                  
         XC    TZHCLEN,TZHCLEN                                                  
         XC    TZHFLAG3,TZHFLAG3                                                
         XC    TZHCRC,TZHCRC                                                    
         XC    TZHECB,TZHECB                                                    
         XC    TZHID,TZHID         CLEAR THIS LAST!                             
         SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET WAIT UNTIL POSTED EITHER BY OPERATOR OR FACPAK                  *         
***********************************************************************         
         SPACE 1                                                                
WAIT     ST    RE,SAVERE                                                        
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         XC    ECB,ECB                                                          
         LA    R1,ECB                                                           
         ICM   RF,15,=V(ADWAIT)                                                 
         BZ    WAIT02                                                           
         BASSM RE,RF                                                            
         B     WAITX                                                            
*                                                                               
WAIT02   WTO   'SINGLE THREAD WAIT'                                             
*                                                                               
         WAIT  ECB=ECB                                                          
*                                                                               
         WTO   'EXIT WAIT'                                                      
*                                                                               
WAITX    L     RE,SAVERE                                                        
         O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'ASIDFLD'                                                     
ASIDFLD  DC    D'0'                                                             
*                                                                               
ARZERO   DS    16F'0'                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0L                                                               
         DC    CL8'ZIPINL'                                                      
ZIPINL   DC    A(4096)                                                          
         DC    A(0)                                                             
         DC    CL8'ZIPCMPL'                                                     
ZIPCMPL  DC    A(0)                                                             
         DC    A(0)                                                             
         DC    CL8'ZIPUNCL'                                                     
ZIPUNCL  DC    A(0)                                                             
         DC    A(0)                                                             
*                                                                               
         DC    CL16'*ZIPBUFF*ZIPBUFF*'                                          
ZIPBUFF  DC    CL45'THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG.'              
         DC    CL45'THE BUFFER I REPEATED IS THERE NOW YOU SEE...'              
         DC    CL45'TEST DATA TIME, THIS IS TEST DATA YOU KNOW !!'              
         DC    CL45'WHOS YOUR COMPRESSOR?                        '              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'              
         DC    CL45'                                             '              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'COLOUR ME HAPPY YOU BIG STALLION             '              
         DC    CL45'                                             '              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'                                             '              
         DC    CL45'                                             '              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'1234567890123                         9012345'              
         DC    CL45'123456789012345678901234567890123456789012345'              
         DC    CL45'12345678901234567890123456              12345'              
         DC    CL45'                                           45'              
         DC    CL16'**ZIPCMPZIPCMP**'                                           
ZIPCMP   DC    41XL100'00'                                                      
         DC    CL16'**ZIPUNCZIPUNC**'                                           
ZIPUNC   DC    41XL100'00'                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER REQUIRED BOOKS                                                *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FAPLO                                                          
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
WORK     DS    XL40                                                             
SAVERE   DS    A                                                                
ALET     DS    A                                                                
AHEADER  DS    A                   A(THIS TZIPHDRD)                             
ECB      DS    F                   ECB TO POST COMPLETION/WAIT ON               
LOCKID   DS    0A                  LOCKING TOKEN                                
FACPAK   DS    X                                                                
TASK     DS    X                                                                
ASID     DS    H                                                                
AFSTZIP  DS    A                   A(FIRST ZIP IMAGE FOUND)                     
POSTNUM  DS    X                   COUNT OF POSTS                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 2                                                                
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
* FATABSZIP                                                                     
       ++INCLUDE FATABSZIP                                                      
WORKAREA CSECT                                                                  
         DC    500000X'00'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DDZIPTEST 04/28/98'                                      
         END                                                                    
