*          DATA SET SPGETRATE  AT LEVEL 044 AS OF 06/09/16                      
*PHASE T00A5FA                                                                  
         SPACE 1                                                                
*================================================================               
* NOTE THAT ONLY REMAINING DATE TESTS ARE IN NTP CODE !                         
*================================================================               
                                                                                
*================================================================               
* IF YOU CHANGE THE RANGE OF VALUES FOR TRADE PRODUCTS (DFCO)                   
* YOU WILL NEED TO MAKE CORRESPONDING CHANGES IN                                
* SPREPB102 AND SPREQ02                                                         
*================================================================               
                                                                                
*====================================================================*          
* CHANGE HISTORY                                                                
* MAR/96   SUPPORT FOR NTP RATES BY PRODUCT CODE FOR DFNY CHILD SPOT            
* MAR28/96 IF BDPURP=X'FF' SUPPRESS SPECIAL NTP RATES                           
* APR04/96 RE-INSTALL SUPPORT FOR FREE RIDERS                                   
* 10DEC/96 FOR AGY CK, P RATES, COMPUTE TAX ON TRUE NET                         
* FEB/97   CANADIAN HST                                                         
* 24SEP/97 FOR AGY CK, V RATE COMPUTES NET + .885 GROSS                         
* 19NOV/97 ALLOW US RATES IN DOLLARS                                            
* 12JAN/98 MAX COST/SPOT = $2,000,000                                           
* 18JUN98  COST2 BECOMES A 6 DEC PLACE FACTOR                                   
* 08DEC98  TEST FOR NOT-TBS TRADE PRD ALLOC TO SET NET=0                        
* 12MAY99  IF BDPURP=X'FE' AND BUY IS NON-TBS TRADE (WHERE TRADE PRD            
*          HAS X'80' BIT ON, COMBINE CASH/TRADE PRDS                            
* 10MAY00  ALLOW CANAD SPOT BUYS IN DOLLARS AND REMOVE TEST FOR                 
*          NO DEC31/90 GST                                                      
* 15MAY00  NEED TO BE MORE CAREFUL ABOUT DEC31/90 GST FLAG !                    
* 10AUG00  SUPPORT X'13' EXTENSION ELEMENT FOR COS2 OVERRIDES                   
* 25APR01  FOR AGY CK, X RATE COMPUTES NET + .04 GROSS                          
* 31MAY01  FOR DFCO, CHANGE NTP FACTOR EFF 05/28/01                             
* 03MAY02  FOR CANAD NET, MKT 0, RETURN COSTS IF BDPURP=X'FD'                   
* 13DEC04  FOR CANAD NET, ALLOW MKT 0 COST OVRDS AND ALWAYS RETURN $            
* 05MAY06  NEW NTP RATES                                                        
* 23APR08  NEW NTP CLIENT                                                       
* 28OCT10  NEW NTP CLIENTS                                                      
* 13NOV10  FIX COST OVERRIDE BUG FOR CANADIAN MKT 0 BUYS                        
* 03DEC10  QUEBEC TAXES FOR JAN/11 AND ON NOW 8.5%                              
* 11JUL11  MIDAS SUPPORT                                                        
* 03DEC10  QUEBEC TAXES FOR JAN/12 AND ON NOW 9.5%                              
* 05JUL12  FIX COST2 CALCULATIONS                                               
* 13MAR13  NO HST FOR BC & 14% HST FOR PE STARTING APR01/2013                   
*====================================================================*          
         SPACE 2                                                                
*====================================================================*          
*        PAR 1   PRD CODE  / A(COST AREA)                            *          
*        PAR 2   SLN CODE  / A(BUYREC)                               *          
*        PAR 3   INDICATOR / A(BUYELEM)                              *          
*        PAR 4   DOL TYPE  / A(EXCHANGE AREA) FOR INDICATOR=C'X'     *          
*                                                      OR   C'Z'     *          
*              PRD CODE = X'00' GIVES BUY RATES                      *          
*                         X'NN' GIVES RATES FOR SPECIFIC PRODUCT     *          
*                         X'FF' GIVES POOL RATE (INCLUDING UNALL)    *          
*                                                                    *          
*              SLN CODE = X'00' GIVES ALL POL ALLOCS FOR THIS PRD    *          
*                        'SECS' GIVES POL ALLOCS OF THIS LEN         *          
*                                                                    *          
*              INDICATOR = X=4TH PARM IS A(EXCHANGE AREA)            *          
*                            (XGROSS/XNET/XTAX/XC58/GSTRATE/GSTAMT)  *          
*                          Z=4TH PARM IS A(EXTENDED EXCHANGE AREA)   *          
*                                                                    *          
*              DOL TYPE    C=CANADIAN,U=USA                          *          
*                                                                    *          
*              SET COSTAREA(4) = 'COS2' TO EXTRACT COST2             *          
*                                                                    *          
*              ON RETURN   X'FF'=ERROR                               *          
*                                                                    *          
*====================================================================*          
         TITLE 'GETRATE - FIND RATES FOR BUY ELEMENTS'                          
         PRINT NOGEN                                                            
GETRATE  CSECT                                                                  
         NMOD1 WORKX-WORKD,GETRATE,CLEAR=YES                                    
         USING WORKD,RC                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING COSTAREA,RA                                                      
         L     R3,4(1)                                                          
         USING BUYRECD,R3                                                       
         L     R2,8(1)                                                          
         USING REGELEM,R2                                                       
*                                                                               
         MVI   PRDSW,0             RESET PARTNER SWITCH                         
         MVC   PRDCD,0(R1)                                                      
         MVC   SLNCD,4(R1)                                                      
         MVC   NETPAKCD,8(R1)                                                   
         ST    R1,SAVER1                                                        
         CLC   =C'COS2',COSTAREA                                                
         BNE   *+8                                                              
         MVI   COS2IND,C'Y'        SET COST2 REQUESTED                          
         XC    COSTAREA(16),COSTAREA                                            
*                                                                               
         TM    BDCIND2,X'20'       TEST CANADIAN BUY                            
         BNO   *+8                                                              
         BAS   R9,GETGST                                                        
*                                                                               
         CLI   PRDCD,0             TEST BUY RATE REQUEST                        
         BE    GR2B                                                             
         CLI   RCODE,6                                                          
         BL    GRERR                                                            
         CLI   RCODE,14                                                         
         BH    GRERR                                                            
*                                                                               
GR2B     SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
*                                                                               
         TM    BDCIND2,X'20'       TEST CANADIAN BUY                            
         BNO   GR2C                                                             
         TM    BDCIND2,X'01'       IS THE RATE IN PENNIES (CAN NET)             
         BO    GR2D                 YES - OVERRIDES DOLLARS                     
*                                                                               
GR2C     TM    BDCIND2,X'10'       TEST RATE IN DOLLARS                         
         BZ    GR2D                                                             
         C     R0,MAXDOLS          TEST COST TO MAX THAT WORKS                  
         BNH   *+6                                                              
         SR    R0,R0               IF > MAX, JUST SET COST TO 0                 
*                                                                               
         MHI   R0,100              CONVERT RATE TO PENNIES                      
*                                                                               
GR2D     ST    R0,PRDGRS                                                        
         ST    R0,PRDNET                                                        
         B     GR2E                                                             
*                                                                               
MAXDOLS  DC    A(2000000)         SPOT MUST BE < 2,000,000 DOLLARS              
MAXDOL2  DC    A(200000000)       SPOT MUST BE < 200,000,000  PENNIES           
*                                                                               
GR2E     MVI   CANETTAX,C'N'                                                    
*                                                                               
         BAS   R9,GN1              ADJUST TO GET GROSS/NET                      
         SR    R8,R8                                                            
         IC    R8,BDSEC            SET DEFAULT PARTNER SHARE                    
         CLI   PRDCD,0             WANT BUY RATES                               
         BE    GR12                                                             
         CLI   BUYKEY+3,X'FF'                                                   
         BE    GRPOL                                                            
*                                                                               
         MVC   SPOTS+3(1),RNUM     MOVE NUMBER OF SPOTS                         
         CLI   BDTIME,0            TEST REG OR PIGGYBACK                        
         BE    GR12                                                             
         EJECT                                                                  
*                                  PIGGYBACK - FIND PBELEM                      
         MVI   PRDSW,C'P'          INDICATE PASSIVE PARTNER                     
         LA    R4,BDELEM                                                        
GRPIG1   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),4                                                          
         BNE   GRPIG1                                                           
         IC    R0,1(R4)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'7'             SET R1 FOR BCT                              
         LA    R4,2(R4)                                                         
GRPIG2   CLC   0(1,R4),PRDCD                                                    
         BE    GRPIG3                                                           
         LA    R4,7(R4)                                                         
         BCT   R1,GRPIG2                                                        
         LA    R4,BDCOSTP-3        FAKE POINTER FOR ACTIVE PARTNER              
         MVI   PRDSW,0             RESET PASSIVE SWITCH                         
GRPIG3   IC    R8,3(R4)            SET PARTNER SHARE IN R8                      
         B     GR12                                                             
         EJECT                                                                  
GRPOL    TM    RSTATUS,X'04'       TEST HIATUS                                  
         BZ    GRPOL24             NO                                           
*                                                                               
GRPOL22  XC    COSTAREA(16),COSTAREA                                            
         B     GRX                                                              
*                                                                               
GRPOL24  TM    RSTATUS,X'20'       TEST RATE OVERRIDE                           
         BZ    GRPOL36                                                          
* NEED TO CHECK FOR X'13' ELEMENT FOLLOWING                                     
         LR    RE,R2                                                            
GRPOL30  SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'10'         TEST EXT ELEMENT                             
         BL    GRPOL34             NOT THERE - DO OVERRIDE                      
         CLI   0(RE),X'13'                                                      
         BL    GRPOL30             KEEP LOOKING                                 
         BH    GRPOL34             NOT THERE - DO OVERRIDE                      
* X'13' ELEMENT FOUND                                                           
         TM    2(RE),X'80'         TEST COST1 ONLY                              
         BZ    GRPOL32             NO                                           
         CLI   COS2IND,C'Y'        TEST DOING COST2                             
         BE    GRPOL36             YES - IGNORE OVERRIDE                        
         B     GRPOL34                                                          
*                                                                               
GRPOL32  TM    2(RE),X'40'         TEST COST2 ONLY                              
         BZ    GRPOL34             NO                                           
         CLI   COS2IND,C'Y'        TEST DOING COST2                             
         BNE   GRPOL36             NO - IGNORE OVERRIDE                         
*                                                                               
GRPOL34  MVI   PRDGRS,0            CLEAR HOB DUMMY                              
         MVC   PRDGRS+1(3),RPCOST                                               
         MVI   COSTOVRD,C'Y'       INDICATE COST OVERRIDE                       
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+14                                                             
         NC    PRDGRS,=X'0003FFFF'     DROP SPOTS                               
         MVI   COSTOVRD,C'N'       NO COST OVERRIDE                             
         MVC   PRDNET,PRDGRS                                                    
         BAS   R9,GN1              ADJUST TO GET GROSS/NET                      
         MVI   COSTOVRD,C'N'       NO COST OVERRIDE                             
*                                                                               
GRPOL36  DS    0H                                                               
         MVI   SPOTS+3,1           SET NUMBER OF SPOTS                          
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    GRPOL38                                                          
         ZIC   R0,RPCOST           SPOTS IN FIRST 6 BITS                        
         SRL   R0,2                                                             
         ST    R0,SPOTS                                                         
GRPOL38  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,RLEN                                                          
         AHI   R0,-10                                                           
*                                                                               
         CLI   PRDCD,X'FF'         PROCESSING POL PRD                           
         BNE   GRPOL40                                                          
         TM    BDSTAT3,BDST3_SPODS TEST SPODS ALLOWED                           
         BZ    GR12                NO                                           
         CLI   RLEN,14             TEST ONLY ONE PRODUCT                        
         BNE   GR12                                                             
         CLC   BDSEC,RPTIME        TEST SPOT SLN = BUY SLN                      
         BE    GR12                YES                                          
         CLI   SLNCD,0                                                          
         BE    *+14                                                             
         CLC   SLNCD,RPTIME        RIGHT LENGTH                                 
         BNE   GRPOL48                                                          
         IC    R8,RPTIME           THIS IS A SPOD - USE SLN TO GET COST         
         B     GR12                                                             
*                                                                               
GRPOL40  LTR   R0,R0               SPOT ALLOCATED                               
         BZ    GRPOL22             NO - IGNORE                                  
*                                                                               
         SRL   R0,2                SET R0 FOR BCT                               
         LA    R4,RPPRD                                                         
         SR    R8,R8               SET SHARE TO 0                               
*                                                                               
GRPOL42  CLC   PRDCD,0(R4)         RIGHT PRD                                    
         BE    GRPOL44                                                          
         CLI   BDPURP,X'FE'        TEST COMBINE TRADE/CASH PRD                  
         BNE   GRPOL48             NO                                           
         IC    RE,0(R4)                                                         
         N     RE,=X'0000007F'                                                  
         CLM   RE,1,PRDCD                                                       
         BNE   GRPOL48                                                          
*                                                                               
GRPOL44  CLI   SLNCD,0                                                          
         BE    GRPOL46                                                          
         CLC   SLNCD,1(R4)         RIGHT LENGTH                                 
         BNE   GRPOL48                                                          
*                                                                               
GRPOL46  IC    R8,1(R4)                                                         
         B     GRPOL60                                                          
*                                                                               
GRPOL48  LA    R4,4(R4)                                                         
         BCT   R0,GRPOL42                                                       
*                                                                               
         LTR   R8,R8                                                            
         BZ    GRPOL22             SPOT NOT ALLOCATED TO THIS PRD               
         EJECT                                                                  
GRPOL60  TM    RSTATUS,X'08'       TEST BRAND A PAYS ALL                        
         BO    GRPOL62                                                          
*                                                                               
         CLC   PRDCD,RPPRD         ARE WE DOING PRD 1                           
         BE    *+8                 YES                                          
         MVI   PRDSW,C'P'          INDICATE PASSIVE PARTNER                     
         B     GR12                                                             
*                                                                               
GRPOL62  DS    0H                  GIVE ALL COST TO PRODUCT A                   
         LA    R5,RPPRD                                                         
         SR    R4,R5                                                            
         SRL   R4,2                GET PARTNER NUMBER                           
         IC    R8,BDSEC            SET FULL SHARE                               
         LTR   R4,R4               TEST FOR PARTNER A                           
         BZ    GR12                YES - GIVE IT ALL THE COST                   
         SR    R8,R8               ELSE GIVE 0                                  
         B     GR12                                                             
         EJECT                                                                  
*===============================================================                
* SUBROUTINE TO COMPUTE GROSS OR NET FROM GIVEN VALUES                          
*===============================================================                
                                                                                
GN1      CLI   COS2IND,C'Y'          COST2 REQUESTED?                           
         BNE   GN1AA                 NO                                         
         TM    BDSTAT3,BDST3_COS2RT  COS2 TO HONOR RATE TYPE?                   
         BZ    GN1AA                 NO                                         
         BAS   RE,GETCOS2            YES - GET COS2 NOW!                        
*                                                                               
GN1AA    OC    BUYKEY+4(2),BUYKEY+4  TEST MARKET ZERO BUY                       
         BNZ   GN1A                                                             
         CLI   BDPURP,X'FD'          TEST RETURN COSTS                          
         BE    GN1A                                                             
         XC    PRDGRS,PRDGRS         RATES ARE ALWAYS ZERO                      
         XC    PRDNET,PRDNET                                                    
         B     GN10                                                             
*                                                                               
GN1A     L     R1,PRDGRS                                                        
         CLI   PRDCD,0             TEST BUY RATE REQUEST                        
         BE    GN1B                                                             
         TM    BDSTAT2,X'20'       TEST NON-TBS TRADE BUY                       
         BZ    GN1B                                                             
         CLI   RLEN,10             TEST SPOT ALLOCATED                          
         BNH   GN1B                                                             
         TM    RPPRD,X'80'         TEST ALLOCATED TO TRADE PRD                  
         BZ    GN1B                                                             
         XC    PRDNET,PRDNET                                                    
         B     GN10                                                             
*                                                                               
GN1B     TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BZ    GN1C                NO                                           
         XC    PRDNET,PRDNET       SET NET=0                                    
         B     GN10                                                             
*                                                                               
GN1C     TM    BDCIND2,X'04'       BDCIND IS CHARACTER?                         
         BZ    GN1D                NEIN                                         
*                                                                               
         CLI   BDCIND,BDCF         FEE RATE                                     
         BE    GN1E                                                             
         B     GN2                                                              
*                                                                               
GN1D     TM    BDCIND,X'80'        FEE RATE                                     
         BZ    GN2A                                                             
GN1E     ST    R1,PRDNET                                                        
         B     GN10                                                             
*                                                                               
GN2      CLI   BDCIND,BDCGRS       GROSS                                        
         BE    GN2B                                                             
         B     GN3                                                              
*                                                                               
GN2A     TM    BDCIND,X'20'        GROSS                                        
         BZ    GN3A                                                             
GN2B     M     R0,=F'85'                                                        
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         ST    R1,PRDNET                                                        
         B     GN10                                                             
*                                                                               
GN3      CLI   BDCIND,BDCN                                                      
         BE    GN3B                                                             
         B     GN5                                                              
*                                                                               
GN3A     TM    BDCIND,X'10'                                                     
         BZ    GN5A                                                             
GN3B     M     R0,=F'100'                                                       
         AHI   R1,42                                                            
         D     R0,=F'85'                                                        
         ST    R1,PRDGRS                                                        
         B     GN10                                                             
*                                                                               
GN5      CLI   BDCIND,BDCV                                                      
         BE    GN5B                                                             
         B     GN6                                                              
*                                                                               
GN5A     TM    BDCIND,X'08'                                                     
         BZ    GN6A                                                             
*                                                                               
GN5B     M     R0,=F'115'                                                       
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         ST    R1,PRDGRS                                                        
         B     GN10                                                             
*                                                                               
GN6      CLI   BDCIND,BDCS         SPECIAL                                      
         BE    GN6B                                                             
         B     GN7                                                              
*                                                                               
GN6A     TM    BDCIND,X'04'        SPECIAL?                                     
         BZ    GN7A                                                             
GN6B     M     R0,=F'85'                                                        
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         ST    R1,PRDNET                                                        
         ST    R1,PRDGRS                                                        
         B     GN10                                                             
*                                                                               
GN7      CLI   BDCIND,BDCX         X RATE                                       
         BE    GN7B                                                             
         B     GN8                                                              
*                                                                               
GN7A     TM    BDCIND,X'02'        X RATE                                       
         BZ    GN8A                                                             
*                                                                               
GN7B     M     R0,=F'85'                                                        
         AHI   R1,45                                                            
         D     R0,=F'90'                                                        
         ST    R1,PRDNET                                                        
         B     GN10                                                             
         EJECT                                                                  
GN8      CLI   BDCIND,BDCQ         Q RATE                                       
         BE    GN8B                                                             
         B     GN9                                                              
*                                                                               
GN8A     TM    BDCIND,X'40'        Q RATE? (5 PER CENT ON NET)                  
         BZ    GN9A                                                             
GN8B     M     R0,=F'85'                                                        
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         ST    R1,PRDNET                                                        
*                                                                               
         M     R0,=F'5'                                                         
         AHI   R1,50                                                            
         D     R0,=F'100'                                                       
         A     R1,PRDNET                                                        
         ST    R1,PRDGRS                                                        
         B     GN10                                                             
*                                                                               
GN9      CLI   BDCIND,BDCNTP       NTP RATE                                     
         BE    GN9B                                                             
         B     GN10                                                             
         EJECT                                                                  
GN9A     TM    BDCIND,X'FE'        NTP (P) RATE                                 
         BNZ   GN10                HAS NO BITS ON                               
*                                                                               
GN9B     CLC   =C'DF',BUYALPHA     TEST DFS                                     
         BNE   NTPX                                                             
*                                                                               
         SR    R0,R0                ASSUME NTP VALUE 0 (FOR NTP=0)              
         LA    RE,NTPLST7                                                       
         LHI   RF,(NTPLST7X-NTPLST7)/2                                          
*                                                                               
NTP2     CLC   BUYKEY+1(2),0(RE)   BUY KEY CLIENT TO LIST                       
         BE    NTP4                                                             
         BL    NTP10                                                            
         AHI   RE,2                                                             
         BCT   RF,NTP2                                                          
         B     NTP10                                                            
*                                                                               
         USING REGELEM,R2                                                       
NTP4     CLI   PRDCD,0             TEST BUY RATE REQUEST                        
         BE    NTP50                                                            
         CLI   1(R2),10            TEST SPOT ALLOCATED                          
         BNH   NTP50                                                            
         CLI   RPPRD,64            TEST NTP CLASS 0 (1-63)                      
         BL    NTP50                                                            
         LHI   R0,1700                                                          
         CLI   RPPRD,96            TEST NTP CLASS 1 (64-95)                     
         BL    NTP50                                                            
         CLC   RDATE,=X'D6BC'      AS OF MAY28/07                               
         BL    *+8                                                              
         LHI   R0,5000             RATE IS 50% FOR NTP2                         
         CLI   RPPRD,128           TEST NTP CLASS 2 (96-127)                    
         BL    NTP50                                                            
*                                                                               
         XR    R0,R0               NTP3 SAME AS NTP0                            
         CLI   RPPRD,201           TEST NTP CLASS 3 (128-200)                   
         BL    NTP50                                                            
         DC    H'0'                OTHER CLASSES NOT DEFINED                    
                                                                                
*** BE CAREFUL TO KEEP THIS LIST IN ORDER!                                      
*** AND I KNOW IT LOOKS WRONG THAT LN1 HAS A HIGHER VALUE THAN LN8              
*** THAT'S BECAUSE WE ONLY REALLY ALLOW 1-6. 7/8/9 GET CODED AS G/H/I           
*** AND THE ALPHA VALUES ARE LESS THAN THE NUMERIC ONES!                        
                                                                                
NTPLST7  DC    X'81C6'      AO7                                                 
         DC    X'81C7'      AO8                                                 
         DC    X'81C8'      AO9                                                 
         DC    X'8826'      CB7                                                 
         DC    X'8827'      CB8                                                 
         DC    X'8828'      CB9                                                 
         DC    X'8DA0'      DNA                                                 
         DC    X'8E4E'      DSO                                                 
         DC    X'9186'      EM7                                                 
         DC    X'9187'      EM8                                                 
         DC    X'9188'      EM9                                                 
         DC    X'9E4E'      HSO                                                 
         DC    X'ADA6'      LN7                                                 
         DC    X'ADA7'      LN8                                                 
         DC    X'ADBA'      LN1                                                 
         DC    X'B5A6'      NN7                                                 
         DC    X'B5A7'      NN8                                                 
         DC    X'B5A8'      NN9                                                 
         DC    X'BC66'      PD7                                                 
         DC    X'BC67'      PD8                                                 
         DC    X'BC68'      PD9                                                 
         DC    X'BEE6'      PX7                                                 
         DC    X'BEE7'      PX8                                                 
         DC    X'BEE8'      PX9                                                 
NTPLST7X EQU   *                                                                
*                                                                               
NTP10    LHI   R0,225              GROSS =  2.25 PCT                            
*                                                                               
         CLI   PRDCD,0             TEST BUY RATE REQUEST                        
         BE    NTP50                                                            
*                                                                               
         CLI   1(R2),10            TEST SPOT ALLOCATED                          
         BNH   NTP50                                                            
*                                                                               
         LHI   R0,570                                                           
         CLI   BDPURP,X'FF'        TEST SUPPRESS FEATURE                        
         BE    NTP50                                                            
*                                                                               
         CLC   =X'BE60',BUYKEY+1   TEST CLIENT PTA                              
         BE    NTPPTA                                                           
*                                                                               
         CLI   10(R2),64           TEST PRD IN NTP GROUP 0                      
         BL    NTP50               YES                                          
         LHI   R0,1500             NTP GROUP 1 VALUE IS 15.00                   
         CLC   2(2,R2),=X'CABC'    TEST PRIOR TO 05/28/01                       
         BL    *+8                                                              
         LHI   R0,1700             NOW IT'S 17.00                               
         CLI   10(R2),96                                                        
         BL    NTP50                                                            
         CLI   10(R2),128          NTP2 VALUE = NTP1 VALUE                      
         BL    NTP50                                                            
*                                                                               
         LHI   R0,570                                                           
         CLI   10(R2),201          NTP3 VALUE = NTP1 VALUE                      
         BL    NTP50                                                            
         DC    H'0'                                                             
*                                                                               
NTPPTA   LHI   R0,1500             NTP=1 FACTOR                                 
         CLC   2(2,R2),=X'CABC'    TEST PRIOR TO 05/28/01                       
         BL    *+8                                                              
         LHI   R0,1700             NOW IT'S 17.00                               
*                                                                               
         CLI   10(R2),160          160-191 ARE NTP 2 (=NTP1)                    
         BH    NTP50                                                            
*                                                                               
         CLI   10(R2),95           96-159 ARE NTP 1                             
         BH    NTP50                                                            
         CLI   10(R2),X'40'        AND RL                                       
         BE    NTP50                                                            
         CLI   10(R2),X'41'        AND FL                                       
         BE    NTP50                                                            
*                                                                               
         LHI   R0,570              01-95 ARE NTP=0                              
*                                                                               
NTP50    MR    R0,R0                                                            
         AHI   R1,5000                                                          
         D     R0,=F'10000'                                                     
         ST    R1,PRDGRS                                                        
         XC    PRDNET,PRDNET       NET = 0                                      
         B     GN10                                                             
*                                                                               
NTPX     M     R0,=F'925'                                                       
         AHI   R1,462                                                           
         D     R0,=F'1000'                                                      
         ST    R1,PRDNET                                                        
*                                                                               
GN10     CLI   COS2IND,C'Y'                                                     
         BNE   GN10X                                                            
         TM    BDSTAT3,BDST3_COS2RT  COS2 TO HONOR RATE TYPE?                   
         BNZ   GN10X                 YES - ALREADY GOT COS2                     
         BAS   RE,GETCOS2            GET COS2 NOW!                              
GN10X    TM    BDCIND2,X'20'         TEST CANAD                                 
         BNO   GN11                                                             
         OC    BUYKEY+4(2),BUYKEY+4  TEST NTWK LEVEL BUY (MKT=0)                
         BNZ   GN11                                                             
         TM    BDCIND2,X'01'         TEST COST IN PENNIES                       
         BO    GN11                                                             
         CLI   COSTOVRD,C'Y'         COST OVERRIDE                              
         BE    GN11                  YES - ALWAYS STORED IN PENNIES             
* CONVERT COST TO PENNIES (THANK YOU YRTO AND GRANT)                            
         L     R1,PRDGRS                                                        
         MHI   R1,100                                                           
         ST    R1,PRDGRS                                                        
         L     R1,PRDNET                                                        
         MHI   R1,100                                                           
         ST    R1,PRDNET                                                        
*                                                                               
GN11     CLI   BDLEN,70            CHECK FOR OLD BUYREC                         
         BLR   R9                                                               
                                                                                
*============================================================*                  
* COMPUTE GST AMOUNT                                         *                  
*============================================================*                  
                                                                                
GN12     TM    BDCIND2,X'04'       TEST BDCIND NUMERIC                          
         BZ    GN12A                                                            
         CLI   BDCIND,BDCC         TEST COMMISSION ONLY                         
         BE    GN12B                                                            
GN12A    TM    BDCIND2,X'80'       TEST COMMISSION ONLY                         
         BZ    GN14                                                             
GN12B    XC    PRDNET,PRDNET                                                    
         BR    R9                                                               
*                                                                               
GN14     L     R1,PRDNET                                                        
         SR    R0,R0                                                            
         ICM   R0,7,GSTRATE        GET GST RATE                                 
         BZ    GN16                                                             
         MR    R0,R0               GST RATE X NET                               
         L     RF,=F'100000'                                                    
         BAS   RE,DIV                                                           
         ST    R1,GSTAMT           SAVE GST AMOUNT                              
*                                                                               
GN16     TM    BDCIND2,X'04'       TEST BDCIND CHARACTER ?                      
         BZ    GN16A                                                            
         CLI   BDCIND,BDCC         TEST COMMISSION ONLY                         
         BE    GN36                YES - IGNORE TAX                             
*                                                                               
GN16A    TM    BDCIND2,X'80'       TEST COMMISSION ONLY                         
         BO    GN36                YES - IGNORE TAX (NET=0)                     
*                                                                               
         TM    BDCIND2,X'20'       TEST CANAD                                   
         BZ    GN30                                                             
         TM    BUYKEY,X'03'        TEST NTWK                                    
         BNO   GN30                                                             
         EJECT                                                                  
*========================================================*                      
* SEARCH FOR CANADIAN NETWORK TAX ELEMENT                *                      
*========================================================*                      
         SPACE 1                                                                
GN20     LA    RE,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
GN22     ICM   R0,1,1(RE)                                                       
         BZ    GN30                                                             
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    GN30                TRY FOR OLD TAX                              
         CLI   0(RE),X'69'                                                      
         BNE   GN22                                                             
*                                                                               
         ICM   R0,15,2(RE)         GET TAX AMOUNT                               
         ST    R0,PRDTAX           SAVE TAX AMOUNT                              
         MVI   CANETTAX,C'Y'                                                    
         B     GN34                                                             
*                                                                               
GN30     OC    BDNTAX,BDNTAX       TEST FOR NEW TAX                             
         BZ    GN36                NO                                           
                                                                                
* NEED TO COMPUTE TAX AMOUNT                                                    
                                                                                
GN31A    L     R1,PRDNET                                                        
         A     R1,GSTAMT                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BDNTAX                                                      
         MR    R0,R0               TAX X (NET+GST)                              
*                                                                               
         CLI   PRDCD,0             TEST BUY RATE REQUEST                        
         BE    GN32                YES - ROUND                                  
*                                                                               
GN32     A     R1,=F'50000'        ROUND                                        
         D     R0,=F'100000'       GIVES TAX IN R1                              
         ST    R1,PRDTAX           SAVE TAX AMOUNT                              
*                                                                               
GN34     L     R0,PRDGRS                                                        
         A     R0,PRDTAX                                                        
         ST    R0,PRDGRS                                                        
*                                                                               
         L     R0,PRDNET                                                        
         A     R0,PRDTAX                                                        
         ST    R0,PRDNET                                                        
*                                                                               
GN36     DS    0H                                                               
         BR    R9                                                               
         EJECT                                                                  
GR12     TM    BDCIND2,X'04'       TEST BDCIND CHARACTER ?                      
         BZ    GR12A                                                            
         TM    BDCIND2,BDC2NEG     TEST MINUS RATE                              
         BZ    GR14                                                             
         B     GR12B                                                            
*                                                                               
GR12A    TM    BDCIND,X'01'        TEST MINUS RATE                              
         BZ    GR14                                                             
GR12B    LM    R4,R6,PRDGRS                                                     
         LCR   R4,R4                                                            
         LCR   R5,R5                                                            
         LCR   R6,R6                                                            
         STM   4,6,PRDGRS                                                       
         L     R0,GSTAMT                                                        
         LCR   R0,R0                                                            
         ST    R0,GSTAMT                                                        
*                                                                               
GR14     CLI   PRDCD,0             TEST IF REQUEST FOR BUY RATES                
         BE    GR16                                                             
         TM    RSTATUS,X'80'       TEST MINUS SPOTS                             
         BC    8,GR16                                                           
         L     R4,SPOTS                                                         
         LCR   R4,R4                                                            
         ST    R4,SPOTS                                                         
*                                                                               
GR16     SR    R9,R9                                                            
         IC    R9,BDSEC                                                         
GR16X    LA    R1,PRDGRS                                                        
         LA    R0,3                                                             
         CLI   PRDSW,0             TEST PASSIVE PARTNER                         
         BE    GR18                NO                                           
         SR    R8,R9               COMPLEMENT SECONDS                           
         LPR   R8,R8               AND MAKE POSITITVE                           
         EJECT                                                                  
* R8 HAS SHARE OR SHARE COMPLEMENT IN SECONDS                                   
*                                                                               
GR18     L     R7,0(R1)            DOLLARS                                      
         MR    R6,R8               X SHARE                                      
         SLDA  R6,1                X 2                                          
         DR    R6,R9               DIVIDE BY TOT SEC                            
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AHI   R7,1                ROUND                                        
         SRA   R7,1                TRUNCATE                                     
         CLI   PRDSW,0             TEST PASSIVE PRD                             
         BE    *+10                NO                                           
         S     R7,0(R1)            SUBTRACT TOTAL DOLS                          
         LCR   R7,R7               AND REVERSE SIGN                             
         OC    SPOTS,SPOTS         TEST 0 SPOTS                                 
         BE    *+8                                                              
         M     R6,SPOTS                                                         
         ST    R7,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,GR18                                                          
*                                                                               
* NOW ADJUST GST AMOUNT SIMILARLY                                               
*                                                                               
         LA    R1,GSTAMT                                                        
         L     R7,0(R1)            DOLLARS                                      
         MR    R6,R8               X SHARE                                      
         SLDA  R6,1                X 2                                          
         DR    R6,R9               DIVIDE BY TOT SEC                            
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AHI   R7,1                ROUND                                        
         SRA   R7,1                TRUNCATE                                     
         CLI   PRDSW,0             TEST PASSIVE PRD                             
         BE    *+10                NO                                           
         S     R7,0(R1)            SUBTRACT TOTAL DOLS                          
         LCR   R7,R7               AND REVERSE SIGN                             
         OC    SPOTS,SPOTS         TEST 0 SPOTS                                 
         BE    *+8                                                              
         M     R6,SPOTS                                                         
         ST    R7,0(R1)                                                         
*                                                                               
GR20     TM    BDCIND2,X'04'       TEST IF NUMERIC                              
         BZ    GR20A                                                            
         TM    BDCIND2,BDC2NEG     TEST MINUS RATE                              
         BZ    GR22                                                             
         B     GR20B                                                            
*                                                                               
GR20A    TM    BDCIND,X'01'        TEST MINUS RATE                              
         BZ    GR22                                                             
GR20B    XC    SPOTS,SPOTS                                                      
         EJECT                                                                  
GR22     CLI   XCHSW,C'X'          IS EXCHANGE RATE AREA PRESENT                
         BE    *+12                                                             
         CLI   XCHSW,C'Z'                                                       
         BNE   GRX                                                              
         L     R1,SAVER1           YES - SO THIS IS CANADIAN AGY                
         SR    R4,R4                                                            
         ICM   R4,7,13(R1)         R4=A(EXCHANGE RATE AREA)                     
         BZ    GR40                                                             
         MVC   EXCHIND,12(R1)      MOVE REQUESTED CURRENCY TYPE                 
*                                                                               
         USING XCHAREAD,R4                                                      
*                                                                               
         CLI   XCHSW,C'Z'          TEST EXTENDED EXCHANGE AREA PASSED           
         BNE   *+8                                                              
         BAS   R9,GETPST           YES-GET PST RATES AND AMOUNTS                
*                                                                               
         MVC   XGROSS,PRDGRS       INITIALIZE TO ORIGINAL DOLLAR VALUES         
         MVC   XNET,PRDNET                                                      
         MVC   XTAX,PRDTAX                                                      
         XC    XC58,XC58                                                        
         MVC   XGSTCODE(4),GSTCODE  MOVE CODE AND RATE                          
         MVC   XGSTAMT,GSTAMT       MOVE GST AMOUNT IN PENNIES                  
*                                                                               
*                                                                               
         SR    R0,R0               LOCATE BUY EXCHANGE RATE ELEM                
         LA    R6,BDELEM                                                        
*                                                                               
GR24     CLI   0(R6),0                                                          
         BE    GR40                NOT FOUND                                    
         CLI   0(R6),XCHCODEQ                                                   
         BE    GR25                                                             
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     GR24                                                             
*                                                                               
         USING XCHELEM,R6                                                       
*                                                                               
GR25     CLI   EXCHIND,C'C'        TEST CANAD CURR REQ                          
         BNE   *+10                NO - DO NOT REPORT C58                       
         MVC   XC58+2(2),XCHC58    PICK UP C58 TAX RATE                         
         EJECT                                                                  
GR30     DS    0H                                                               
         SR    R0,R0               R0=EXCHANGE RATE (USA TO CANAD)              
         ICM   R0,3,XCHRATE                                                     
*                                                                               
         L     R1,SAVER1                                                        
         L     R7,8(R1)            POINT TO BUY ELEMENT                         
         OC    4(2,R7),4(R7)       TEST CLEARED                                 
         BZ    GR31                NO - USE DEFAULT XCH RATE                    
         SPACE 1                                                                
* SEARCH FOR EXCHANGE RATE ON CLEARANCE DATE                                    
         SPACE 1                                                                
         LA    RE,9(R6)            POINT TO EXTENDED RATES                      
         ZIC   RF,1(R6)            GET LENGTH OF XCH RATE ELEM                  
         AHI   RF,-9               ADJUST FOR FIXED PORTION                     
         BZ    GR31                                                             
         SRL   RF,2                SET FOR BCT                                  
*                                                                               
GR30A    CLC   4(2,R7),0(RE)       MATCH DATES                                  
         BE    GR30B                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,GR30A                                                         
         B     GR31                IF NOT FOUND, USE DEFAULT XCH RATE           
*                                                                               
GR30B    ICM   R0,3,2(RE)          PICK UP NEW XCH RATE                         
*                                                                               
GR31     CLI   EXCHIND,C'C'        TEST EXCHANGE INTO CN DOLLARS                
         BNE   GR31A                                                            
         CLI   XCHDTYP,C'C'        TEST BUY IN CN DOLLARS                       
         BE    GR31US              YES - GET US DOLLARS IN XCH AREA             
         B     GR31CN                                                           
         SPACE 1                                                                
* REQUEST IS FOR US DOLLARS                                                     
         SPACE 1                                                                
GR31A    CLI   XCHDTYP,C'U'        TEST BUY IN US DOLLARS                       
         BE    GR31CN              YES - GET CN DOLLARS IN XCH AREA             
         B     GR31US              ELSE GET US DOLLARS IN XCH AREA              
         EJECT                                                                  
*======================================================*                        
* GET CANADIAN DOLLARS IN XCH AREA                     *                        
*======================================================*                        
         SPACE 1                                                                
GR31CN   L     RF,PRDGRS           GROSS                                        
         BAS   R9,ADJCN                                                         
         ST    RF,XGROSS                                                        
*                                                                               
         L     RF,PRDNET           NET                                          
         BAS   R9,ADJCN                                                         
         ST    RF,XNET                                                          
*                                                                               
         L     RF,PRDTAX           NET                                          
         BAS   R9,ADJCN                                                         
         ST    RF,XTAX                                                          
*                                                                               
         L     RF,XGSTAMT          GST                                          
         BAS   R9,ADJCN                                                         
         ST    RF,XGSTAMTX                                                      
         B     GR34                                                             
*                                                                               
ADJCN    AR    RF,RF               X 2                                          
         MR    RE,R0               X EXCHANGE RATE                              
         D     RE,=F'10000'                                                     
         LTR   RF,RF               ROUND                                        
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         BR    R9                                                               
         EJECT                                                                  
* CALCULATE US DOLLARS FOR XCH AREA                                             
         SPACE 1                                                                
GR31US   LTR   R0,R0               TEST EXCHANGE RATE = 0                       
         BZ    GR40                YES-ERROR                                    
         L     RF,PRDGRS           GROSS                                        
         BAS   R9,ADJUS                                                         
         ST    RF,XGROSS                                                        
*                                                                               
         L     RF,PRDNET           NET                                          
         BAS   R9,ADJUS                                                         
         ST    RF,XNET                                                          
*                                                                               
         L     RF,PRDTAX           TAX                                          
         BAS   R9,ADJUS                                                         
         ST    RF,XTAX                                                          
*                                                                               
         L     RF,XGSTAMT          GST                                          
         BAS   R9,ADJUS                                                         
         ST    RF,XGSTAMTX                                                      
         B     GR34                                                             
*                                                                               
ADJUS    M     RE,=F'20000'        X 10000 X 2                                  
         DR    RE,R0               / EXCHANGE RATE                              
         LTR   RF,RF               ROUND                                        
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         BR    R9                                                               
         SPACE 1                                                                
*======================================================*                        
* IF REQUESTED AND BUY CURRENCIES ARE DIFFERENT        *                        
*    SWITCH GROSS/NET/TAX                              *                        
*======================================================*                        
         SPACE 1                                                                
GR34     CLC   EXCHIND,XCHDTYP     TEST CONVERSION NECESSARY                    
         BE    GRX                 NO - DONE                                    
         LM    R0,R2,PRDGRS                                                     
         MVC   PRDGRS(12),XGROSS                                                
         STM   R0,R2,XGROSS                                                     
         L     R0,XGSTAMT                                                       
         MVC   XGSTAMT,XGSTAMTX                                                 
         ST    R0,XGSTAMTX                                                      
         B     GRX                                                              
*                                                                               
GR40     L     R1,SAVER1                                                        
         CLI   EXCHIND,C'C'        TEST CANADIAN CURRENCY REQ                   
         BE    GRX                 NO XCH ELEM SO BUY IS IN CAN $               
         MVI   12(R1),X'FF'        RETURN ERROR CODE                            
*                                                                               
GRX      XIT1                                                                   
GRERR    DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
GETCOS2  NTR1                                                                   
*                                                                               
         LA    RE,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
GETCST10 ICM   R0,1,1(RE)                                                       
         BZ    GETCSTX             RECORD IS NOT TOO GOOD                       
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    GETCSTX             SECOND COST NOT IN THIS RECORD               
         CLI   0(RE),X'71'                                                      
         BNE   GETCST20                                                         
         MVC   PRDGRS,2(RE)        EXTRACT PENNIES                              
         L     R1,PRDGRS                                                        
         TM    BDSTAT3,BDST3_COS2RT COS2 TO HONOR RATE TYPE?                    
         BNZ   GETCST15             YES - NET=GROSS                             
         M     R0,=F'85'                                                        
         LA    RF,100                                                           
         BAS   RE,DIV                                                           
GETCST15 ST    R1,PRDNET                                                        
*                                                                               
         TM    BDCIND2,X'10'       RATE IN DOLLARS?                             
         BZ    GETCSTX             NO                                           
         L     R0,PRDGRS                                                        
         C     R0,MAXDOLS          TEST COST TO MAX THAT WORKS                  
         BNH   *+6                                                              
         SR    R0,R0               IF > MAX, JUST SET COST TO 0                 
         MHI   R0,100              CONVERT RATE TO PENNIES                      
         ST    R0,PRDGRS                                                        
*                                                                               
         L     R0,PRDNET                                                        
         C     R0,MAXDOLS          TEST COST TO MAX THAT WORKS                  
         BNH   *+6                                                              
         SR    R0,R0               IF > MAX, JUST SET COST TO 0                 
         MHI   R0,100              CONVERT RATE TO PENNIES                      
         ST    R0,PRDNET                                                        
         B     GETCSTX                                                          
*                                                                               
GETCST20 CLI   0(RE),X'73'                                                      
         BNE   GETCST10                                                         
         CLC   2(4,RE),=X'80000000'  ZERO SOMETIMES LOOKS LIKE THIS!            
         BNE   *+8                                                              
         NI    2(RE),X'7F'                                                      
         MVC   FULL,2(RE)          EXTRACT PERCENTAGE                           
*                                                                               
         L     R0,PRDGRS           ALREADY ADJUSTED TO PENNIES                  
         BAS   RE,COMPCOS2                                                      
         C     R0,MAXDOL2          TEST COST TO MAX THAT WORKS                  
         BNH   *+6                                                              
         SR    R0,R0               IF > MAX, JUST SET COST TO 0                 
         ST    R0,PRDGRS                                                        
*                                                                               
         L     R0,PRDNET           ALREADY ADJUSTED TO PENNIES                  
         BAS   RE,COMPCOS2                                                      
         C     R0,MAXDOL2          TEST COST TO MAX THAT WORKS                  
         BNH   *+6                                                              
         SR    R0,R0               IF > MAX, JUST SET COST TO 0                 
         ST    R0,PRDNET                                                        
*                                                                               
GETCSTX  B     GRX                                                              
                                                                                
*============================================================*                  
* COMPUTE COST2 FROM FACTOR                                  *                  
*============================================================*                  
COMPCOS2 CVD   R0,DUB              MAKE AMOUNT PACKED                           
         ZAP   P16,DUB                                                          
         L     R0,FULL             GET COS2 FACTOR                              
         CVD   R0,DUB              MAKE IT PACKED                               
         MP    P16,DUB             GROSS/NET X FACTOR                           
         SRP   P16,64-6,5                                                       
         ZAP   DUB,P16                                                          
         CVB   R0,DUB                                                           
         BR    RE                                                               
*=======================================================*                       
* ON ENTRY R0/R1 HAVE DIVIDEND                          *                       
* RF HAS DIVISOR. NOTE IF RF=0, RESULT IS 0             *                       
*=======================================================*                       
         SPACE 1                                                                
DIV      LTR   RF,RF                                                            
         BNZ   DIV2                                                             
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
DIV2     SLDA  R0,1                X 2                                          
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE FINDS GST CODE AND APPLICABLE RATE IF A     *                      
* SPOT ELEMENT LOOK-UP                                   *                      
*========================================================*                      
         SPACE 1                                                                
GETGST   DS    0H                                                               
         MVI   GSTCODE,C'S'        SET DEFAULT CODE                             
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
GETGST2  ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GETGST4                                                          
         CLI   0(R6),X'6A'         TEST GST OVERRIDE ELEMENT                    
         BNE   GETGST2                                                          
         MVC   GSTCODE,2(R6)       SAVE OVERRIDE CODE                           
         CLI   GSTCODE,C' '                                                     
         BH    *+8                                                              
         MVI   GSTCODE,C'S'                                                     
*                                                                               
GETGST4  CLI   PRDCD,0             TEST REQUEST FOR BUY DESC RATES              
         BE    GETGSTX             YES - JUST RETURN CODE                       
*                                                                               
         LA    R1,GSTTAB           POINT TO TABLE                               
         LA    R0,GSTTABN          SET NUMBER OF ENTRIES                        
*                                                                               
GETGST10 CLC   GSTCODE,0(R1)                                                    
         BNE   GETGST12                                                         
*                                                                               
         CLC   2(2,R2),4(R1)       COMPARE TO EFFECTIVE DATE                    
         BL    GETGST12            PRIOR - DON'T USE THIS RATE                  
         MVC   GSTRATE,1(R1)       ELSE SAVE RATE                               
*                                                                               
GETGST12 LA    R1,L'GSTTAB(R1)                                                  
         BCT   R0,GETGST10                                                      
*                                                                               
GETGSTX  BR    R9                                                               
*                                                                               
GETGSTNO MVI   GSTCODE,0                                                        
         XC    GSTRATE,GSTRATE                                                  
         BR    R9                                                               
*                                                                               
GSTTAB   DS    0XL6                                                             
         DC    C'S',AL3(07000),X'B59F'  DEC31/90                                
         DC    C'S',AL3(06000),X'D4DA'  JUN26/06 - MOS JUL/06                   
         DC    C'S',AL3(05000),X'D79F'  DEC31/07 - MOS JAN/08                   
         DC    C'X',AL3(00000),X'0000'                                          
         DC    C'Z',AL3(00000),X'0000'                                          
GSTTABN  EQU   (*-GSTTAB)/L'GSTTAB                                              
         EJECT                                                                  
*=====================================================================*         
* SUBROUTINE GETS PST CODES AND RATES AND APPLIES RATES TO            *         
* NET+GST TO GET PST AMOUNTS                                          *         
* 06MAR97 AS THERE ARE NO PROVINCES WITH MULTIPLE PST TAXES, CHANGE   *         
*         CODE TO STOP AFTER IT FINDS THE FIRST NON-ZERO PST CODE     *         
*=====================================================================*         
         SPACE 1                                                                
*          DATA SET SPGETRATEN AT LEVEL 041 AS OF 11/14/12                      
         SPACE 1                                                                
GETPST   DS    0H                                                               
         USING XCHAREAD,R4                                                      
         LA    R0,10               CLEAR AREA FOR 10 PROVINCES                  
         LA    R1,XPSTPROV                                                      
         XC    0(XPSTLEN,R1),0(R1)                                              
         LA    R1,XPSTLEN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R6,BDELEM           FIND PST ELEMENT IN BUY                      
         SR    R0,R0                                                            
*                                                                               
GETPST2  CLI   0(R6),0                                                          
         BE    GETPSTX                                                          
         CLI   0(R6),X'6B'                                                      
         BE    GETPST3                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETPST2                                                          
*                                                                               
         USING PSTELEM,R6                                                       
GETPST3  LA    R0,L'PSTVALS        LOOP THROUGH PROVINCES                       
         LA    R5,PSTVALS                                                       
         LA    R7,PROVTAB                                                       
         LA    R8,XPSTPROV                                                      
         USING XPSTPROV,R8                                                      
*                                                                               
GETPST4  CLI   0(R5),0             TEST NO ENTRY                                
         BE    GETPST20                                                         
         CLI   0(R5),C'X'          IF CODE = X OR Z, LEAVE AS NULLS             
         BE    GETPST20                                                         
         LA    R1,PSTTAB           SEARCH PST TABLE FOR RATE                    
         LA    RF,PSTTABN                                                       
         XC    PSTRATE,PSTRATE                                                  
*                                                                               
GETPST6  CLC   0(2,R1),0(R7)       MATCH PROVINCE                               
         BNE   GETPST8                                                          
         CLC   2(1,R1),0(R5)       MATCH PST CODE                               
         BNE   GETPST8                                                          
         CLC   2(2,R2),6(R1)       COMPARE BUY DATE TO EFFECTIVE DATE           
         BL    GETPST8             PRIOR - DON'T USE THIS RATE                  
         MVC   PSTRATE,3(R1)                                                    
         MVC   PSTDATE,6(R1)                                                    
*                                                                               
GETPST8  LA    R1,L'PSTTAB(R1)                                                  
         BCT   RF,GETPST6                                                       
*                                                                               
         OC    PSTRATE,PSTRATE     TEST FOUND A RATE                            
         BZ    GETPST20            NO                                           
*                                                                               
         MVC   XPSTPROV,0(R7)      YES-SET PROVINCE CODE                        
         MVC   XPSTCODE,0(R5)      PST CODE                                     
         MVC   XPSTRATE,PSTRATE    PST RATE                                     
*------>                                                                        
         CLI   XPSTCODE,C'H'       HST IMPLIES NO GST                           
         BNE   *+10                                                             
         XC    GSTAMT,GSTAMT                                                    
*------>                                                                        
         MVI   EXCLGST,C'N'                                                     
         CLC   =C'PQ',XPSTPROV     TEST IT'S PQ                                 
         BNE   GETPST10                                                         
         CLC   PSTDATE,=X'E19F'    PRIOR TO 31DEC12                             
         BL    GETPST10                                                         
         MVI   EXCLGST,C'Y'        EXCLUDE GST FROM JAN13 -->                   
*------>                                                                        
GETPST10 ICM   RF,15,PRDTAX       IF THERE'S TAX FROM EARLIER                   
         BZ    GETPST12           (NOT CANADIAN NETWORK TAX),                   
         CLI   CANETTAX,C'Y'       THEN GET RID OF IT                           
         BE    GETPST12                                                         
         L     RE,PRDGRS                                                        
         SR    RE,RF                                                            
         ST    RE,PRDGRS                                                        
         L     RE,PRDNET                                                        
         SR    RE,RF                                                            
         ST    RE,PRDNET                                                        
         XC    PRDTAX,PRDTAX                                                    
         XC    XTAX,XTAX                                                        
*                                                                               
GETPST12 SR    RE,RE               CALCULATE PST AMOUNT                         
         ICM   RE,7,PSTRATE                                                     
         BZ    GETPST20                                                         
         L     RF,PRDNET           BASED ON NET + GST                           
         CLI   EXCLGST,C'Y'                                                     
         BE    *+8                                                              
         A     RF,GSTAMT                                                        
         AR    RE,RE                                                            
         MR    RE,RE                                                            
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,XPSTAMT          PST AMOUNT                                   
         B     GETPSTX                                                          
*                                                                               
GETPST20 LA    R5,1(R5)            NEXT PROVINCE                                
         LA    R7,2(R7)                                                         
         LA    R8,XPSTLEN(R8)                                                   
         BCT   R0,GETPST4                                                       
*                                                                               
GETPSTX  BR    R9                                                               
         SPACE 2                                                                
PSTTAB   DS    0CL8                                                             
         DC    CL2'BC',C'H',AL3(12000),X'DCDC'    JUN28/10                      
         DC    CL2'BC',C'H',AL3(00000),X'E281'    APR01/13                      
*                                                                               
         DC    CL2'NB',C'H',AL3(15000),X'C27F'    MAR31/97                      
         DC    CL2'NB',C'H',AL3(14000),X'D4DA'    JUN26/06                      
         DC    CL2'NB',C'H',AL3(13000),X'D79F'    DEC31/07                      
         DC    CL2'NB',C'H',AL3(15000),X'E8DB'    JUN27/16                      
*                                                                               
         DC    CL2'NF',C'H',AL3(15000),X'C27F'    MAR31/97                      
         DC    CL2'NF',C'H',AL3(14000),X'D4DA'    JUN26/06                      
         DC    CL2'NF',C'H',AL3(13000),X'D79F'    DEC31/07                      
         DC    CL2'NF',C'H',AL3(15000),X'E8DB'    JUN27/16                      
*                                                                               
         DC    CL2'NS',C'H',AL3(15000),X'C27F'    MAR31/97                      
         DC    CL2'NS',C'H',AL3(14000),X'D4DA'    JUN26/06                      
         DC    CL2'NS',C'H',AL3(13000),X'D79F'    DEC31/07                      
         DC    CL2'NS',C'H',AL3(15000),X'DCDC'    JUN28/10                      
*                                                                               
         DC    CL2'ON',C'H',AL3(13000),X'DCDC'    JUN28/10                      
*                                                                               
         DC    CL2'PQ',C'S',AL3(04000),X'BB61'    NOV01/93                      
         DC    CL2'PQ',C'S',AL3(06500),X'BCBE'    MAY30/94                      
         DC    CL2'PQ',C'S',AL3(07500),X'C39D'    DEC29/97                      
         DC    CL2'PQ',C'S',AL3(08500),X'DD9B'    DEC27/10                      
         DC    CL2'PQ',C'S',AL3(09500),X'DF9A'    DEC26/11                      
         DC    CL2'PQ',C'S',AL3(09975),X'E19F'    DEC31/12                      
*                                                                               
         DC    CL2'PE',C'H',AL3(14000),X'E281'    APR01/13                      
         DC    CL2'PE',C'H',AL3(15000),X'E93A'    SEP26/16                      
*                                                                               
PSTTABN  EQU   (*-PSTTAB)/L'PSTTAB                                              
         SPACE 2                                                                
PROVTAB  DC    CL2'BC'             BRITISH COLUMBIA                             
         DC    CL2'AL'             ALBERTA                                      
         DC    CL2'SA'             SASKATCHEWAN                                 
         DC    CL2'MA'             MANITOBA                                     
         DC    CL2'ON'             ONTARIO                                      
         DC    CL2'PQ'             QUEBEC                                       
         DC    CL2'NB'             NEW BRUNSWICK                                
         DC    CL2'NS'             NOVA SCOTIA                                  
         DC    CL2'PE'             PRINCE EDWARD ISLAND                         
         DC    CL2'NF'             NEWFOUNDLAND                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
PRDCD    DS    C                                                                
SLNCD    DS    C                                                                
PRDSW    DS    C                                                                
NETPAKCD DS    C                                                                
XCHSW    EQU   NETPAKCD            C'X' = 4TH PARM IS A(EXCHANGE AREA)          
EXCHIND  DS    C                                                                
FULL     DS    F                                                                
SAVER1   DS    A                                                                
GSTAMT   DS    F                                                                
GSTCODE  DS    C                                                                
GSTRATE  DS    XL3                                                              
PSTRATE  DS    XL3                                                              
PSTDATE  DS    XL2                                                              
PSTFOUND DS    CL1                                                              
ANYPST   DS    CL1                                                              
CANETTAX DS    CL1                                                              
COS2IND  DS    CL1                                                              
COSTOVRD DS    CL1                                                              
EXCLGST  DS    CL1                                                              
         DS    0D                                                               
P16      DS    PL16                                                             
WORKX    EQU   *                                                                
         SPACE 2                                                                
COSTAREA DSECT                                                                  
SPOTS    DS    F                                                                
PRDGRS   DS    F                                                                
PRDNET   DS    F                                                                
PRDTAX   DS    F                                                                
         SPACE 2                                                                
XCHAREAD DSECT                                                                  
       ++INCLUDE SPXCHAREA                                                      
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPGETRATE 06/09/16'                                      
         END                                                                    
