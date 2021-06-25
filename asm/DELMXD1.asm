*          DATA SET DELMXD1    AT LEVEL 005 AS OF 10/06/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXD1A                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICEDPT, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: THIS SOURCE MODULE USED TO BE CALLED DELMXD01                 *         
***********************************************************************         
DAYPRTX  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE35+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE35           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         BC    0,MAIN10            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
*                                                                               
         XC    PREV_WK,PREV_WK                                                  
         MVI   WK#,C'1'                                                         
         MVI   NEXT_REC,C'Y'                                                    
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
         USING WD_LMDSECT,R3                                                    
*                                                                               
         CLC   WD_KCODE,=C'38'     DAYPART CUME RECORDS                         
         BE    DP01                                                             
         CLC   WD_KCODE,=C'37'     DAYPART HUT/OUT RECORDS                      
         BE    DP01                                                             
         B     KEEPREC                                                          
*                                                                               
************************************************************                    
* .   CALCULATE THE WEEK NUMBER FOR THE CURRENT RECORD                          
* ..  STORE DDS QHR IN KEY FOR SORTING                                          
* ... CHANGE DATE ON RECORD TO MATCH 24 HOUR SAMPLE DATE                        
************************************************************                    
DP01     DS    0H                                                               
         CLI   NEXT_REC,C'Y'                                                    
         BNE   DP36                                                             
         MVI   WK#,C'1'                                                         
         MVI   DAY#,C'0'                                                        
                                                                                
*                                                                               
DP02     LA    R5,DPTABLE                                                       
         USING DPTTABD,R5                                                       
DP04     CLI   0(R5),0                ONLY KEEP DAYPARTS WE CARE                
         BNE   DP06                                                             
* IGNORE DAYPARTS WE DONT HAVE IN OUR TABLE                                     
         MVI   NEXT_REC,C'Y'                                                    
         B     DELREC                                                           
* IF DAYPART IS NOT IN TABLE THAT MEANS WE DONT CARE ABOUT IT                   
* JUST SIMPLY STORE THE NAME AS "NOT FOUND"                                     
         MVC   WD_PNRPNAME,=C'NOT FOUND-OMIT'                                   
         MVC   SVPNAME,WD_PNRPNAME                                              
         B     DP36                                                             
DP06     CLC   WD_CDRDPART,0(R5)                                                
         BNE   DP09                                                             
                                                                                
* MATCH DAYPART TO TABLE ENTRY - NOW HAVE TO MATCH TIMEZONE                     
         LA    RE,DPTIMEZ                                                       
         LA    R0,1                                                             
DP08     CLC   WD_MHQMKTZ,0(RE)                                                 
         BE    DP10                                                             
         AHI   RE,L'WD_MHQMKTZ                                                  
         AHI   R0,1                                                             
         CHI   R0,6                   ONLY 6 TIME ZONES                         
         BH    DP09                                                             
         B     DP08                                                             
                                                                                
DP09     AHI   R5,DPTABLN                                                       
         B     DP04                                                             
* FOUND TABLE ENTRY FOR CURRENT DAYPART RECORD                                  
DP10     MVC   WD_PNRPNAME,DPINTPN    MOVE IN PROGRAM NAME                      
         MVC   SVPNAME,WD_PNRPNAME                                              
         MVC   TOTCOUNT,=H'0'                                                   
         MVC   WD_CDRKDSEQ,DPINTDAY   INTERNAL DAY/SQH/EQH DAYPART CODE         
         MVC   WD_CDRIDPTC,DPINTDPT   INTERNAL DAYPART CODE                     
* FOR HUT/PUT LEVEL RECORD JUST EXIT HERE AND DONT EXPLODE TO                   
* QTR HOUR LEVELS                                                               
******   CLC   WD_KCODE,=C'37'     DAYPART HUT/OUT RECORDS                      
******   BE    KEEPREC                                                          
*                                                                               
         MVC   TMPDAY1,DPDAYS1                                                  
         MVC   TMPSQH1,DPINTSQ1                                                 
         MVC   TMPEQH1,DPINTEQ1                                                 
         CLI   TMPEQH1,QTR_5A                                                   
         BNE   *+8                                                              
         MVI   TMPEQH1,96                                                       
         ZIC   RE,TMPEQH1                                                       
         SHI   RE,1                                                             
         STC   RE,TMPEQH1                                                       
         MVC   TMPDAY2,DPDAYS2                                                  
         MVC   TMPSQH2,DPINTSQ2                                                 
         MVC   TMPEQH2,DPINTEQ2                                                 
         CLI   TMPEQH2,QTR_5A                                                   
         BNE   *+8                                                              
         MVI   TMPEQH2,96                                                       
         ZIC   RE,TMPEQH2                                                       
         SHI   RE,1                                                             
         STC   RE,TMPEQH2                                                       
DP16     MVC   CURRDAY1,TMPDAY1                                                 
         MVC   CURRDAY2,TMPDAY2                                                 
DP20     MVC   CURRSQH1,TMPSQH1                                                 
         MVC   CURRSQH2,TMPSQH2                                                 
         MVC   CURREQH1,TMPEQH1                                                 
         MVC   CURREQH2,TMPEQH2                                                 
         ZIC   RE,TMPSQH1                                                       
         SHI   RE,1                                                             
         STC   RE,CURRSQH1                                                      
                                                                                
DP24     CLI   DAY#,C'8'          IF WE ARE DONE WITH ALL DAYS                  
         BH    DP42               THEN PROCESS NEXT WEEK                        
         ZICM  RF,CURRDAY1,(12)                                                 
         MVI   KEEPDAY,C'N'                                                     
         TM    CURRDAY1,X'80'                                                   
         BNO   *+12                                                             
         MVI   KEEPDAY,C'Y'                                                     
         B     DP36                                                             
                                                                                
         ZIC   RE,DAY#                                                          
         AHI   RE,1                                                             
         STC   RE,DAY#                                                          
         SLL    RF,1                                                            
         STCM   RF,12,CURRDAY1                                                  
         B      DP24                                                            
                                                                                
         DROP  R5                                                               
*                                                                               
DP36     DS    0C                                                               
*                                                                               
         MVC   WD_PNRPNAME,SVPNAME                                              
         MVC   WD_KWEEK,WK#                                                     
         MVC   WD_KDAY,DAY#                                                     
* DO NOT BUMP TO NEXT DAY  UNTIL WE ARE DONE WITH ALL THE QHRS                  
* IN THE DAY                                                                    
         CLI   DAY#,C'8'          IF WE ARE DONE WITH ALL DAYS                  
         BH    DP42               THEN PROCESS NEXT WEEK                        
         MVI   NEXT_REC,C'N'                                                    
         ZIC   RE,CURRSQH1                                                      
         AHI   RE,1                                                             
         STC   RE,CURRSQH1                                                      
         STC   RE,WD_KQHR                                                       
         CLC   CURRSQH1,CURREQH1                                                
         BH    DP40                                                             
*&&DO                                                                           
* TEST TO SHOW THE COUNT OF THE EXPLODED RECORDS                                
         ZICM  RE,TOTCOUNT,(3)         TEMP DELETE LATER                        
         AHI   RE,1                                                             
         STCM  RE,3,TOTCOUNT                                                    
         MVC   WD_KRPRD,TOTCOUNT                                                
*&&                                                                             
*                                                                               
         B     ADDREC                                                           
*                                                                               
* BUMP TO NEXT DAY WHEN ALL QHRS ARE DONE                                       
DP40     ZIC   RE,DAY#                                                          
         AHI   RE,1                                                             
         STC   RE,DAY#                                                          
         ZICM  RF,CURRDAY1,(12)                                                 
         SLL   RF,1                                                             
         STCM  RF,12,CURRDAY1                                                   
         B     DP20                                                             
                                                                                
                                                                                
* DONE WITH ALL QHRS/ ALL DAYS FOR CURRENT WEEK- BUMP TO NEXT WEEK              
* IF DONE WITH ALL QHRS FOR 1ST ROTATION CHECK IF THERE IS A SECOND             
* ROTATION TO PROCESS FOR THIS DAYPART                                          
DP42     OC    CURRDAY2,CURRDAY2                                                
         BZ    DP44                                                             
         MVC   CURRDAY1,CURRDAY2                                                
         ZIC   RE,CURRSQH2                                                      
         SHI   RE,1                                                             
         STC   RE,CURRSQH1                                                      
         MVC   CURREQH1,CURREQH2  SET END QHR TO 2ND ROTATION END QHR           
         XC    CURRDAY2,CURRDAY2  CLEAR THIS OUT - DO ONLY ONCE                 
         MVI   DAY#,C'0'                                                        
         B     DP24                                                             
DP44     ZIC   RE,WK#                                                           
         AHI   RE,1                                                             
         STC   RE,WK#                                                           
         MVI   DAY#,C'0'                                                        
*                                                                               
         CLI   WK#,C'4'                                                         
         BNH   DP16                                                             
         MVI   NEXT_REC,C'Y'                                                    
         B     DELREC                                                           
*                                                                               
************************************************************                    
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0:  KEEP RECORD                       
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4:  DELETE RECORD                     
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: ADD RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
*----------------------------------------------------------------------         
         EJECT                                                                  
         ORG   DAYPRTX+(((*-DAYPRTX)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE35   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
TMPDAY1  DS    BL2                                                              
TMPSQH1  DS    X                                                                
TMPEQH1  DS    X                                                                
TMPDAY2  DS    BL2                                                              
TMPSQH2  DS    X                                                                
TMPEQH2  DS    X                                                                
CURRDAY1 DS    BL2                                                              
CURRSQH1  DS    X                                                               
CURREQH1  DS    X                                                               
CURRDAY2 DS    BL2                                                              
CURRSQH2  DS    X                                                               
CURREQH2  DS    X                                                               
SVPNAME  DS    CL(L'WD_PNRPNAME)                                                
         SPACE 3                                                                
*                                                                               
WK#      DS    CL1                                                              
DAY#     DS    CL1                                                              
KEEPDAY  DS    CL1                                                              
PREV_WK  DS    XL1                                                              
NEXT_REC DS    CL1                 DONE WITH THIS PROGRAM. GET ANOTHER          
*                                                                               
TOTCOUNT DS    XL2                                                              
*                                                                               
                                                                                
*---------------------------------------------------------------                
* THESE ARE THE SAME DAYPARTS FROM THE OLD CONVERSION PROCESS                   
* WE ARE PROCESSING ALL AVAILABLE DAYPARTS ON THE FILE SINCE                    
* PROPOSER ONLY CARES ABOUT THE OLD ONES                                        
DPTABLE  DS    0H                                                               
*                                                                               
         DC    C'10001000',X'15',AL1(QTR_5A,QTR_6A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 6-7A      '                                             
         DC    BL2'0111110000000000',AL1(QTR_6A,QTR_7A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(01)                                                          
                                                                                
         DC    C'10020500',X'15',AL1(QTR_5A,QTR_8A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 6-9A      '                                             
         DC    BL2'0111110000000000',AL1(QTR_6A,QTR_9A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(02)                                                          
*                                                                               
         DC    C'10022000',X'15',AL1(QTR_6A,QTR_8A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 7-9A      '                                             
         DC    BL2'0111110000000000',AL1(QTR_7A,QTR_9A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(03)                                                          
*                                                                               
         DC    C'10023000',X'15',AL1(QTR_8A,QTR_11A)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 9A-12N    '                                             
         DC    BL2'0111110000000000',AL1(QTR_9A,QTR_12P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(04)                                                          
*                                                                               
         DC    C'10026800',X'15',AL1(QTR_11A,QTR_2P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 12N-3P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_12P,QTR_3P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(05)                                                          
*                                                                               
         DC    C'10027000',X'15',AL1(QTR_11A,QTR_2P)                            
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 12N-3P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_12P,QTR_3P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(05)                                                          
*                                                                               
         DC    C'10027000',X'15',AL1(QTR_11A,QTR_3P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 12N-4P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_12P,QTR_4P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(06)                                                          
*                                                                               
         DC    C'10030000',X'15',AL1(QTR_2P,QTR_4P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 3-5P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_3P,QTR_5P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(07)                                                          
*                                                                               
         DC    C'10030900',X'15',AL1(QTR_3P,QTR_5P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 4-6P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_4P,QTR_6P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(08)                                                          
*                                                                               
         DC    C'10032000',X'15',AL1(QTR_3P,QTR_630P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 4-730P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_4P,QTR_730P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(09)                                                          
         DC    C'10032000',X'15',AL1(QTR_2P,QTR_530P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 3-630P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_3P,QTR_630P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(09)                                                          
*                                                                               
         DC    C'10033400',X'15',AL1(QTR_4P,QTR_5P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 5-6P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_5P,QTR_6P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(10)                                                          
         DC    C'10033400',X'15',AL1(QTR_3P,QTR_4P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 4-5P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_4P,QTR_5P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(10)                                                          
*                                                                               
         DC    C'10035000',X'15',AL1(QTR_5P,QTR_530P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 6-630P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_6P,QTR_630P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(11)                                                          
         DC    C'10035000',X'15',AL1(QTR_4P,QTR_430P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 5-530P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_5P,QTR_530P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(11)                                                          
*                                                                               
         DC    C'10036000',X'15',AL1(QTR_5P,QTR_630P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 6-730P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_6P,QTR_730P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(12)                                                          
         DC    C'10036000',X'15',AL1(QTR_4P,QTR_530P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 5-630P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_5P,QTR_630P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(12)                                                          
*                                                                               
         DC    C'10037000',X'15',AL1(QTR_5P,QTR_7P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 6-8P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_6P,QTR_8P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(13)                                                          
         DC    C'10037000',X'15',AL1(QTR_4P,QTR_6P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 5-7P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_5P,QTR_7P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(13)                                                          
*                                                                               
         DC    C'10039000',X'15',AL1(QTR_6P,QTR_630P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 7-730P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_7P,QTR_730P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(14)                                                          
         DC    C'10039000',X'15',AL1(QTR_5P,QTR_530P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 6-630P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_6P,QTR_630P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(14)                                                          
*                                                                               
         DC    C'10039675',X'15',AL1(QTR_6P,QTR_9P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 7-10P     '                                             
         DC    BL2'0111110000000000',AL1(QTR_7P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(15)                                                          
*                                                                               
         DC    C'10040000',X'15',AL1(QTR_630P,QTR_7P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 730-8P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_730P,QTR_8P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(16)                                                          
         DC    C'10040000',X'15',AL1(QTR_530P,QTR_6P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 630-7P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_630P,QTR_7P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(16)                                                          
*                                                                               
         DC    C'10040800',X'15',AL1(QTR_7P,QTR_9P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 8-10P     '                                             
         DC    BL2'0111110000000000',AL1(QTR_8P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(17)                                                          
         DC    C'10040800',X'15',AL1(QTR_6P,QTR_8P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 7-9P      '                                             
         DC    BL2'0111110000000000',AL1(QTR_7P,QTR_9P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(17)                                                          
*                                                                               
         DC    C'10041000',X'15',AL1(QTR_7P,QTR_10P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 8-11P     '                                             
         DC    BL2'0111110000000000',AL1(QTR_8P,QTR_11P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(18)                                                          
         DC    C'10041000',X'15',AL1(QTR_6P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 7-10P     '                                             
         DC    BL2'0111110000000000',AL1(QTR_7P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(18)                                                          
*                                                                               
         DC    C'10041450',X'15',AL1(QTR_7P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 8-10P     '                                             
         DC    BL2'0111110000000000',AL1(QTR_8P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(19)                                                          
*                                                                               
         DC    C'10042500',X'15',AL1(QTR_9P,QTR_10P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 10-11P    '                                             
         DC    BL2'0111110000000000',AL1(QTR_10P,QTR_11P)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(20)                                                          
         DC    C'10042500',X'15',AL1(QTR_8P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 9-10P'                                                  
         DC    BL2'0111110000000000',AL1(QTR_9P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(20)                                                          
*                                                                               
         DC    C'10043000',X'15',AL1(QTR_10P,QTR_1030P)                         
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 11-1130P  '                                             
         DC    BL2'0111110000000000',AL1(QTR_11P,QTR_1130P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(21)                                                          
         DC    C'10043000',X'15',AL1(QTR_9P,QTR_930P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 10-1030P  '                                             
         DC    BL2'0111110000000000',AL1(QTR_10P,QTR_1030P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(21)                                                          
*                                                                               
         DC    C'10045000',X'15',AL1(QTR_1030P,QTR_12A)                         
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 1130P-1A  '                                             
         DC    BL2'0111110000000000',AL1(QTR_1130P,QTR_1A)                      
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(22)                                                          
         DC    C'10045000',X'15',AL1(QTR_930P,QTR_11P)                          
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 1030P-12M '                                             
         DC    BL2'0111110000000000',AL1(QTR_1030P,QTR_12A)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(22)                                                          
*                                                                               
         DC    C'10045300',X'15',AL1(QTR_10P,QTR_1030P)                         
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 11-1130P  '                                             
         DC    BL2'0111110000000000',AL1(QTR_11P,QTR_1130P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(40)                                                          
*                                                                               
         DC    C'10045050',X'15',AL1(QTR_1030P,QTR_12A)                         
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'M-F 1130P-1A  '                                             
         DC    BL2'0111110000000000',AL1(QTR_1130P,QTR_1A)                      
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(41)                                                          
*                                                                               
         DC    C'10020400',X'15',AL1(QTR_4A,QTR_5A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 5-6A      '                                             
         DC    BL2'0111110000000000',AL1(QTR_5A,QTR_6A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(24)                                                          
*                                                                               
         DC    C'10042375',X'15',AL1(QTR_9P,QTR_930P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 10-1030P  '                                             
         DC    BL2'0111110000000000',AL1(QTR_10P,QTR_1030P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(25)                                                          
*                                                                               
         DC    C'10042675',X'15',AL1(QTR_930P,QTR_10P)                          
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 1030-11P  '                                             
         DC    BL2'0111110000000000',AL1(QTR_1030P,QTR_11P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(26)                                                          
*                                                                               
         DC    C'10043500',X'15',AL1(QTR_10P,QTR_11P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-F 11P-12M   '                                             
         DC    BL2'0111110000000000',AL1(QTR_11P,QTR_12A)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(27)                                                          
*                                                                               
                                                                                
         DC    C'50009000',X'17',AL1(QTR_630P,QTR_10P)                          
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 730-11P '                                             
         DC    BL2'0111110110000000',AL1(QTR_730P,QTR_11P)                      
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(25)                                                          
         DC    C'50009000',X'17',AL1(QTR_530P,QTR_9P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 630-10P '                                             
         DC    BL2'0111110110000000',AL1(QTR_630P,QTR_10P)                      
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(25)                                                          
*                                                                               
         DC    C'50003000',X'17',AL1(QTR_7P,QTR_8P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 8-9P    '                                             
         DC    BL2'0111110110000000',AL1(QTR_8P,QTR_9P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(26)                                                          
         DC    C'50003000',X'17',AL1(QTR_6P,QTR_7P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 7-8P    '                                             
         DC    BL2'0111110110000000',AL1(QTR_7P,QTR_8P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(26)                                                          
*                                                                               
         DC    C'50004000',X'17',AL1(QTR_7P,QTR_10P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 8-11P   '                                             
         DC    BL2'0111110110000000',AL1(QTR_8P,QTR_11P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(27)                                                          
         DC    C'50004000',X'17',AL1(QTR_6P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 7-10P   '                                             
         DC    BL2'0111110110000000',AL1(QTR_7P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(27)                                                          
                                                                                
         DC    C'50004900',X'17',AL1(QTR_6P,QTR_9P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'M-SU 7-10P+   '                                             
         DC    BL2'0111110100000000',AL1(QTR_7P,QTR_10P)                        
         DC    BL2'0000000010000000',AL1(QTR_6P,QTR_10P)                        
         DC    AL1(28)                                                          
*                                                                               
         DC    C'50004950',X'17',AL1(QTR_6P,QTR_9P)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-SU 7-10P+1  '                                             
         DC    BL2'0111110100000000',AL1(QTR_7P,QTR_10P)                        
         DC    BL2'0000000010000000',AL1(QTR_7P,QTR_11P)                        
         DC    AL1(29)                                                          
*                                                                               
         DC    C'50005000',X'17',AL1(QTR_7P,QTR_1015P)                          
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 8-11P+  '                                             
         DC    BL2'0111110100000000',AL1(QTR_8P,QTR_11P)                        
         DC    BL2'0000000010000000',AL1(QTR_7P,QTR_11P)                        
         DC    AL1(30)                                                          
         DC    C'50005000',X'17',AL1(QTR_6P,QTR_915P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 7-10P+  '                                             
         DC    BL2'0111110100000000',AL1(QTR_7P,QTR_10P)                        
         DC    BL2'0000000010000000',AL1(QTR_6P,QTR_10P)                        
         DC    AL1(30)                                                          
*                                                                               
         DC    C'50005500',X'17',AL1(QTR_6P,QTR_10P)                            
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 7-11P   '                                             
         DC    BL2'0111110110000000',AL1(QTR_7P,QTR_11P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(42)                                                          
*                                                                               
         DC    C'50006000',X'17',AL1(QTR_8P,QTR_10P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 9-11P   '                                             
         DC    BL2'0111110110000000',AL1(QTR_9P,QTR_11P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(31)                                                          
         DC    C'50006000',X'17',AL1(QTR_7P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 8-10P   '                                             
         DC    BL2'0111110110000000',AL1(QTR_8P,QTR_10P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(31)                                                          
*                                                                               
         DC    C'50007000',X'17',AL1(QTR_10P,QTR_1030P)                         
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 11-1130P'                                             
         DC    BL2'0111110110000000',AL1(QTR_11P,QTR_1130P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(32)                                                          
         DC    C'50007000',X'17',AL1(QTR_9P,QTR_930P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 10-1030P'                                             
         DC    BL2'0111110110000000',AL1(QTR_10P,QTR_1030P)                     
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(32)                                                          
*                                                                               
         DC    C'50009200',X'17',AL1(QTR_1A,QTR_5A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SU-SA 2-6A    '                                             
         DC    BL2'0111110110000000',AL1(QTR_2A,QTR_6A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(33)                                                          
*                                                                               
         DC    C'50002000',X'17',AL1(QTR_8A,QTR_11P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SU-SA 9-12M   '                                             
         DC    BL2'0111110110000000',AL1(QTR_9A,QTR_12A)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(34)                                                          
*                                                                               
         DC    C'50001000',X'17',AL1(QTR_6A,QTR_12A)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SU-SA 7A-1A   '                                             
         DC    BL2'0111110110000000',AL1(QTR_7A,QTR_1A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(35)                                                          
*                                                                               
         DC    C'50000250',X'17',AL1(QTR_5A,QTR_1A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SU-SA 6A-2A   '                                             
         DC    BL2'0111110110000000',AL1(QTR_6A,QTR_2A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(38)                                                          
*                                                                               
         DC    C'50004975',X'17',AL1(QTR_7P,QTR_915P)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_PACI,0,0,0,0)                             
         DC    CL14'SU-SA 8-10P+  '                                             
         DC    BL2'0111110100000000',AL1(QTR_8P,QTR_10P)                        
         DC    BL2'0000000010000000',AL1(QTR_7P,QTR_10P)                        
         DC    AL1(37)                                                          
         DC    C'50004975',X'17',AL1(QTR_6P,QTR_815P)                           
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'SU-SA 6-9P+   '                                             
         DC    BL2'0111110100000000',AL1(QTR_7P,QTR_9P)                         
         DC    BL2'0000000010000000',AL1(QTR_6P,QTR_9P)                         
         DC    AL1(37)                                                          
*                                                                               
         DC    C'50004985',X'17',AL1(QTR_7P,QTR_9P)                             
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'MS8-10 S710   '                                             
         DC    BL2'0111110100000000',AL1(QTR_8P,QTR_10P)                        
         DC    BL2'0000000010000000',AL1(QTR_7P,QTR_10P)                        
         DC    AL1(44)                                                          
*                                                                               
         DC    C'50004990',X'17',AL1(QTR_7P,QTR_10P)                            
         DC    AL2(TIMEZN_CENT,TIMEZN_MOUN)                                     
         DC    AL2(TIMEZN_ALAS,TIMEZN_HAWA,0,0)                                 
         DC    CL14'MS8-11 S711   '                                             
         DC    BL2'0111110100000000',AL1(QTR_8P,QTR_11P)                        
         DC    BL2'0000000010000000',AL1(QTR_7P,QTR_11P)                        
         DC    AL1(45)                                                          
*                                                                               
                                                                                
         DC    C'20021000',X'60',AL1(QTR_6A,QTR_11A)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SAT 7A-12N    '                                             
         DC    BL2'0000000100000000',AL1(QTR_7A,QTR_12P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(37)                                                          
*                                                                               
         DC    C'20022000',X'60',AL1(QTR_11A,QTR_4P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SAT 12N-5P    '                                             
         DC    BL2'0000000100000000',AL1(QTR_12P,QTR_5P)                        
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(38)                                                          
*                                                                               
                                                                                
         DC    C'30021000',X'70',AL1(QTR_12P,QTR_5P)                            
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SUN 1-6P      '                                             
         DC    BL2'0000000010000000',AL1(QTR_1P,QTR_6P)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(39)                                                          
*                                                                               
         DC    C'10020030',X'15',AL1(QTR_1A,QTR_4A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 2A-5A     '                                             
         DC    BL2'0111110000000000',AL1(QTR_2A,QTR_5A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(46)                                                          
*                                                                               
         DC    C'10020210',X'15',AL1(QTR_4A,QTR_430A)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 5-530A    '                                             
         DC    BL2'0111110000000000',AL1(QTR_5A,QTR_530A)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(47)                                                          
*                                                                               
         DC    C'10020435',X'15',AL1(QTR_430A,QTR_5A)                           
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'M-F 530A-6A   '                                             
         DC    BL2'0111110000000000',AL1(QTR_530A,QTR_6A)                       
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(48)                                                          
*                                                                               
         DC    C'50000100',X'17',AL1(QTR_4A,QTR_1A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SUN-SAT 5A-2A '                                             
         DC    BL2'0111110110000000',AL1(QTR_5A,QTR_2A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(49)                                                          
*   ALL DAY ?                                                                   
         DC    C'50009300',X'17',AL1(QTR_4A,QTR_4A)                             
         DC    AL2(TIMEZN_EAST,TIMEZN_CENT,TIMEZN_MOUN)                         
         DC    AL2(TIMEZN_PACI,TIMEZN_ALAS,TIMEZN_HAWA)                         
         DC    CL14'SUN-SAT 5A-5A '                                             
         DC    BL2'0111110110000000',AL1(QTR_5A,QTR_5A)                         
         DC    AL2(0),AL1(0,0)                                                  
         DC    AL1(50)                                                          
         DC    X'00'                                                            
*---------------------------------------------------------------                
QTR_5A    EQU   0                                                               
QTR_530A  EQU   2                                                               
QTR_6A    EQU   4                                                               
QTR_630A  EQU   6                                                               
QTR_7A    EQU   8                                                               
QTR_730A  EQU   10                                                              
QTR_8A    EQU   12                                                              
QTR_830A  EQU   14                                                              
QTR_9A    EQU   16                                                              
QTR_930A  EQU   18                                                              
QTR_10A   EQU   20                                                              
QTR_1030A EQU   22                                                              
QTR_11A   EQU   24                                                              
QTR_1130A EQU   26                                                              
QTR_12P   EQU   28                                                              
QTR_1230P EQU   30                                                              
QTR_1P    EQU   32                                                              
QTR_130P  EQU   34                                                              
QTR_2P    EQU   36                                                              
QTR_230P  EQU   38                                                              
QTR_3P    EQU   40                                                              
QTR_330P  EQU   42                                                              
QTR_4P    EQU   44                                                              
QTR_430P  EQU   46                                                              
QTR_5P    EQU   48                                                              
QTR_530P  EQU   50                                                              
QTR_6P    EQU   52                                                              
QTR_630P  EQU   54                                                              
QTR_7P    EQU   56                                                              
QTR_730P  EQU   58                                                              
QTR_8P    EQU   60                                                              
QTR_815P  EQU   61                                                              
QTR_830P  EQU   62                                                              
QTR_9P    EQU   64                                                              
QTR_915P  EQU   65                                                              
QTR_930P  EQU   66                                                              
QTR_10P   EQU   68                                                              
QTR_1015P EQU   69                                                              
QTR_1030P EQU   70                                                              
QTR_11P   EQU   72                                                              
QTR_1115P EQU   73                                                              
QTR_1130P EQU   74                                                              
QTR_12A   EQU   76                                                              
QTR_1230A EQU   78                                                              
QTR_1A    EQU   80                                                              
QTR_130A  EQU   82                                                              
QTR_2A    EQU   84                                                              
QTR_230A  EQU   86                                                              
QTR_3A    EQU   88                                                              
QTR_330A  EQU   90                                                              
QTR_4A    EQU   92                                                              
QTR_430A  EQU   94                                                              
*=========================== DPTABLE DSECT =======================              
*                                                                               
DPTTABD  DSECT                                                                  
DPTABST  DS    0C                                                               
DPTAPCDE DS    CL8                 DAYPART CODE FROM TAPE                       
DPINTDAY DS    CL1                 DAY CODE FOR INTERIM RECORD                  
DPSTEQH  DS    XL2                 START AND END QHRS FOR INTERIM REC           
DPTIMEZ  DS    CL12                TIME ZONE                                    
DPINTPN  DS    CL14                ALPHA DESCRIPTION                            
DPDAYS1  DS    BL2                 MASK OF DAYSROTATION1                        
DPINTSQ1 DS    CL1                 START TIME QTR HOUR                          
DPINTEQ1 DS    CL1                 END TIME QTR HOUR                            
DPDAYS2  DS    BL2                 MASK OF DAYS ROTATION2                       
DPINTSQ2 DS    CL1                 START TIME QTR HOUR                          
DPINTEQ2 DS    CL1                 END TIME QTR HOUR                            
DPINTDPT DS    CL1                 END TIME QTR HOUR                            
DPTABEN  DS    0C                                                               
DPTABLN  EQU   DPTABEN-DPTABST                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DELMXD1   10/06/14'                                      
         END                                                                    
