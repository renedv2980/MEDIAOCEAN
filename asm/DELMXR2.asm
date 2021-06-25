*          DATA SET DELMXR2    AT LEVEL 005 AS OF 02/28/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXR2A                                                                 
***********************************************************************         
* THIS VERSION OF THE EXIT PARSES TP DATA FOR DAYPART CONVERSIONS               
***********************************************************************         
*INCLUDE REGSAVE                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE NSIWEEK                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE LMTIME                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICEDPT, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
*                                                                     *         
***********************************************************************         
DELMXR2  CSECT                                                                  
*                                                                               
         ENTRY E15                 MUST BE "E15" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E15,RC              RC = PROGRAM BASE REGISTER                   
E15      SAVE  (14,12),,DELMXR2                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         BC    0,MAIN10            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
*                                                                               
         MVC   VADDAY,=V(ADDAY)                                                 
         MVC   VGETDAY,=V(GETDAY)                                               
         MVC   VDATCON,=V(DATCON)                                               
         XC    PREV_WK,PREV_WK                                                  
         MVI   WK#,1                                                            
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
         USING WD_LMDSECT,R3                                                    
*                                                                               
************************************************************                    
* .   CALCULATE THE WEEK NUMBER FOR THE CURRENT RECORD                          
* ..  STORE DDS QHR IN KEY FOR SORTING                                          
* ... CHANGE DATE ON RECORD TO MATCH 24 HOUR SAMPLE DATE                        
************************************************************                    
QH01     DS    0H                                                               
*                                                                               
         LA    RE,WD_KCALDT+11     QUARTER HOUR                                 
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTOR ,DMCB,(1,DUB),QHR                                                
         L     RF,=V(HRTOQH2)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         GOTOR ,DMCB,(0,WD_KCALDT),DUB                                          
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CLI   QHR,X'60'           12A-445A MINUS A DAY                         
         BNL   QH10                                                             
         CLI   QHR,X'4C'                                                        
         BL    QH10                                                             
         L     R4,=F'-1'                                                        
         GOTOR ,DMCB,DUB,DUB,(R4)                                               
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
QH10     GOTOR ,DMCB,DUB,(4,VGETDAY),VADDAY,VDATCON                             
         L     RF,=V(NSIWEEK)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLC   PREV_WK,0(R1)                                                    
         BE    QH40                                                             
*                                                                               
         CLC   WD_KSEQ,PREV_SEQ                                                 
         BNE   QH20                                                             
         CLC   WD_KCODE,PREV_REC                                                
         BNE   QH20                                                             
         CLC   WD_KDISC,PREV_STA                                                
         BNE   QH20                                                             
         CLC   WD_KSAMTY,PREV_SAM                                               
         BE    QH30                                                             
*                                                                               
QH20     XC    PREV_WK,PREV_WK                                                  
         MVI   WK#,0                                                            
         MVC   PREV_SEQ,WD_KSEQ                                                 
         MVC   PREV_REC,WD_KCODE                                                
         MVC   PREV_SAM,WD_KSAMTY                                               
         MVC   PREV_STA,WD_KDISC                                                
*                                                                               
QH30     MVC   PREV_WK,0(R1)                                                    
         ZIC   R1,WK#                                                           
         AHI   R1,1                                                             
         STC   R1,WK#                                                           
*                                                                               
QH40     MVC   WD_KWEEK,WK#                                                     
         MVC   WD_KQHR,QHR                                                      
*                                                                               
         GOTOR ,DMCB,(0,DUB),DUB2                                               
         L     RF,=V(GETDAY)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   WD_KDAY,0(R1)                                                    
         OI    WD_KDAY,X'F0'                                                    
*                                                                               
         GOTOR ,DMCB,(0,DUB),(20,DUB)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   WD_KYEAR,DUB                                                     
         MVC   WD_KDATE(2),DUB+4                                                
         MVC   WD_KDATE+3(2),DUB+6                                              
         B     KEEPREC                                                          
*                                                                               
************************************************************                    
KEEPREC  DS    0H                                                               
         BAS   RE,ACTIVEWK                                                      
         BE    KR10                                                             
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
KR10     DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4:  DELETE RECORD                     
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         BAS   RE,ACTIVEWK                                                      
         BE    AR10                                                             
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
AR10     DS    0H                                                               
         SGR   GR1,GR1                                                          
         LR    R1,R3                                                            
         LGHI  GRF,12              SET RC=12: ADD RECORD                        
         B     GOBACK                                                           
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
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
*                                                                               
SNAPIT   DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                                                               
         SR    RE,RC               DISPLACEMENT TO SNAPIT CALL                  
         STCM  RE,7,THREE          24-BIT ADDRESSING MODE ASSUMED               
         LA    R2,THREE                                                         
         LHI   R0,L'THREE                                                       
         LA    R1,HDR1HEXD         A(OUTPUT AREA)                               
SNAPIT10 LLC   RE,0(R2)                                                         
         SLL   RE,24                                                            
         SRDL  RE,28               ISOLATE HIGH-ORDER NIBBLE                    
         SRL   RF,28               ISOLATE LOW-ORDER NIBBLE                     
         LLC   RE,HEXTAB(RE)                                                    
         STC   RE,0(R1)                                                         
         LLC   RF,HEXTAB(RF)                                                    
         STC   RF,1(R1)                                                         
         LA    R2,1(R2)            BUMP TO NEXT BYTE                            
         LA    R1,2(R1)                                                         
         BCT   R0,SNAPIT10                                                      
*                                                                               
         ZIC   R1,4(RC)            LENGTH OF EXIT NAME                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDR1NAME(0),5(RC)   MOVE EXIT NAME TO OUTPUT AREA                
*                                                                               
*                                     R3 = A(SORT RECORD)                       
         LA    R4,WD_LMRECLQ(R3)   R4 = A(JUST BEYOND SORT RECORD)              
         OPEN  (SNAPDUMP,OUTPUT)   OPEN SNAP DUMP DATASET                       
*                                                                               
*                                  DUMP PSW, REGS, CSECT, AND SORT REC.         
         SNAP  DCB=SNAPDUMP,PDATA=(PSW,REGS,SA,SAH,SUBTASKS,JPA),      +        
               STORAGE=((R3),(R4)),STRHDR=HDR1L                                 
         LTR   RF,RF               WAS SNAP SUCCESSFUL?                         
         BZ    SNAPITX             YES                                          
         ABEND 301                 ABEND IF WE CAN'T GET A SNAP DUMP            
         CLOSE SNAPDUMP                                                         
SNAPITX  L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
ACTIVEWK NTR1                                                                   
         MVC   WD_KAWKS,=C'    '                                                
         LA    R1,WD_KAWKS                                                      
         ZIC   R2,WD_KWEEK                                                      
         AR    R1,R2                                                            
         SHI   R1,1                                                             
         STC   R2,0(R1)                                                         
         OI    0(R1),X'F0'                                                      
         OI    WD_KWEEK,X'F0'                                                   
*                                                                               
         MVC   WD_KDAYMAP,=28C' '                                               
         LA    R1,WD_KDAYMAP                                                    
         LA    R0,X'F0'                                                         
         ZIC   RE,WD_KWEEK                                                      
         XR    RE,R0                                                            
         SHI   RE,1                                                             
         MHI   RE,7                                                             
         AR    R1,RE                                                            
         ZIC   RE,WD_KDAY                                                       
         XR    RE,R0                                                            
         SHI   RE,1                                                             
         AR    R1,RE                                                            
*                                                                               
         LA    R2,DAYTAB           TRANSLATE DAY CODE                           
AW10     CLC   WD_KDAY,0(R2)                                                    
         BE    AW20                                                             
         LA    R2,L'DAYTAB(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   AW10                                                             
****     DC    H'0'                INVALID DAY CODE                             
         B     AW30                SET CONDITION CODE TO NEQ                    
                                                                                
AW20     MVC   0(L'WD_KDAY,R1),1(R2)                                            
         MVC   WD_KDAY,1(R2)                                                    
         CR    RC,RC               SET CONDITION CODE TO EQ                     
         B     AWX                                                              
*                                                                               
AW30     CHI   RC,0                SET CONDITION CODE TO NEQ                    
*                                                                               
AWX      XIT1                                                                   
DAYTAB   DS    0CL2                                                             
         DC    C'1',C'1'                                                        
         DC    C'2',C'2'                                                        
         DC    C'3',C'3'                                                        
         DC    C'4',C'4'                                                        
         DC    C'5',C'5'                                                        
         DC    C'6',C'7'                                                        
         DC    C'7',C'8'                                                        
         DC    X'FF'                                                            
*----------------------------------------------------------------------         
*----------------------------------------------------------------------         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A DFSORT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
         DROP  RC                                                               
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL EXTERNAL SUBROUTINE                     
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
         ORG   DELMXR2+(((*-DELMXR2)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
SAVERE   DS    F                   FOR INTERNAL SUBROUTINES                     
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
THREE    DS    XL3                                                              
HALF     DS    H                                                                
ELCODE   DS    X                                                                
ELEM     DS    CL128                                                            
         SPACE 3                                                                
SNAPDUMP DCB   DDNAME=SNAPDUMP,DSORG=PS,RECFM=VBA,MACRF=(W),LRECL=125, +        
               BLKSIZE=1632                                                     
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF'                                              
*                                                                               
HDR1L    DC    AL1(HDR1LQ)         L'HEADER                                     
HDR1     DC    C'*** YOU DIED AT OFFSET '                                       
HDR1HEXD DS    CL6                 HEX OFFSET WITHIN CSECT                      
         DC    C' IN DFSORT EXIT '                                              
HDR1NAME DC    CL8' '              EXIT NAME                                    
         DC    C'. SORT RECORD FOLLOWS ***'                                     
HDR1LQ   EQU   *-HDR1                                                           
*                                                                               
*                                                                               
WORK     DS    XL17                FOR EDIT                                     
DUB      DS    D                                                                
DUB2     DS    D                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VDATCON  DS    V                                                                
QHR      DS    XL1                                                              
SQHR     DS    XL1                                                              
EQHR     DS    XL1                                                              
SDATE    DS    XL6                                                              
EDATE    DS    XL6                                                              
DUR      DS    XL1                                                              
WK#      DS    CL1                                                              
PREV_SEQ DS    XL2                                                              
PREV_REC DS    XL2                                                              
PREV_STA DS    XL4                                                              
PREV_SAM DS    XL1                                                              
PREV_DAY DS    XL1                                                              
PREV_QHR DS    XL1                                                              
PREV_WK  DS    XL1                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DELMXR2   02/28/14'                                      
         END                                                                    
