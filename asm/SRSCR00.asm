*          DATA SET SRSCR00    AT LEVEL 014 AS OF 10/01/19                      
*PHASE T11F00A                                                                  
*INCLUDE WKSCAN                                                                 
*INCLUDE SCRUMPY                                                                
         TITLE '$SCR - WRKF SCRIPT EXECUTOR '                                   
SCR      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$SCR**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         NI    SSBJFLG2,255-SSBJFSCP STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   ATCB,SSBTKADR                                                    
         MVC   ATIA,SRPAR2                                                      
         MVC   AUTL,SRPAR3                                                      
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         LA    RF,MYPGMLST                                                      
         STCM  RF,7,TASVC          SET DUMMY PGMLST ENTRY ALSO                  
         DROP  R1                                                               
*                                                                               
         L     R1,ATCB                                                          
         MVC   STARTTIM,TCBSTTM-TCBD(R1)                                        
*                                                                               
         L     R1,SRPAR4                                                        
         USING COMFACSD,R1                                                      
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         DROP  R1                                                               
*                                                                               
         L     R1,=V(WKSCAN)                                                    
         A     R1,RELO                                                          
         ST    R1,VWKSCAN                                                       
         L     R1,=V(SCRUMPY)                                                   
         A     R1,RELO                                                          
         ST    R1,VSCRUMPY                                                      
*                                                                               
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
         L     R1,=A(BUFF1-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF1                                                        
         L     R1,=A(BUFF2-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF2                                                        
*                                                                               
         BAS   RE,MAIN             GOTO MAIN PROG                               
         B     XMOD1                                                            
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
XMOD1    L     RD,SAVERD                                                        
         XMOD1                                                                  
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         LA    R2,WORK             SCAN WRKF FOR ACTIVE SCRIPT FILES            
         USING WSBLOCKD,R2                                                      
         XC    WSBLOCK,WSBLOCK                                                  
         MVI   WSFILE,X'FF'        ALL FILES                                    
         MVI   WSSORT,2            SORT BY CREATION                             
         MVI   WSFLAGS,WSFXUSER    IGNORE USER IN SORT                          
         OI    WSFLAGS,WSFREFNO    RETURN REF# IN CIADDR                        
         MVI   WSSTAT,X'80'        FIND ACTIVE FILES                            
         MVI   WSSTATN,X'23'       EXCLUDE TEMP AND PROCESSED FILES             
         MVC   WSSUBPRG,SSBSYSN1   SET 1 CHAR SYSTEM ID                         
         MVI   WSTYPE,C'A'         FIND TYPE A (AUTO SCRIPT)                    
         MVC   WSSMAX,=PL2'200'    SET MAX TO 200                               
         CLC   SSBTPOPT,LATERUN    TEST FOR LATE SCRIPTS                        
         BL    *+8                                                              
         MVI   WSTYPE+1,C'L'       FIND TYPE L (LATE AUTO SCRIPT)               
         GOTO1 VWKSCAN,DMCB,(R2),ABUFF1,AIOAREA,ABUFF2,SRPAR4                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA          POINT TO TABLE                               
         USING WSEDATAD,R2                                                      
MAIN010  CLC   WSEDATA,FFS         TEST FOR END OF TABLE                        
         BE    MAINX                                                            
*                                                                               
         XC    WORK,WORK           LET SCRUMPY DO THE WORK                      
         MVC   WORK+UKFILENO-UKRECD(2),WSECIAD                                  
         OI    WORK+UKFLAG-UKRECD,X'80'                                         
         MVC   WORK(2),WSEUSER                                                  
         GOTO1 VSCRUMPY,DMCB,SRPAR4,ABUFF1,ABUFF2,WORK,0                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SCRUMPY ERROR RETURN                         
*                                                                               
         CLC   16(2,R1),=H'8999'   TEST ABEND OCCURRED                          
         BH    MAIN020             YES - DON'T NEED ANOTHER DUMP                
*                                                                               
         CLC   16(2,R1),=H'1000'   DONT DUMP IF SYS DOWN                        
         BL    *+14                                                             
         CLC   16(2,R1),=H'1255'                                                
         BNH   MAIN020                                                          
*                                                                               
         CLC   16(2,R1),=H'1403'   DONT DUMP IF PROGRAM IS DOWN                 
         BE    MAIN013                                                          
         CLC   16(2,R1),=H'1405'          OR IF PROGRAM NOT AUTHORIZED          
         BNE   MAIN019                                                          
*                                                                               
MAIN013  BAS   RE,SATLSTDY         IS TODAY A SATURDAY AND THE LAST DAY         
         BNE   MAIN019                 OF THE MONTH?                            
*                                                                               
*******  SEND OUT THE EMAIL THAT THIS HAPPENNED                                 
         MVC   EMSG,WARNMSG                                                     
         GOTO1 VDATAMGR,DMCB2,=C'OPMSG',(L'EMSG,EMSG)                           
         B     MAIN020                                                          
*                                                                               
MAIN019  OC    16(2,R1),16(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                SCRIPT ERROR RETURN                          
*                                                                               
MAIN020  LA    R2,L'WSEDATA(R2)    LOOK FOR MORE                                
*                                                                               
         TIME  BIN                 TEST TIME TAKEN SO FAR                       
         S     R0,STARTTIM                                                      
         C     R0,=F'60000'        10 MINUTES                                   
*                                                                               
         BL    MAIN010             LESS THAN 10 MINS SO CONTINUE                
*                                                                               
MAINX    B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* TEST IF AUTOPAY                                                               
*      IF TODAY IS A SATURDAY                                                   
*      IF IT IS THE LAST DAY OF THE MONTH                                       
*                                                                               
* PAY IS DISABLE FOR SOME REASON WHEN THE ABOVE ARE TRUE                        
*                                                                               
* ON EXIT:     (CC)                YES OR NO                                    
*                                                                               
* NOTE: USING DMCB2 SO WE THAT ON THE RETURN R1 DOESN'T HAVE A MESSED           
*       DMCB                                                                    
***********************************************************************         
SATLSTDY NTR1                                                                   
         L     RE,ABUFF1                                                        
         CLC   2(3,RE),=C'APY'     ARE WE AUTOPAY?                              
         BE    *+14                NO, DOESN'T SATISFY ABOVE CONDITIONS         
         CLC   2(3,RE),=C'NAP'     ARE WE AUTOPAY?                              
         BNE   SLDX                NO, DOESN'T SATISFY ABOVE CONDITIONS         
*                                                                               
         GOTO1 VDATCON,DMCB2,(5,0),(0,WORK)   NOTE: USE OF DMCB2                
         GOTO1 VADDAY,DMCB2,WORK,WORK+6,F'1'                                    
         CLC   WORK+6+4(2),=C'01'  TOMORROW IS 1ST DAY OF NEXT MONTH?           
         BNE   SLDX                                                             
         GOTO1 VGETDAY,DMCB2,WORK,WORK+6                                        
         CLI   DMCB2,6                                                          
         BNE   SLDX                                                             
*                                                                               
SLDX     B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS AND LTORG                               *                    
*************************************************************                   
         SPACE 1                                                                
FFS      DC    16X'FF'                                                          
*                                                                               
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
PRTQUE   DC    CL8'PRTQUE'                                                      
ACTI     DC    CL8'ACTI'                                                        
*                                                                               
CTFILE   DC    C'CTFILE '                                                       
FACWRK   DC    C'FACWRK '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
*                                                                               
LATERUN  DS    0PL4                                                             
*&&UK*&& DC    PL4'170000'         TIME FOR LATE SCRIPTS TO RUN                 
*&&US*&& DC    PL4'110000'         TIME FOR LATE SCRIPTS TO RUN                 
*                                                                               
MYPGMLST DC    CL7'SRSCR00',X'14',X'1F',X'00',AL1(000),36X'00'                  
         DC    50X'00'                                                          
*                                                                               
WARNMSG  DS    0CL(L'EMSG)         SOME CONSTANT VALUES FOR E-MAIL              
WARNMSG1 DC    C'AUTONOTE*'                                                     
WARNMSG2 DC    CL45'ABEA,WHOA,AKAT'        WHO TO SEND MESSAGE TO               
WARNMSG3 DC    C':'                                                             
         DC    C'AUTOPAY, TODAY IS LAST DAY OF MONTH && A SATURDAY'             
WARNMSG6 DC    CL(L'EMSG-(*-WARNMSG))' '   SPARE SPACES                         
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DSECT                              *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
RELO     DS    A                                                                
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
*                                                                               
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
STARTTIM DS    F                   TCB START TIME                               
*                                                                               
WORK     DS    XL64                                                             
*                                                                               
VSWITCH  DS    V                                                                
VHELLO   DS    V                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VDATCON  DS    V                                                                
VCALLOVL DS    V                                                                
VLOCKET  DS    V                                                                
VWKSCAN  DS    V                                                                
VSCRUMPY DS    V                                                                
*                                                                               
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ATCB     DS    A                                                                
ASELIST  DS    A                                                                
AIOAREA  DS    A                                                                
ABUFF1   DS    A                                                                
ABUFF2   DS    A                                                                
*                                                                               
EMSG     DS    CL105               E-MAIL MESSAGE TO LOTUS NOTES                
*                                                                               
IOAREA   DS    4096C               IO AREA                                      
BUFF1    DS    14336C              BUFFER 1                                     
BUFF2    DS    14336C              BUFFER 2                                     
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* FADSECTS                                                                      
* DDCOMFACS                                                                     
* DDWKSCAND                                                                     
* DMWRKFK                                                                       
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDWKSCAND                                                      
       ++INCLUDE DMWRKFK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SRSCR00   10/01/19'                                      
         END                                                                    
