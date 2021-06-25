*          DATA SET FAJESMAIL  AT LEVEL 044 AS OF 03/23/16                      
*PHASE JESMAILA                                                                 
         TITLE 'SMTP MAIL FROM FACPAK'                                          
***********************************************************************         
* ROUTINE WILL OPEN THE JES3 READER AND PUT A FORMATTED EMAIL OUT     *         
* EMAIL WILL BE OUTPUT AS A SERIES OF CL80 RECORDS TO JES3            *         
* THE ASSUMPTION IS MADE THAT THIS JOB IS SINGLE THREADED THROUGHOUT  *         
* THE INTERFACE TO IT IS HANDLED BY DDJESMAIL                         *         
***********************************************************************         
         PRINT NOGEN                                                            
         ENTRY WORKAREA                                                         
JESMAIL  START                                                                  
         NBASE WORKL,JESMAIL*,RA,WORK=WORKAREA                                  
         USING WORKD,RC                                                         
         L     R9,0(R1)                                                         
         USING ATCD,R9             R9=A(ATC ENTRY)                              
         L     R8,ATCSYSFC                                                      
         USING SYSFACD,R8          R8=A(SYSFACS)                                
         L     R7,VSSB                                                          
         USING SSBD,R7             R7=A(SSB)                                    
*                                                                               
         MVC   ATCNAME,ANAME                                                    
         MVI   ATCSTAT1,ATCSATCH                                                
         MVI   ATCSTAT2,0                                                       
         MVI   ATCSTATI,0                                                       
         B     WAIT                                                             
*                                                                               
MAIN10   LA    R2,SMTPRPL                                                       
         USING IFGRPL,R2           R2=A(RPL FOR INTERNAL READER)                
         OC    ATCARECB,ATCARECB                                                
         BZ    WAIT                                                             
         BRAS  RE,DYNALLOC                                                      
         OPEN  SMTP                OPEN INTERNAL READER                         
*                                                                               
         L     R3,ATCOTHER         R3=A(CARD DECK - FF MARKS END)               
         CLC   =CL10'*SMTPMAIL*',0(R3)  ONLINE VAR-LENGTH FORMAT?               
         BNE   MAIN40                   NO - FIXED LENGTH ARRAY                 
*                                                                               
*                                  ONLINE VAR-LENGTH FORMAT                     
         AHI   R3,10                                                            
MAIN20   CLC   0(2,R3),=X'FFFF'    END OF BUFFER                                
         JE    MAIN60                                                           
         CLC   0(2,R3),=X'0000'    CAN'T HAVE 0 LENGTH,                         
         JE    MAIN60                    TREAT IT AS "END" FOR NOW              
*                                                                               
         MVC   RECORD,SPACES                                                    
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)          LENGTH OF TEXT                               
*                                                                               
         LHI   RE,L'RECORD-1       USER SMALLER LENGTH (REC OR TEXT)            
         CR    R4,RE                                                            
         BH    *+6                                                              
         LR    RE,R4                                                            
*                                                                               
         LA    R1,RECORD                                                        
         CLI   2(R3),C'.'          LINE STARTED WITH C'.'?                      
         BNE   MAIN30                                                           
         CLC   =X'00014B0004D8E4C9E3',0(R3)    .QUIT STATMENT?                  
         BE    MAIN30                          YES- OK                          
         MVI   RECORD,C'.'         ADD C'.' TO ESCAPE IT.                       
         LA    R1,RECORD+1                                                      
*                                                                               
MAIN30   BCTR  RE,0                -1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R3)                                                    
*                                                                               
         PUT   RPL=(2)                                                          
         AR    R3,R4               BUMP TO NEXT LINE                            
         AHI   R3,2                +2 BYTES LENGTH FIELD                        
         B     MAIN20                                                           
*                                                                               
*                                  FIXED LENGTH ARRAY                           
MAIN40   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    MAIN50                                                           
         MVC   RECORD(80),0(R3)                                                 
         PUT   RPL=(2)                                                          
         AHI   R3,80                                                            
         B     MAIN40                                                           
***********************************************************************         
*THIS FIXED LENGTH EMAIL SHOULD NO LONGER IN USE BECAUSE                        
*DDJESMAIL ALWAYS CONVERT INTO VARIABLE LENGTH FOR THIS, 9/18/2012              
*AUTONOTE IN CASE WE MISS SOMETHING.                                            
***********************************************************************         
MAIN50   WTO   TEXT=ANTXT01H                                                    
         B     MAIN60                                                           
*                                                                               
ANTXT01H DC    AL2(ANTXT01Q)                                                    
ANTXT01  DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'FAJESMAIL,FIXED LENGTH EMAIL!'                                 
ANTXT01Q EQU   *-ANTXT01                                                        
*                                                                               
*                                                                               
MAIN60   ENDREQ RPL=(2)                                                         
         CLOSE SMTP                CLOSE INTERNAL READER                        
*                                                                               
         XC    ATCATCB,ATCATCB     POST COMPLETE                                
         L     R1,ATCARECB                                                      
         POST  (1)                                                              
*                                                                               
WAIT     NI    ATCSTAT1,ATCSATCH   RESET TASK STATUS                            
         LA    R1,ATCECB                                                        
         XC    ATCECB,ATCECB                                                    
         WAIT  1,ECB=(1),LONG=YES                                               
         TM    SSBSTAT1,SSBSEOJ                                                 
         BZ    MAIN10                                                           
*                                                                               
XBASE    NI    ATCSTAT1,ATCSATCH   RESET TASK STATUS                            
         XBASE                                                                  
         DROP  R2,R7,R8,R9                                                      
         EJECT                                                                  
***********************************************************************         
* DYNALLOC SMTP01 WRITER CLASS FOR SYSOUT                             *         
***********************************************************************         
DYNALLOC NTR1  ,                                                                
         LA    R1,RBLK             SET UP REQ BLK POINTER                       
         ST    R1,ARBLK                                                         
         OI    ARBLK,X'80'                                                      
         MVI   RBLKLEN,20          SET UP REQ BLK LEN                           
         MVI   RBLKVERB,1          SET ALLOCATE BY DSN VERB CODE                
         LA    R1,ATXT2                                                         
         ST    R1,RBLKATXT         SET POINTER TO TEXT POINTER LIST             
*                                                                               
         LA    R1,TXTDD            SET UP POINTER TO DDNAME TEXT                
         ST    R1,ATXTDD                                                        
         MVC   0(2,R1),=X'0001'    SET DDNAME TEXT CODE                         
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=AL2(4)     SET LEN IN DDNAME TEXT HEADER                
         MVC   6(8,R1),=CL8'SMTP  '                                             
*                                                                               
         LA    R1,TXTSYSO          SET UP POINTER TO SYSOUT TEXT                
         ST    R1,ATXTSYSO                                                      
         MVC   0(2,R1),=X'0018'    SET SYSOUT TEXT CODE                         
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0001'                                                 
         MVI   6(R1),C'T'                                                       
*                                                                               
         LA    R1,TXTWRT           SET UP POINTER TO WRITER NAME                
         ST    R1,ATXTWRT                                                       
         MVC   0(2,R1),=X'0019'    SET WRITER NAME                              
         MVC   2(2,R1),=X'0001'                                                 
         MVC   4(2,R1),=X'0008'                                                 
*&&UK*&& MVC   6(8,R1),=CL8'TCPSMTP'                                            
*&&US                                                                           
         MVC   6(8,R1),=CL8'SMTP01'                                             
         L     RE,X'10'(,R0)       A(CVT)                                       
         USING CVT,RE                                                           
         L     RE,CVTSMCA          A(SMCA)                                      
         USING SMCABASE,RE                                                      
         CLC   =C'SYA',SMCASID     (TSO7 AKA SY7)                               
         BNE   *+10                                                             
         MVC   6(8,R1),=CL8'SMTP07'                                             
*&&                                                                             
         LA    R1,TXTCLSE          FREE=CLOSE                                   
         ST    R1,ATXTCLSE                                                      
         MVC   0(2,R1),=X'001C'                                                 
         MVC   2(2,R1),=X'0000'                                                 
         MVC   4(2,R1),=X'0000'                                                 
*                                                                               
         OI    ATXTSX,X'80'        SET END OF LIST                              
         LA    R1,ARBLK                                                         
         DYNALLOC                                                               
         LTR   RF,RF               TEST FOR ERRORS                              
         BZ    DYNX                NO                                           
         L     R2,RBLKERR                                                       
         DCHO                                                                   
*                                                                               
DYNX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
ANAME    DC    CL8'JES3MAIL'                                                    
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL16'SMTPACB=SMTPACB='                                           
SMTP     ACB   AM=VSAM,MACRF=(ADR,SEQ,OUT),DDNAME=SMTP                          
*                                                                               
         DS    0D                                                               
         DC    CL16'SMTPRPL=SMTPRPL='                                           
SMTPRPL  RPL   ACB=SMTP,OPTCD=(ADR,SEQ,SYN,NUP),RECLEN=161,            *        
               AREA=RECORD,AREALEN=161                                          
*                                                                               
         DS    0D                                                               
         DC    CL16'RECORD=>RECORD=>'                                           
RECORD   DC    CL161' '                                                         
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DS    250D                                                             
         DC    CL16'WORKAREXWORKAREX'                                           
*                                                                               
SPACES   DC    CL(L'RECORD)' '                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
SAVERD   DS    A                                                                
FULL     DS    F                                                                
*                                                                               
ARBLK    DS    A                   REQUEST BLOCK POINTER                        
*                                                                               
RBLK     DS    0XL20               REQUEST BLOCK                                
RBLKLEN  DS    XL1                                                              
RBLKVERB DS    XL1                                                              
         DS    XL2                                                              
RBLKERR  DS    XL2                                                              
RBLKINFO DS    XL2                                                              
RBLKATXT DS    A                                                                
         DS    XL4                                                              
RBLKFLAG DS    XL4                                                              
*                                                                               
ATXT2    DS    0A                  TEXT BLOCK POINTERS FOR DISP=SHR             
ATXTDD   DS    A                                                                
ATXTSYSO DS    A                                                                
ATXTWRT  DS    A                                                                
ATXTDEST DS    A                                                                
ATXTCLSE DS    A                                                                
         ORG   *-4                                                              
ATXTSX   DS    A                   LAST TEXT BLOCK POINTER FOR SYSOUT           
         ORG                                                                    
*                                                                               
TXTDD    DS    XL6,CL8             TEXT BLOCK DDNAME        0001                
TXTSYSO  DS    XL6,XL1             TEXT BLOCK SYSOUT=T      0018                
TXTWRT   DS    XL6,CL8             TEXT BLOCK SYSOUT WRITER 0042                
TXTDEST  DS    XL6,CL8             TEXT BLOCK SYSOUT DEST   0058                
TXTCLSE  DS    XL6                 TEXT BLOCK FREE=CLOSE    0042                
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         CVT   DSECT=YES                                                        
*                                                                               
         IEESMCA                                                                
*                                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
*                                                                               
         IFGRPL                                                                 
         IEFZB4D0                                                               
         IEFZB4D2                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044FAJESMAIL 03/23/16'                                      
         END                                                                    
